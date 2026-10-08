# NBA Shiny: Multi-Customer Architecture

## Overview

Building a multi-customer NBA Fantasy dashboard in Shiny with shared, league-scoped
containers behind a single authenticated entry point. Goal: avoid duplicating league
containers across customers while maintaining customer isolation.

## Problem Statement

- Multiple customers need access to the NBA fantasy dashboard
- Each customer may access different combinations of leagues
- One customer is one manager (competitor) within a league
- A single league container should be shared across customers (not duplicated)
- Customers need logical isolation from each other

## Architecture at a Glance

```
Customer browser
  │
  ▼
nba.shiny-entrypoint           (single smart entry point: auth + league picker + iframe shell)
  │  reads Cockroach live for credentials, customer→league mapping, league registry
  │  embeds a signed iframe
  ▼
nba.shiny-<platform>-<league_id>   (shared league container, one per league, data baked in)
```

Key principle: **one entry point** (auth + routing), **many shared league containers**
(one per league, customer-agnostic), connected by signed iframes.

## Entry Point Flow

1. **Authenticate** — adapted `mod_modal_login` step 1: customer credentials checked
   against Cockroach. If correct, establish the session.
2. **Select workspace** — step 2 modal lists only the customer's leagues (from
   `fty.customer_league`). The manager (`competitor_id`, `platform`) is derived from
   the mapping, not chosen separately. Auto-enter when the customer has a single league.
3. **Embed** — entry point builds a signed league-container URL and renders it in an iframe.

### Change League

- A "Change league" button in the entry-point header reopens step 2 (no re-auth) and
  updates the iframe `src` in place.
- League containers run in **embedded mode**: no login modal, no switch button.

### Customer Mapping Assumption

One customer = one manager (competitor) per league. The schema is still keyed on
`competitor_id`, so supporting co-managers/multi-manager later is a UI change only,
not a migration.

## Manager Handoff (iframe URL Contract)

The chosen manager is passed to the league container through the iframe URL and the
container seeds itself from it (no modal):

```
https://<container_url>/?platform=<p>&league_id=<id>&competitor_id=<cid>&exp=<ts>&sig=<hmac>
sig = HMAC(customer_id, platform, league_id, competitor_id, exp, secret)
```

- League container parses `url_search`, validates `sig` and expiry.
- On success it sets `rv_carry_thru` directly: ids from the URL, names from
  `ls_fty_lookup` (baked), `cur_matchup_period` from `dfs_fty_schedule` (baked) —
  the same assignment logic as `mod_modal_login.R:30-48`, just fed from the URL.
- `fty_parameters_met <- TRUE`, render immediately, skip the modal.
- Invalid/expired/missing signature → refuse to render.

Because `platform`, `league_id`, and `competitor_id` all originate from the server-side
mapping, the token binds the customer to their own manager. The signature is
**load-bearing** and is the only barrier preventing direct access outside the entry point.

## Data Update & Storage

### Constraints

- Served data is a **build-time snapshot** baked into images.
- League containers have **no runtime DB access**; only the ETL/build host reaches Cockroach.
- Containers are **shared** across customers → served fantasy data is **league-scoped and
  customer-agnostic**.
- The **entry point has read-only runtime access** to Cockroach for control data
  (credentials, customer→league mapping, league registry).
- League identity is `(platform, league_id)`; `league_id` alone is not unique across platforms.

### Data Scopes

| Scope | Contents | Storage | Built |
|-------|----------|---------|-------|
| Shared NBA base | `df_nba_player_box_score`, `df_nba_schedule`, `df_nba_roster`, `ls_nba_teams`, `dfs_rolling_stats`, `ls_player_game_log`, `ls_injuries`, `cur_date` | `nba.shiny.data` package | once per release |
| League fantasy | `df_fty_base`, fty schedule/roster/box-scores/free-agents/activity, `dfs_league_overview`, `dfs_h2h_past/future`, `dfs_player_comparison`, `dfs_fty_nba_mup_weeks`, `ls_lo_lg_cats`, `ls_fty_lookup` | per-league image | once per league |
| Customer control | credentials, customer→league mapping, league registry/URLs | CockroachDB (`fty.*`) | live at runtime |

### Image Layering

Daily data refresh must **not** rebuild the slow dependency layer. Split them:

```
rocker-verse / dev deps
  └── nba.shiny_base              # OS libs + renv deps        → rebuild on dependency change only (slow)
        ├── nba.shiny_data        # + shared NBA data package  → rebuild daily (tiny layer)
        │     └── nba.shiny-<platform>-<league_id>   # + league fty rda → rebuild daily (tiny layer)
        └── nba.shiny-entrypoint  # iframe shell + auth, no data → rebuild on code change
```

| Image | Base | Adds | Rebuild cadence |
|-------|------|------|-----------------|
| `nba.shiny_base` | rocker-verse | OS libs, renv deps | on dependency change only |
| `nba.shiny_data` | `nba.shiny_base` | shared NBA data package | daily |
| `nba.shiny-<platform>-<league_id>` | `nba.shiny_data` | one league's fty rda | daily |
| `nba.shiny-entrypoint` | `nba.shiny_base` | iframe shell + auth (no data) | on code change |

Caveat: this stays cheap only if the data packages add **no new R/OS dependencies**; any
dependency they need must move up into `nba.shiny_base`, or the layer cache is invalidated.

The shared-NBA-data layer is stored once in the registry and reused by every league
image (a "shared Docker layer"). The per-league run only adds a small layer with that
league's fty data.

### Package Structure

Evidence: extraction of shared UI/auth code is smaller than it looks, because the
entry-point modal queries Cockroach while the league `mod_modal_login` reads baked data.
So use separate packages (chosen over a single package with an `entry`/`league` mode flag,
which forces two tarballs from one source and a LazyData trap):

- **`nba.shiny.core`** — shared theme/CSS, login-modal UI widgets, helpers.
- **`nba.shiny`** — league dashboard (imports `nba.shiny.core` + data). Current package, trimmed.
- **`nba.shiny.entry`** — entry shell (imports `nba.shiny.core`, queries Cockroach; no baked data).

### Generation Pipeline

Replace the per-customer flow (`data-raw/_generate_all.R` parameterised by `CUSTOMER_ID`)
with league-scoped generation:

1. **`_generate_base.R`** — the NBA/shared half of `02_nba_base.R`; run once per release.
2. **`_generate_league.R`** — `03_fty_base.R` + `data_h2h.R`, `data_league_overview.R`,
   `data_player_comparison.R`, `data_schedule_table.R`; parameterised by
   `(platform, league_id)`. **Remove every `filter(customer_id == cus_id)` clause** — the
   source views already carry `league_id`, so there is no per-customer duplication.
3. **`cron.sh` rewrite** — loop active **leagues** (from `fty.league`), not customers:
   - ETL base once → build/push `nba.shiny_data`.
   - Per league: generate → `R CMD build` → `docker build/push` → restart the HF Space.
   - Entry-point image rebuilt only on app/code change.

### DB Layer

Replace the hardcoded `db_con()` (`R/utils_database.R`) with a `db_connect(env)` that reads
credentials from env/secret, is read-only, and targets the schema assumed by the untracked
`tests/testthat/test-data-pipeline.R`: `util.player_id_map_vw`, `nba.team_roster_vw`,
`nba.player_box_score_vw`, `fty.roster_schedule_vw`. Formalise the player-id-map contract
and keep that test.

### Schema (Cockroach)

| Object | Key columns | Notes |
|--------|-------------|-------|
| `fty.customer` | `customer_id`, `slug`, `is_active` | existing |
| `fty.customer_league` | `customer_id`, `platform`, `league_id`, `competitor_id` | PK `(customer_id, platform, league_id)`; one manager per league per customer |
| `fty.league` | `platform`, `league_id`, `slug`, `container_url`, `is_active` | PK `(platform, league_id)`; `container_url` is a controlled domain, not a derived host |
| `util.player_id_map_vw` | player-id bridge | formalise; asserted by tests |

### Refresh

Nightly (or per-release) cron produces a new build-time snapshot. Freshness = build time;
`cur_date` is frozen at generation. Sequence: regenerate data → rebuild `nba.shiny_data`
→ rebuild affected league images → restart Spaces. The expensive `nba.shiny_base` is
untouched unless dependencies change. The entry-point image needs no data step.

## Isolation Guarantees

| Layer | Mechanism |
|-------|-----------|
| **UI** | Entry point lists only the authenticated customer's leagues |
| **Session** | Each login is a separate entry-point Shiny session |
| **Handoff** | Signed iframe token binds customer → their manager; league container rejects unsigned/expired |
| **Container** | League containers are shared and customer-agnostic; each serves exactly one `(platform, league_id)` |
| **Data** | League containers bake all competitors' data (for comparison); the mapping decides only the spotlight manager |

Cross-container access is prevented by the signature: a customer can only ever load the
spotlight *as their own manager*. Other managers appear as comparison data, which is
league data, not an assumed session.

## Embedding (Hugging Face Spaces)

- League containers must be **public or protected** (protected = private source, public embed URL).
- `disable_embedding` must remain `false` (the default) in each Space.
- Embed the **`.hf.space`** URL, never the `huggingface.co/spaces/...` page (which sets `X-Frame-Options: deny`).
- Token validation is **app-level** — HF does not enforce it; the Shiny app must refuse unsigned access.

## Provider Portability

Hugging Face is the primary host. The design keeps a future RunPod (or other) migration
cheap, but is not literally provider-agnostic.

**Carries over:** data images, layering, registry, signed-URL handoff, app-level token
validation, and the entry-point/league split.

**HF-specific:**

- URL derivation (`*.hf.space`); RunPod uses pod-id proxy URLs that change on recreate/migration.
- Public/protected visibility + `disable_embedding`.
- Restart API in `cron.sh`.

**To keep the swap cheap:**

- **Deploy adapter interface** — `provision(league) → container_url`, `redeploy(league, image)`,
  `teardown(league)`. One HF adapter now, a RunPod adapter later; `cron.sh` calls the
  interface, not HF directly.
- **Controlled front door** — `fty.league.container_url` is a domain we control (custom
  domain / reverse proxy), so provider URL churn never reaches the app.
- **RunPod risk (do not assume drop-in):** its HTTP proxy path is
  `User → Cloudflare → RunPod LB → Pod`; Cloudflare is unreliable for WebSockets, which
  Shiny depends on. TCP exposure gives a raw IP:random-port with no HTTPS, breaking HTTPS
  iframes. Validate Shiny over RunPod before migrating; likely needs TCP + own TLS reverse proxy.
- **RunPod has no auth/visibility layer** (the proxy is public), so token enforcement is
  mandatory; and no free sleep, so all containers are always-on billed.

Alternative web-app PaaS options if outgrowing HF (Fly.io, Render, Railway, DO App
Platform) are a more natural next step than RunPod, which is optimised for bursty GPU compute.

## Alternatives Considered

### Option A: Consolidated Module (Not Chosen)
- Combine all leagues into single app with module switching
- **Downside:** All leagues in memory, single point of failure

### Option B: Customer-Specific Entry Points (Not Chosen)
- Each customer gets its own container with pre-baked leagues
- **Downside:** N deployments, more ops overhead, customer isolation overhead

**Chosen: shared league containers behind a single smart entry point** ✓
- Minimal deployment footprint
- Logical customer isolation via auth + signed handoff
- Shared league containers reduce resource overhead and duplication

## Next Steps

1. Extract `nba.shiny.core`; split `nba.shiny` (league) and create `nba.shiny.entry` (iframe shell).
2. Adapt `mod_modal_login`: step 2 becomes league-only (manager from mapping); add URL-seeded
   startup path for the league container.
3. Add `db_connect(env)` and formalise `util.player_id_map_vw`; keep `test-data-pipeline.R`.
4. Add `fty.customer_league` and `fty.league`; create the signed-URL helper + validation.
5. Split `data-raw/` into `_generate_base.R` and `_generate_league.R`; remove `customer_id` filters.
6. Add `nba.shiny_data` image layer; rewrite `cron.sh` to loop leagues and use the deploy adapter.
7. Prove the flow end-to-end on HF: login → select league → signed iframe → manager spotlight.
8. Test isolation: unsigned/expired iframe refused; cross-league access blocked.

## Notes

- League containers don't need to know about customers — isolation happens at the entry point.
- iframes provide clear resource boundaries between league contexts.
- Control data lives in Cockroach and is read live; only league/NBA data is baked.
- The signature is the security boundary, not the obscurity of container URLs.
