# AGENTS.md

Monorepo for the NBA fantasy league dashboard: three R packages plus the
container/deploy pipeline. The app is a `{golem}` Shiny app.

## Layout

- `dash_league/` — league dashboard Shiny app. R package name: `league`. Holds `data-raw/`.
- `dash_entry/` — auth entry point (login → league picker → signed iframe). Package: `entry`.
- `dash_core/` — shared theme/db/token/password helpers. Package: `core`.
- `docker/`, `deploy/` — container build/deploy pipeline.
- `e2e/` — Playwright browser test (entry login → signed iframe → league).
- `docs/NUC_DEPLOY.md` — deployment runbook.

IMPORTANT: folders are prefixed `dash_`, but R package names stay
`core`/`entry`/`league`. Use `library(league)`, `Imports: core`, `run_app()`, etc.
`R CMD build` tarballs are named by the Package field (`league_*.tar.gz`), not the
folder.

## Develop

```r
renv::restore()
devtools::load_all("dash_league")
league::run_app()
```

## Test

Unit tests (testthat), run per package:

```r
devtools::test("dash_league")
devtools::test("dash_core")
devtools::test("dash_entry")
```

Browser end-to-end (Playwright) needs live Spaces and credentials, so it is not
part of the unit suite. See `e2e/README.md` for env vars and gotchas:

```bash
cd e2e
npm install
E2E_EMAIL='<test-customer-email>' E2E_PASSWORD='<password>' npm test
```

## Format

`air format` (config in `air.toml`: 120-col, 2-space).

## Deploy

`deploy/cron.sh` is the single deploy entry point (base image + one image per
league; `BUILD_ENTRY=1` also rebuilds the entry Space). `deploy/build_entry.sh`
does entry only.
Always `DRY_RUN=1` first. Docker build context is the repo root; `.dockerignore`
excludes `dash_league/{data,data-raw,dev,tests,man}`, `docs/`, and `e2e/`.
Full runbook: `docs/NUC_DEPLOY.md` (load on demand — do not preload).

## Conventions / gotchas

- Never commit secrets. DB creds live in `~/.config/scs_hub_credentials.ini`;
  tokens in `./.profile` (git-ignored).
- Runtime config via env: `NBA_DB_SECTION`, `NBA_SEASON`, `NBA_HANDOFF_SECRET`,
  `NBA_REQUIRE_HANDOFF`.
- Handoff security: `entry` signs an HMAC token; league containers verify it.
  `NBA_HANDOFF_SECRET` must be identical on the entry and every league Space.
- Don't add code comments unless asked.
