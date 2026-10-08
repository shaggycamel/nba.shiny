# NUC Deployment Runbook

Instructions for an agent operating on the **nuc** host to build and deploy the
NBA Shiny league containers and entry point to Hugging Face Spaces.

Read this whole file before running anything. Prefer `DRY_RUN=1` first.

---

## 1. Context

- Repo: `nba.shiny` (monorepo; the league dashboard is the `nba.shiny.league/`
  package, which holds `data-raw/` and is built from its own directory).
- Sibling packages: `nba.shiny.core/` (shared theme/db/token/password) and
  `nba.shiny.entry/` (the **entry point**: auth → league picker → signed iframe).
- Containers are built from Docker images and deployed as **one HF Space per league**
  plus a single **entry Space**. League containers bake their data at build time;
  the entry point reads customer/league data from CockroachDB at runtime.
- Deploy logic lives in `cron.sh` (leagues), `build_entry.sh` (entry),
  `build_all.sh` (both), and `deploy/` (provider adapter; currently Hugging Face).

## 2. Prerequisites (verify before starting)

- Docker is running and can reach Docker Hub.
- **No host R/renv is needed.** All R steps (data generation, `R CMD build`, league
  listing) run inside the `nba.shiny_base:latest` image, which carries the restored
  library. (The host must have Docker; R is used via `docker run`.)
- Credentials INI exists: `~/.config/sports-hub-credentials.ini` with a
  `cockroach-read` section (and `postgres` for local). Generation defaults to
  `NBA_DB_SECTION=cockroach-read`. (No `psql` needed — leagues are listed via R in
  the base image.)
- A `./.profile` on the nuc exports the tokens (the scripts source it
  automatically when run non-interactively; for a manual run, source it first):
  - `DOCKERHUB_TOKEN` — Docker Hub push token (required).
  - `HUGGINGFACE_TOKEN` — HF token with **write** access to the Spaces (required).
  - `DOCKERHUB_USER` (default `shaggycamel`), `NBA_HF_OWNER` (default `shaggycamel`),
    `NBA_SEASON` (default `2025-26`).

  Create it once (git-ignored, never commit):
  ```bash
  cat > ~/github/nba.shiny/.profile <<'EOF'
  export DOCKERHUB_USER=shaggycamel
  export DOCKERHUB_TOKEN=...
  export HUGGINGFACE_TOKEN=...
  EOF
  chmod 600 ~/github/nba.shiny/.profile
  ```

Preflight:

```bash
cd ~/github/nba.shiny
git pull                      # MUST be up to date; see §3
docker info >/dev/null && echo docker-ok
test -f ~/.config/sports-hub-credentials.ini && echo ini-ok
set -a; source ./.profile; set +a
for v in DOCKERHUB_TOKEN HUGGINGFACE_TOKEN; do
  [ -n "${!v:-}" ] && echo "$v set" || echo "$v MISSING"
done
# League listing runs inside the base image, so do it after the base is built
# (or just rely on the dry run in §5, which exercises the same code path).
```

## 3. Code must be pushed first

The build uses the checked-out code. If the developer's changes are not yet on the
remote, `git pull` will not include them — do not proceed on stale code. Confirm
`git log --oneline -1` matches the commit the developer reported.

## 4. Values

Owner defaults to `shaggycamel`; Space prefix is `nba-shiny`; image prefix is
`nba.shiny-`.

| Purpose | HF Space | URL |
|--------|----------|-----|
| Entry point | `shaggycamel/nba-shiny-entry` | `https://shaggycamel-nba-shiny-entry.hf.space` |
| League ESPN 95537 | `shaggycamel/nba-shiny-espn-95537` | `https://shaggycamel-nba-shiny-espn-95537.hf.space` |
| League ESPN 1382487116 | `shaggycamel/nba-shiny-espn-1382487116` | `…-espn-1382487116.hf.space` |
| League ESPN 1966813226 | `shaggycamel/nba-shiny-espn-1966813226` | `…-espn-1966813226.hf.space` |
| League ESPN 24608 | `shaggycamel/nba-shiny-espn-24608` | `…-espn-24608.hf.space` |

Docker images: `shaggycamel/nba.shiny-espn-<league_id>:latest`; entry
`shaggycamel/nba.shiny.entry:latest`. Base image: `nba.shiny_base:latest`.

`NBA_HANDOFF_SECRET` is an arbitrary shared HMAC key (no default). Generate once
and use the **same** value on the entry Space and every league Space:

```bash
openssl rand -hex 32
```

## 5. Build + deploy

Run from the repo root. Source the tokens first, and always dry-run first.

```bash
set -a; source ./.profile; set +a

# 5.0 Dry run (no docker, no push, no deploy)
DRY_RUN=1 PROVISION=1 bash build_all.sh

# 5.1 Real run. PROVISION=1 creates any missing HF Spaces.
PROVISION=1 bash build_all.sh
```

`build_all.sh` does, in order:
1. Base image `nba.shiny_base:latest` (slow; skipped if it exists unless `REBUILD_BASE=1`)
   and verifies `nba.shiny.core` loads in it.
2. `cron.sh` — generates the shared NBA base once, then per league generates data,
   builds the tarball, builds/pushes `shaggycamel/nba.shiny-<slug>:latest`, and
   deploys. Failures are reported per league; the loop continues.
3. `build_entry.sh` — builds/pushes/deploys `shaggycamel/nba.shiny.entry:latest`.

Individual stages (if you need to isolate a failure):

```bash
REBUILD_BASE=1 bash build_all.sh            # force base rebuild
SKIP_ENTRY=1 bash build_all.sh              # leagues only
PROVISION=1 bash build_entry.sh             # entry only
```

## 6. Configure the Spaces (after they exist)

Set runtime config on **all** Spaces via the HF API (or the Space settings UI).
`NBA_HANDOFF_SECRET` must be identical everywhere.

```bash
HF="$HUGGINGFACE_TOKEN"
SECRET="<the sha256 hex you generated>"

# per Space repo id (entry + each league)
for REPO in \
  shaggycamel/nba-shiny-entry \
  shaggycamel/nba-shiny-espn-95537 \
  shaggycamel/nba-shiny-espn-1382487116 \
  shaggycamel/nba-shiny-espn-1966813226 \
  shaggycamel/nba-shiny-espn-24608
do
  curl -sf -X POST "https://huggingface.co/api/spaces/$REPO/secrets" \
    -H "Authorization: Bearer $HF" -H "Content-Type: application/json" \
    -d "{\"key\":\"NBA_HANDOFF_SECRET\",\"value\":\"$SECRET\",\"description\":\"handoff HMAC key\"}"

  curl -sf -X POST "https://huggingface.co/api/spaces/$REPO/variables" \
    -H "Authorization: Bearer $HF" -H "Content-Type: application/json" \
    -d '{"key":"NBA_DB_SECTION","value":"cockroach-read"}'

  curl -sf -X POST "https://huggingface.co/api/spaces/$REPO/variables" \
    -H "Authorization: Bearer $HF" -H "Content-Type: application/json" \
    -d '{"key":"NBA_SEASON","value":"2025-26"}'
done
```

Additionally, on **league** Spaces only, set strict mode so a container refuses to
run without a valid entry token:

```bash
curl -sf -X POST "https://huggingface.co/api/spaces/shaggycamel/nba-shiny-espn-95537/variables" \
  -H "Authorization: Bearer $HF" -H "Content-Type: application/json" \
  -d '{"key":"NBA_REQUIRE_HANDOFF","value":"1"}'
# repeat for the other league Spaces
```

Secrets/variables require a Space restart to take effect — the deploy step in §5
already restarts, so set these **then re-run** the relevant stage (§5.1) or restart
the Space.

## 7. Set customer passwords

Passwords are stored hashed in `fty.customer.password_hash`. Set one per customer:

```bash
cd ~/github/nba.shiny
NBA_DB_SECTION=cockroach-read NBA_SET_PASSWORD='<password>' \
  Rscript scripts/set_customer_password.R <customer_email>
```

`scripts/` is git-ignored; if it is missing, it was not synced (it is intentionally
not versioned).

## 8. Verify

```bash
# entry point serves a login page
curl -fsS -o /dev/null -w '%{http_code}\n' https://shaggycamel-nba-shiny-entry.hf.space/

# each league Space is up (will show the "Sign in required" modal when
# NBA_REQUIRE_HANDOFF=1 and no token is present — that is correct)
curl -fsS -o /dev/null -w '%{http_code}\n' https://shaggycamel-nba-shiny-espn-95537.hf.space/
```

Then, in a browser: open the entry URL → log in → pick a league → the league
dashboard should load inside the iframe, already scoped to that customer's manager.

## 9. Troubleshooting

- **Base build fails / `nba.shiny.core` missing**: check `renv.lock` includes
  `openssl`, `digest`, `ini`, `bslib`, `DBI`, `RPostgres`. Rebuild with
  `REBUILD_BASE=1`.
- **"no leagues found"**: `fty.league` has no rows for `NBA_SEASON`, or the
  credentials INI section is wrong. Debug inside the base image:
  `docker run --rm -e NBA_SEASON=2025-26 -v "$HOME/.config:/root/.config:ro" -v "$PWD:/work" -w /work/nba.shiny.league nba.shiny_base:latest Rscript ./data-raw/_list_leagues.R`
- **HF restart returns non-2xx**: the Space does not exist (run with `PROVISION=1`)
  or `HUGGINGFACE_TOKEN` lacks write access.
- **401/blank league dashboard**: `NBA_HANDOFF_SECRET` differs between entry and the
  league Space, or is unset. They must match.
- **Manager not auto-selected**: `fty.customer_league.competitor_id` is NULL for that
  row; the entry point falls back to a manager picker.
- **Background/stop needed**: do not `docker system prune` on the nuc — the base image
  is expensive to rebuild.

## 10. Guardrails

- Never commit or echo secrets; do not add them to the repo or this file.
- Always `DRY_RUN=1` first when changing scripts.
- `PROVISION=1` is idempotent (creates missing Spaces, commits a Dockerfile only for
  new Spaces); it will not delete anything.
- Do not force-push or amend commits.
