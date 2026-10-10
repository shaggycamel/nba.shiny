<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{scs.nba.fty.league_dash}`

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An NBA fantasy dashboard built with Shiny and [{golem}](https://thinkr-open.github.io/golem/),
deployed to multiple customers through a single authenticated entry point and
shared, league-scoped containers. This repository is a small monorepo of three R
packages plus the container/deploy pipeline.

## Repository layout

- `dash_league/` — the **league dashboard** package (the Shiny app).
- `dash_entry/` — the **entry point**: auth → league picker → signed iframe.
- `dash_core/` — shared theme, database access, password hashing and
  handoff tokens.
- `docker/`, `deploy/` — container build and deploy pipeline (`deploy/cron.sh` is
  the single deploy entry point).
- `e2e/` — browser end-to-end test (entry login → signed iframe → league).
- `docs/` — deployment runbook.

## Run locally

```r
renv::restore()
devtools::load_all("dash_league")
league::run_app()
```

## Deploy

Builds and deploys run from the repo root. See
[docs/NUC_DEPLOY.md](docs/NUC_DEPLOY.md) for the full runbook.

## Tests

```r
devtools::test("dash_league")
devtools::test("dash_core")
devtools::test("dash_entry")
```

Browser end-to-end (entry login → signed iframe → league dashboard) lives in
[`e2e/`](e2e/README.md).
