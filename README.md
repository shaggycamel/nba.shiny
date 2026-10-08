<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{nba.shiny}`

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An NBA fantasy dashboard built with Shiny and [{golem}](https://thinkr-open.github.io/golem/).
It is deployed to multiple customers through a single authenticated entry point
and shared, league-scoped containers.

## Repository layout

- `R/`, `inst/`, `data-raw/` — the **league dashboard** package (`nba.shiny`).
- `packages/nba.shiny.core/` — shared theme, database access, password hashing
  and handoff tokens.
- `packages/nba.shiny.entry/` — the **entry point**: auth → league picker →
  signed iframe.
- `docker/`, `deploy/`, `cron.sh`, `build_*.sh` — container build and deploy
  pipeline.
- `docs/` — architecture and deployment runbook.

## Run locally

```r
renv::restore()
nba.shiny::run_app()
```

## Deploy

Builds and deploys run from the repo root. See
[docs/NUC_DEPLOY.md](docs/NUC_DEPLOY.md) for the full runbook and
[docs/nba-shiny-architecture.md](docs/nba-shiny-architecture.md) for the design.

## Tests

```r
devtools::test()
```
