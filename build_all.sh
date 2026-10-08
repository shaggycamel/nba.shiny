#!/bin/bash

# Remember to chmod +x build_all.sh on nuc after pulling latest file
#
# One-shot build + deploy: base image, then every league container, then the
# entry point. A thin wrapper around cron.sh + build_entry.sh.
#
# Config (env vars):
#   DOCKERHUB_TOKEN, HUGGINGFACE_TOKEN   (required unless DRY_RUN=1)
#   DOCKERHUB_USER (default shaggycamel)
#   NBA_SEASON (default 2025-26), NBA_DB_SECTION (default cockroach-read)
#   DRY_RUN=1        run through the motions without building/pushing/deploying
#   REBUILD_BASE=1   rebuild the (slow) base image even if it exists
#   PROVISION=1      create HF Spaces if missing
#   SKIP_ENTRY=1     skip the entry point

if [ ! -t 1 ] && [ -f ./.profile ]; then
  # shellcheck source=/dev/null
  source ./.profile
fi

set -uo pipefail

REPO_DIR="${REPO_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)}"
cd "$REPO_DIR" || exit 1

BASE_IMAGE="nba.shiny_base:latest"
DRY_RUN="${DRY_RUN:-0}"
REBUILD_BASE="${REBUILD_BASE:-0}"
PROVISION="${PROVISION:-0}"
SKIP_ENTRY="${SKIP_ENTRY:-0}"

step() { printf "\n=== %s ===\n\n" "$*"; }

if [ "$DRY_RUN" = "1" ]; then
  step "[dry-run] ensure base image ${BASE_IMAGE}"
elif [ "$REBUILD_BASE" = "1" ] || ! docker image inspect "$BASE_IMAGE" >/dev/null 2>&1; then
  step "Building base image ${BASE_IMAGE} (slow)"
  docker build -f ./docker/Dockerfile_base --progress=plain -t "$BASE_IMAGE" . || exit 1

  step "Verifying nba.shiny.core in the base image"
  docker run --rm "$BASE_IMAGE" R -e 'library(nba.shiny.core); cat("nba.shiny.core ok\n")' || exit 1
else
  step "Reusing base image ${BASE_IMAGE} (set REBUILD_BASE=1 to rebuild)"
fi

step "Building/deploying league containers"
BUILD_ENTRY=0 DRY_RUN="$DRY_RUN" PROVISION="$PROVISION" bash "${REPO_DIR}/cron.sh" || exit 1

if [ "$SKIP_ENTRY" = "1" ]; then
  step "Skipping entry point (SKIP_ENTRY=1)"
else
  step "Building/deploying entry point"
  DRY_RUN="$DRY_RUN" PROVISION="$PROVISION" bash "${REPO_DIR}/build_entry.sh" || exit 1
fi

step "Done"
