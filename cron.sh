#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file
#
# League-scoped data refresh + deploy. Generates the shared NBA base once, then
# generates/builds/deploys one container per league via the deploy adapter (see
# deploy/adapter.sh). Data is customer-agnostic; the entry point maps customers
# to leagues at runtime.

# ── Config ────────────────────────────────────────────────────────────────────

# If executing from cron source .profile (containing tokens)
if [ ! -t 1 ] && [ -f ./.profile ]; then
  # shellcheck source=/dev/null
  source ./.profile
fi

set -uo pipefail

# Work from the repo root regardless of the invoking cwd
REPO_DIR="${REPO_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)}"
cd "$REPO_DIR" || exit 1

DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny"
BASE_IMAGE="nba.shiny_base:latest"
TAG="${TAG:-latest}"
SEASON="${NBA_SEASON:-2025-26}"
EXCLUDE_LEAGUES="${EXCLUDE_LEAGUES:-}"
DRY_RUN="${DRY_RUN:-0}"
REBUILD_BASE="${REBUILD_BASE:-0}"
BUILD_ENTRY="${BUILD_ENTRY:-0}"

DOCKERHUB_TOKEN="${DOCKERHUB_TOKEN:-}"
HUGGINGFACE_TOKEN="${HUGGINGFACE_TOKEN:-}"
DATABASE_URL="${DATABASE_URL:-}"

# shellcheck source=/dev/null
source "${REPO_DIR}/deploy/adapter.sh"

step() { printf "\n▶ %s\n\n" "$*"; }
fail() { printf "  ✘ %s\n" "$*" >&2; return 1; }

# ── Base image (deps only, rebuilt rarely) ────────────────────────────────────

build_base() {
  if [ "$DRY_RUN" = "1" ]; then
    step "[dry-run] ensure base image ${BASE_IMAGE} exists"
    return 0
  fi

  if [ "$REBUILD_BASE" = "1" ] || ! docker image inspect "$BASE_IMAGE" >/dev/null 2>&1; then
    step "Building base image ${BASE_IMAGE}"
    docker build -f ./docker/Dockerfile_base --progress=plain -t "$BASE_IMAGE" . || fail "base image build failed"
  else
    step "Reusing base image ${BASE_IMAGE} (set REBUILD_BASE=1 to rebuild)"
  fi
}

# ── Per-league build/deploy ───────────────────────────────────────────────────

process_league() {
  local platform="$1"
  local league_id="$2"
  local slug="$3"
  local image="${DOCKERHUB_USER}/${IMAGE_NAME}-${slug}:${TAG}"

  if [ "$DRY_RUN" != "1" ]; then
    step "Generating data for ${slug} (LEAGUE_ID=${league_id})"
    rm -f ./data/*.rda ./*.tar.gz || fail "cleaning build artifacts"
    NBA_SEASON="$SEASON" LEAGUE_ID="$league_id" Rscript ./data-raw/_generate_league.R || fail "data generation"

    step "Building R package tarball"
    R CMD build . || fail "package build"

    step "Building image: ${image}"
    docker build -f ./docker/Dockerfile -t "$image" . || fail "docker build"
  else
    step "[dry-run] generate + build image ${image}"
  fi

  step "Pushing ${image}"
  if [ "$DRY_RUN" != "1" ]; then
    docker push "$image" || fail "docker push"
  fi

  step "Deploying ${slug}"
  adapter_deploy "$slug" "$image" || fail "deploy"

  printf "  → %s\n" "$(adapter_url "$slug")"
}

# ── Run ───────────────────────────────────────────────────────────────────────

: "${DATABASE_URL:?DATABASE_URL is required}"

build_base || exit 1

if [ "$DRY_RUN" != "1" ]; then
  : "${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"

  step "Generating shared NBA base"
  NBA_SEASON="$SEASON" Rscript ./data-raw/_generate_base.R || exit 1

  step "Logging in to Docker Hub"
  echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin || exit 1
fi

step "Fetching active leagues for season ${SEASON}"
LEAGUES="$(psql "$DATABASE_URL" -t -A -F',' -c \
  "SELECT DISTINCT platform, league_id FROM fty.league WHERE season = '${SEASON}' ORDER BY league_id;" 2>/dev/null)"

if [ -z "$LEAGUES" ]; then
  printf "⚠ No leagues found for season %s\n" "$SEASON"
  exit 1
fi

FAILED=()

while IFS=',' read -r PLATFORM LEAGUE_ID; do
  [ -z "$PLATFORM" ] && continue

  case " ${EXCLUDE_LEAGUES} " in
    *" ${LEAGUE_ID} "*)
      printf "\n• Skipping league %s (excluded)\n" "$LEAGUE_ID"
      continue
      ;;
  esac

  SLUG="$(printf '%s-%s' "$PLATFORM" "$LEAGUE_ID" | tr '[:upper:]' '[:lower:]')"
  step "Processing league: ${SLUG} (${PLATFORM} ${LEAGUE_ID})"

  if ( process_league "$PLATFORM" "$LEAGUE_ID" "$SLUG" ); then
    printf "✔ %s done\n" "$SLUG"
  else
    printf "✘ %s FAILED — continuing to next league\n" "$SLUG"
    FAILED+=("$SLUG")
  fi
done <<< "$LEAGUES"

# ── Entry point (optional: only rebuild when its code changes) ────────────────
if [ "$BUILD_ENTRY" = "1" ]; then
  step "Building/deploying entry point"
  if bash "${REPO_DIR}/build_entry.sh"; then
    printf "✔ entry point done\n"
  else
    printf "✘ entry point FAILED\n"
    FAILED+=("entry")
  fi
fi

if [ ${#FAILED[@]} -gt 0 ]; then
  printf "\n⚠ Failed leagues: %s\n" "${FAILED[*]}"
  exit 1
fi

printf "\n✔ All leagues processed\n"
