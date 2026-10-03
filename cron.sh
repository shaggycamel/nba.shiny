#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file

# ── Config ────────────────────────────────────────────────────────────────────

# If executing from cron source .profile (containing tokens)
if [ ! -t 1 ]; then
    source ./.profile
fi

set -euo pipefail

# Work from the repo root regardless of the invoking cwd
cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Variables
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny"
TAG="${TAG:-latest}"

# Single-image mode (short term): one image, one HuggingFace space.
SINGLE_CUSTOMER_ID="${SINGLE_CUSTOMER_ID:-cus_a24dgn8202vt}"
HF_SPACE="${HF_SPACE:-shaggycamel/nba-shiny}"

DOCKERHUB_TOKEN="${DOCKERHUB_TOKEN:?DOCKERHUB_TOKEN not set}"
HUGGINGFACE_TOKEN="${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN not set}"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

# ── Log in to Docker Hub (once) ─────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Base image check (built externally, cron does not build it) ─────────────
step "Checking base image..."
if ! docker image inspect nba.shiny_base:latest >/dev/null 2>&1; then
    printf "✘ nba.shiny_base:latest not found — build it before running cron\n" >&2
    exit 1
fi

# ── Single-image build/deploy ────────────────────────────────────────────────
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"

step "Cleaning previous build artifacts..."
rm -f ./data-raw/*.rda ./*.tar.gz docker/*.tar.gz

step "Regenerating data for $SINGLE_CUSTOMER_ID..."
CUSTOMER_ID="$SINGLE_CUSTOMER_ID" Rscript ./data-raw/_generate_all.R

step "Building R package tarball..."
R CMD build .

step "Building Docker image: $FULL_IMAGE..."
docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" .

step "Pushing $FULL_IMAGE to Docker Hub..."
docker push "$FULL_IMAGE"

step "Triggering HuggingFace rebuild for $HF_SPACE..."
HTTP_STATUS=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
  "https://huggingface.co/api/spaces/$HF_SPACE/restart?factory=true" \
  -H "Authorization: Bearer $HUGGINGFACE_TOKEN")
if [ "$HTTP_STATUS" -lt 200 ] || [ "$HTTP_STATUS" -ge 300 ]; then
    printf "✘ HuggingFace restart failed for %s (HTTP %s)\n" "$HF_SPACE" "$HTTP_STATUS" >&2
    exit 1
fi
printf "✔ HuggingFace rebuild triggered for %s (HTTP %s)\n" "$HF_SPACE" "$HTTP_STATUS"

printf "\n✔ Single image processed\n"

# ── Per-customer build/deploy (disabled short term) ──────────────────────────
# Restore this block to build/push/trigger one image + HF space per active
# customer. Note: process_customer is called in an `if` condition, so `set -e`
# inside it is suppressed — use `|| return 1` on each critical step instead.
#
# process_customer() {
#     local CUSTOMER_ID="$1"
#     local SLUG="$2"
#     local FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME-$SLUG:$TAG"
#
#     step "Cleaning previous build artifacts for $SLUG..."
#     rm -f ./data-raw/*.rda ./*.tar.gz docker/*.tar.gz || return 1
#
#     step "Regenerating data for $SLUG..."
#     CUSTOMER_ID="$CUSTOMER_ID" Rscript ./data-raw/_generate_all.R || return 1
#
#     step "Building R package tarball for $SLUG..."
#     R CMD build . || return 1
#
#     step "Building Docker image: $FULL_IMAGE..."
#     docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" . || return 1
#
#     step "Pushing $FULL_IMAGE to Docker Hub..."
#     docker push "$FULL_IMAGE" || return 1
#
#     step "Triggering HuggingFace rebuild for $SLUG..."
#     local STATUS
#     STATUS=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
#       "https://huggingface.co/api/spaces/shaggycamel/nba-shiny-$SLUG/restart?factory=true" \
#       -H "Authorization: Bearer $HUGGINGFACE_TOKEN")
#     if [ "$STATUS" -lt 200 ] || [ "$STATUS" -ge 300 ]; then
#         printf "✘ HuggingFace restart failed for %s (HTTP %s)\n" "$SLUG" "$STATUS" >&2
#         return 1
#     fi
# }
#
# step "Fetching active customers..."
# CUSTOMERS=$(psql "$DATABASE_URL" -t -A -F',' -c \
#   "SELECT customer_id, slug FROM fty.customer WHERE is_active;")
#
# FAILED=()
#
# while IFS=',' read -r CUSTOMER_ID SLUG; do
#     [ -z "$CUSTOMER_ID" ] && continue
#     step "Processing customer: $SLUG ($CUSTOMER_ID)"
#
#     if process_customer "$CUSTOMER_ID" "$SLUG"; then
#         printf "✔ %s done\n" "$SLUG"
#     else
#         printf "✘ %s FAILED — continuing to next customer\n" "$SLUG"
#         FAILED+=("$SLUG")
#     fi
# done <<< "$CUSTOMERS"
#
# if [ ${#FAILED[@]} -gt 0 ]; then
#     printf "\n⚠ Failed customers: %s\n" "${FAILED[*]}"
#     exit 1
# fi
#
# printf "\n✔ All customers processed\n"
