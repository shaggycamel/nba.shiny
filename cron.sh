#!/bin/bash

# Remember to chmod +x cron.sh on nuc after pulling latest file

# ── Config ────────────────────────────────────────────────────────────────────

# If executing from cron source .profile (containing tokens)
if [ ! -t 1 ]; then
    source ./.profile
fi

# directory
cd ./github/nba.shiny

# Variables
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny"
TAG="${TAG:-latest}"
HUGGINGFACE_TOKEN="$HUGGINGFACE_TOKEN"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

# ── Per-customer build/deploy ───────────────────────────────────────────────
process_customer() {
    set -e  # bail out of THIS customer's run on first error, doesn't kill the outer loop
    local CUSTOMER_ID="$1"
    local SLUG="$2"
    local FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME-$SLUG:$TAG"

    step "Cleaning previous build artifacts for $SLUG..."
    rm -f ./data-raw/*.rda ./*.tar.gz

    step "Regenerating data for $SLUG..."
    CUSTOMER_ID="$CUSTOMER_ID" Rscript ./data-raw/_generate_all.R

    step "Building R package tarball for $SLUG..."
    R CMD build .

    step "Building Docker image: $FULL_IMAGE..."
    docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" .

    step "Pushing $FULL_IMAGE to Docker Hub..."
    docker push "$FULL_IMAGE"

    step "Triggering Huggingface rebuild for $SLUG..."
    curl -sf -X POST \
      "https://huggingface.co/api/spaces/shaggycamel/nba-shiny-$SLUG/restart?factory=true" \
      -H "Authorization: Bearer $HUGGINGFACE_TOKEN"
}

# ── Log in to Docker Hub (once) ─────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Fetch active customers ───────────────────────────────────────────────────
step "Fetching active customers..."
CUSTOMERS=$(psql "$DATABASE_URL" -t -A -F',' -c \
  "SELECT customer_id, slug FROM fty.customer WHERE is_active;")

FAILED=()

while IFS=',' read -r CUSTOMER_ID SLUG; do
    [ -z "$CUSTOMER_ID" ] && continue
    step "Processing customer: $SLUG ($CUSTOMER_ID)"

    if process_customer "$CUSTOMER_ID" "$SLUG"; then
        printf "✔ %s done\n" "$SLUG"
    else
        printf "✘ %s FAILED — continuing to next customer\n" "$SLUG"
        FAILED+=("$SLUG")
    fi
done <<< "$CUSTOMERS"

if [ ${#FAILED[@]} -gt 0 ]; then
    printf "\n⚠ Failed customers: %s\n" "${FAILED[*]}"
    exit 1
fi

printf "\n✔ All customers processed\n"