#!/bin/bash
set -e # force exit if error
cd ./github/nba.shiny # directory

# ── Config ────────────────────────────────────────────────────────────────────
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny"
TAG="${TAG:-latest}"
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"
HUGGINGFACE_TOKEN="$HUGGINGFACE_TOKEN"

# Custom function for messages
step() { printf "\n▶ %s\n\n" "$*"; }

# ── Regenerate data ────────────────────────────────────────────────────────
step "Running _generate_all.R..."
Rscript ./data-raw/_generate_all.R

# ── Build R package tarball ────────────────────────────────────────────────
step "Building R package tarball..."
R CMD build .

# ── Build Docker image ─────────────────────────────────────────────────────
step "Building Docker image: $FULL_IMAGE..."
docker build -f ./docker/Dockerfile -t "$FULL_IMAGE" .

# ── Log in to Docker Hub ───────────────────────────────────────────────────
step "Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Push to Docker Hub ─────────────────────────────────────────────────────
step "Pushing $FULL_IMAGE to Docker Hub..."
docker push "$FULL_IMAGE"

# ── Trigger Huggingface rebuild ────────────────────────────────────────────
step "Triggering Huggingface rebuild..."
curl -X POST \
  "https://huggingface.co/api/spaces/shaggycamel/nba-shiny/restart?factory=true" \
  -H "Authorization: Bearer $HUGGINGFACE_TOKEN"

printf "\n✔ Done: %s rebuilt on Huggingface\n" "$FULL_IMAGE"

