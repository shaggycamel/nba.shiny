#!/bin/bash
set -e # force exit if error

# ── Config ────────────────────────────────────────────────────────────────────
DOCKERHUB_USER="${DOCKERHUB_USER:-shaggycamel}"
IMAGE_NAME="nba.shiny"
TAG="${TAG:-latest}"
FULL_IMAGE="$DOCKERHUB_USER/$IMAGE_NAME:$TAG"

# ── Regenerate data ────────────────────────────────────────────────────────
echo "▶ Running _generate_all.R..."
Rscript ./data-raw/_generate_all.R

# ── Build Docker image ─────────────────────────────────────────────────────
echo "▶ Building Docker image: $FULL_IMAGE..."
docker build -f ./docker/Dockerfile --progress=plain -t "$FULL_IMAGE" .

# ── Log in to Docker Hub ───────────────────────────────────────────────────
echo "▶ Logging in to Docker Hub..."
echo "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USER" --password-stdin

# ── Push to Docker Hub ─────────────────────────────────────────────────────
echo "▶ Pushing $FULL_IMAGE to Docker Hub..."
docker push "$FULL_IMAGE"

# ── Trigger Render deploy ──────────────────────────────────────────────────
echo "▶ Triggering Render deployment..."
curl -X POST https://api.render.com/deploy/srv-d6lam94hg0os73c8til0?key=1myqCIiCnvk

echo "✔ Done: $FULL_IMAGE deployed to Render"

