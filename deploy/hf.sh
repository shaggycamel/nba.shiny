#!/bin/bash
# Hugging Face Spaces deploy adapter -----------------------------------------
# League container naming: <owner>/nba-shiny-<slug> served at
# https://<owner>-nba-shiny-<slug>.hf.space (embed the .hf.space URL).
# Requires HUGGINGFACE_TOKEN unless DRY_RUN=1.

: "${HF_OWNER:=shaggycamel}"
: "${HF_IMAGE_BASENAME:=nba.shiny}"

hf_space_id() {
  echo "${HF_OWNER}/nba-shiny-${1}"
}

adapter_url() {
  echo "https://${HF_OWNER}-nba-shiny-${1}.hf.space"
}

adapter_deploy() {
  local slug="$1"
  local image="$2"

  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] restart HF space $(hf_space_id "$slug") (image ${image:-<none>}) -> $(adapter_url "$slug")"
    return 0
  fi

  : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN is required for HF deploys}"
  curl -sf -X POST \
    "https://huggingface.co/api/spaces/$(hf_space_id "$slug")/restart?factory=true" \
    -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}"
}

adapter_teardown() {
  local slug="$1"
  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] teardown HF space $(hf_space_id "$slug")"
    return 0
  fi
  echo "hf adapter: teardown for $(hf_space_id "$slug") is not automated (pause it in the Hub UI)" >&2
}
