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

# Create and configure the Space if it does not already exist. Idempotent.
adapter_provision() {
  local slug="$1"
  local image="$2"
  local app="${3:-nba.shiny}"
  local name="nba-shiny-${slug}"
  local repo="${HF_OWNER}/${name}"

  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] provision HF space ${repo} (FROM ${image}, app ${app})"
    return 0
  fi

  : "${HUGGINGFACE_TOKEN:?HUGGINGFACE_TOKEN is required for HF provisioning}"

  local status
  status="$(curl -s -o /dev/null -w '%{http_code}' \
    -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    "https://huggingface.co/api/spaces/${repo}")"

  if [ "$status" = "200" ]; then
    echo "[hf] ${repo} already exists"
    return 0
  fi

  echo "[hf] creating ${repo}"
  curl -sf -X POST "https://huggingface.co/api/repos/create" \
    -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
    -H "Content-Type: application/json" \
    -d "{\"type\":\"space\",\"name\":\"${name}\",\"private\":false,\"sdk\":\"docker\"}" >/dev/null || return 1

  # Dockerfile runs our prebuilt image on the Hugging Face port.
  local df_json="FROM ${image}\\nEXPOSE 7860\\nUSER rstudio\\nCMD [\\\"R\\\", \\\"-e\\\", \\\"options('shiny.port'=7860, 'shiny.host'='0.0.0.0'); ${app}::run_app()\\\"]"

  printf '%s\n' \
    '{"key":"header","value":{"summary":"provision space"}}' \
    "{\"key\":\"file\",\"value\":{\"path\":\"Dockerfile\",\"content\":\"${df_json}\",\"encoding\":\"utf-8\"}}" |
    curl -sf -X POST "https://huggingface.co/api/spaces/${repo}/commit/main" \
      -H "Authorization: Bearer ${HUGGINGFACE_TOKEN}" \
      -H "Content-Type: application/x-ndjson" \
      --data-binary @- >/dev/null || return 1

  echo "[hf] provisioned ${repo}"
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
