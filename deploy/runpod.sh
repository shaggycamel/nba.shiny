#!/bin/bash
# RunPod deploy adapter (scaffold) -------------------------------------------
# Future provider. RunPod proxy URLs are pod-id based and change on recreate,
# so this adapter must publish a stable container_url (custom domain / reverse
# proxy) into the league registry rather than deriving it. See the
# "Provider Portability" section of docs/nba-shiny-architecture.md.
#
# NOT FUNCTIONAL YET: adapter_deploy fails closed so a mis-set DEPLOY_PROVIDER
# cannot silently no-op in production.

: "${RUNPOD_API_KEY:=}"

adapter_url() {
  # TODO: read the stable container_url for this league from fty.league.
  echo "https://${1}.<runpod-custom-domain>"
}

adapter_provision() {
  echo "runpod adapter: provision for ${1} not implemented (create the pod + proxy externally)" >&2
  return 1
}

adapter_deploy() {
  local slug="$1"
  local image="$2"

  if [ "${DRY_RUN:-0}" = "1" ]; then
    echo "[dry-run] runpod deploy ${slug} (image ${image:-<none>})"
    return 0
  fi

  echo "runpod adapter: not implemented (would redeploy ${slug} with ${image})" >&2
  return 1
}

adapter_teardown() {
  echo "runpod adapter: teardown for ${1} not implemented" >&2
  return 1
}
