#!/bin/bash
# Deploy adapter interface ---------------------------------------------------
# Provider-agnostic deploy hooks, so cron.sh never calls a provider directly.
# Select the implementation with DEPLOY_PROVIDER (default: hf). Each provider
# script must define the following functions:
#
#   adapter_url <slug>            -> print the container URL for a league
#   adapter_provision <slug> <image> <app> -> create/configure the container (idempotent)
#   adapter_deploy <slug> <image> -> ensure the provider runs <image> for <slug>
#   adapter_teardown <slug>       -> stop/remove the league container
#
# Source this file from cron.sh; it sources the selected provider script.

: "${DEPLOY_PROVIDER:=hf}"

_adapter_dir="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
_adapter_script="${_adapter_dir}/${DEPLOY_PROVIDER}.sh"

if [ ! -f "$_adapter_script" ]; then
  echo "deploy adapter: unknown DEPLOY_PROVIDER '${DEPLOY_PROVIDER}' (${_adapter_script} not found)" >&2
  return 1 2>/dev/null || exit 1
fi

# shellcheck source=/dev/null
source "$_adapter_script"
