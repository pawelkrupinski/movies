#!/usr/bin/env bash
# The one way a local script gets a secret: from its environment, else from the Touch ID vault.
#
# There is no .env.local any more (deleted 2026-10-09, after a credential theft). Secrets live
# only in the vault (`secrets`, namespace `movies`), and the established pattern — infra/bin/tf —
# is a script that re-runs ITSELF under `secrets run`, which unlocks the namespace (one Touch ID)
# and execs it again with the keys as environment variables. A variable that is already set (CI,
# a caller that exported it, the launchd wrapper in local-mirror/service.sh) means no vault lookup
# at all. Non-secret configuration (hosts, local URIs, database names) never comes through here:
# it is a plain environment variable with a committed default at its point of use.
#
# Sourcing this file is side-effect free: it defines one function.
#
#   ensure_vault_env SPEC... -- CMD...
#
# SPEC is VAR or VAR=KEY (the vault key, when it is named differently from the variable), as
# `secrets run` takes them. When every VAR is set, returns 0 and does nothing. Otherwise execs
# `secrets run movies <only the missing SPECs> -- CMD...` — CMD is normally `"$BASH" "$0" "$@"`,
# so the caller starts over with the variables set and passes this check the second time.
# Returns 1 (no exec) when something is missing and there is no `secrets` CLI on PATH — a CI
# runner, a Linux box — so the caller can fail with its own message.
ensure_vault_env() {
  local missing=() spec var
  while [ $# -gt 0 ] && [ "$1" != "--" ]; do
    spec="$1"; var="${spec%%=*}"; shift
    [ -n "${!var:-}" ] || missing+=("$spec")
  done
  [ "${1:-}" = "--" ] && shift
  [ ${#missing[@]} -gt 0 ] || return 0
  if ! command -v secrets >/dev/null 2>&1; then
    echo "[vault] ${missing[*]%%=*} not set, and no \`secrets\` CLI on PATH to read it from the vault" >&2
    return 1
  fi
  exec secrets run movies "${missing[@]}" -- "$@"
}
