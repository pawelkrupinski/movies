#!/usr/bin/env bash
# The shared harness of the prod-ops scripts (scripts/mongo-grants, scripts/fleet-db-finish, and the next one): a tool
# that reads or changes PROD, runs as `--check` then `--apply` (or a destructive flag with exact names), is idempotent,
# refuses unsafe states in words, and has a shell test run against a fake mongosh (scripts/prod-ops-fixture.sh).
# docs/prod-ops-scripts.md says how to write one. Sourcing this defines functions only; bash 3.2 compatible.
#
#   prod_ops_init <tool>                 name the tool: the prefix of its refusals ("[fleet-db-finish] ...")
#   prod_ops_mode "$@"                   exactly one of --check | --apply (or -h/--help: the header, exit 0); sets
#                                        PROD_OPS_MODE. Anything else: the usage on stderr, exit 2.
#   prod_ops_exact_names <flag> "<names>" "$@"
#                                        "$@" are the arguments AFTER <flag>: they must be exactly <names>, each once, in
#                                        any order (no pattern, nothing extra). Else exit 2, before any credential is asked.
#   prod_ops_exec <--both|--root|--app> <command...>
#                                        run a command under scripts/with-prod-mongo.sh: ONE Touch ID, ONE tunnel; the
#                                        command gets MONGODB_URI (app) and, with --both, MONGO_ROOT_URI, both tunnelled.
#   prod_ops_session <--both|--root|--app> "$@"
#                                        for a script that is itself the command: re-runs itself (same arguments) under
#                                        prod_ops_exec once, and on the second pass returns with mongosh_at, as_root and
#                                        as_app defined. Call it after the arguments are validated.
#   as_root <js> / as_app <js>           mongosh --quiet --eval <js> as MONGO_ROOT_URI / MONGODB_URI
#   last_line                            filter: the last line of stdin (mongosh prints warnings first)
#   prod_ops_phase <title>               print "== <title>": each step of --apply says which one it is in
#   prod_ops_refuse <reason...>          "[tool] reason" on stderr, exit 1: the state says do not go on
#   prod_ops_usage_error <reason...>     "[tool] reason" on stderr, exit 2: the caller's mistake
#
# Rules the helpers cannot enforce: every phase is idempotent (a second --apply changes nothing and says so);
# --check changes nothing; a destructive flag names exactly what it destroys and re-checks the name where it acts.
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/tool-conventions.sh" # usage_from_header

PROD_OPS_HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROD_OPS_TOOL="${PROD_OPS_TOOL:-prod-ops}"
PROD_OPS_MODE=""

prod_ops_init() { PROD_OPS_TOOL="$1"; }
prod_ops_usage_error() { echo "[$PROD_OPS_TOOL] $*" >&2; exit 2; }
prod_ops_refuse() { echo "[$PROD_OPS_TOOL] $*" >&2; exit 1; }
prod_ops_phase() { echo "== $*"; }
last_line() { tail -n 1; }

prod_ops_mode() {
  case "${1:-}" in
    -h|--help) usage_from_header; exit 0 ;;
    --check|--apply) [ $# -eq 1 ] || { usage_from_header >&2; exit 2; } ;;
    *) usage_from_header >&2; exit 2 ;;
  esac
  # shellcheck disable=SC2034 # read by the tool that called this
  PROD_OPS_MODE="${1#--}"
}

prod_ops_exact_names() {
  local flag="$1" names="$2" want got seen expected
  shift 2
  # shellcheck disable=SC2206 # word-split on purpose: <names> is a space-separated list of plain names
  expected=($names)
  [ $# -eq ${#expected[@]} ] || prod_ops_usage_error "$flag takes exactly the ${#expected[@]} names: $names (got: $*)"
  for want in "${expected[@]}"; do
    seen=0; for got in "$@"; do [ "$got" = "$want" ] && seen=$((seen + 1)); done
    [ "$seen" -eq 1 ] || prod_ops_usage_error "$flag takes exactly the ${#expected[@]} names, each once: $names (got: $*)"
  done
}

prod_ops_exec() {
  local flag="$1"; shift
  case "$flag" in
    --both|--root) exec "$PROD_OPS_HERE/with-prod-mongo.sh" "$flag" "$@" ;;
    --app) exec "$PROD_OPS_HERE/with-prod-mongo.sh" "$@" ;;
    *) prod_ops_usage_error "prod_ops_exec: unknown credential flag $flag" ;;
  esac
}

prod_ops_session() {
  local flag="$1"; shift
  if [ -z "${PROD_OPS_TUNNELLED:-}" ]; then
    prod_ops_exec "$flag" env PROD_OPS_TUNNELLED=1 "$BASH" "$0" "$@"
  fi
  # shellcheck source=scripts/local-mirror/prod-tunnel.sh
  . "$PROD_OPS_HERE/local-mirror/prod-tunnel.sh" # mongosh_at
}

as_root() { mongosh_at "$MONGO_ROOT_URI" --quiet --eval "$1"; }
as_app() { mongosh_at "$MONGODB_URI" --quiet --eval "$1"; }
