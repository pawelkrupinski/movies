#!/usr/bin/env bash
# HOW MANY OF A COUNTRY'S SPLIT ORDER-INDEPENDENCE ROWS A CONVERGENCE RUN RUNS: `both` (p01 and p12: passes 0,1 and
# 1,2, holding all three passes to one corpus) or `one` (p01 alone: passes 0 and 1).
#
# BOTH on main, always. Off main (an agent's branch; a measure/ branch runs no order row at all) ONE, but only when the
# ref's change from main is KNOWN and touches nothing .github/order-independence-paths.txt lists -- what can make the
# films depend on arrival order. Anything else is `both`: a read that fails or hangs, an answer that is not a file
# list, a list Forgejo may have cut short (its diff stops at 100 files), a path list that cannot be read.
#
# THE TRADE-OFF (also in identity-model-convergence.yml's leg comment): `one` compares two arrival orders where `both`
# compares three, so an order dependence that shows only between passes 1 and 2 goes unseen on that branch -- until
# its change reaches main, where both rows run on every push. A change that can move the order (any module's main code,
# the convergence specs, the build, this wiring) runs both wherever it is. Saves one ~8-10 minute row per split country
# (DE, UK, US) per convergence run off main; Poland and Spain run their passes in one row either way.
#
# usage: order-phases.sh <ref> <sha>    env: GH_REPO (owner/repo), GH_TOKEN; ORDER_PATHS (default the paths file)
# Prints `both` or `one`, and why on stderr; exits 0 either way. Tested by scripts/ci/order-phases-test.sh.
set -uo pipefail
ref=${1:-} sha=${2:-}
here=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
paths_file=${ORDER_PATHS:-$here/.github/order-independence-paths.txt}
both() { echo "order-phases: both -- $*" >&2; echo both; exit 0; }

[ "$ref" = refs/heads/main ] && both "main runs every order row"
[ -n "$sha" ] && [ -n "${GH_REPO:-}" ] || both "no ref, commit or repository to compare"
mapfile -t prefixes < <(grep -v '^\s*#' "$paths_file" 2>/dev/null | sed '/^\s*$/d')
[ "${#prefixes[@]}" -gt 0 ] || both "no order paths readable at $paths_file"

# allow-silenced: a read that fails is never "nothing changed" -- it runs both rows (the `|| both` below).
files=$(timeout "${ORDER_PHASES_TIMEOUT:-60}" gh api "repos/$GH_REPO/compare/main...$sha" --jq '.files | if type == "array" then .[].filename else error("no file list") end' 2> /dev/null) \
  || both "could not read the change from main"
count=$(printf '%s' "$files" | grep -c .)
[ "$count" -lt "${ORDER_PHASES_MAX_FILES:-100}" ] || both "$count files changed: a list that may be cut short"
while IFS= read -r file; do
  [ -n "$file" ] || continue
  for prefix in "${prefixes[@]}"; do
    case "$file" in "$prefix"*) both "$file is order code ($prefix)" ;; esac
  done
done <<< "$files"
echo "order-phases: one -- $count file(s) changed from main, none order code" >&2
echo one
