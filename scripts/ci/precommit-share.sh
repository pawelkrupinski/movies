#!/usr/bin/env bash
# THE LANDING CHECK'S SHARES (.github/workflows/precommit-check.yml): how one job of the check calls the pre-push hook, and
# what the hook would run there -- so a job sets up sbt, Node and the tools only when its share has a check to run.
#
#   precommit-share.sh env <share>     the job's hook environment, KEY=VALUE lines for $GITHUB_ENV
#   precommit-share.sh checks <share>  the checks the hook runs for <share>, space-separated (none: an empty line)
#
# <share>: `rest` (every selected check but the other shares'), `identity`, `oldrefs`, `page`, `heavy` (the heavy rows
# named by $ROWS), or `plan` (every check, the heavy rows among them: the plan job's). The mode is the branch's:
# check/fast-<id> is agentctl's batch check (--fast-only: the sbt check and the touched files' linters, all in `rest`),
# every other branch the change's own checks (--checks-only). The range is the change since it left main
# (`<merge-base with origin/main>..HEAD`); origin/main is fetched before.
#
# `checks` is the hook's own selection (PREPUSH_LIST=1, scripts/hooks/pre-push), under the very environment `env` gives the
# hook step, so a job that sets nothing up for an empty share runs exactly what it ran when it always set up: and its
# hook step still runs, so a check this missed fails there for want of sbt rather than going unchecked.
set -euo pipefail

usage() { echo "usage: $0 env|checks rest|identity|oldrefs|page|heavy|plan" >&2; exit 2; }
[ $# -eq 2 ] || usage
what=$1 share=$2
case "$what" in env|checks) ;; *) usage ;; esac
case "$share" in rest|identity|oldrefs|page|heavy|plan) ;; *) usage ;; esac

ref="${GITHUB_REF_NAME:-$(git rev-parse --abbrev-ref HEAD)}"
mode=--checks-only
case "$ref" in fast-*|check/fast-*) mode=--fast-only ;; esac
range="$(git merge-base HEAD origin/main)..HEAD"

vars=("PREPUSH_MODE=$mode" "PREPUSH_RANGE=$range")
case "$share" in
    rest)
        elsewhere="page oldrefs"; [ "$mode" = --fast-only ] || elsewhere+=" identity"
        vars+=("PREPUSH_ELSEWHERE=$elsewhere") ;;
    heavy) vars+=("PREPUSH_HEAVY_ROWS=1" "PREPUSH_ONLY=${ROWS:?heavy: ROWS names the rows}") ;;
    plan) vars+=("PREPUSH_HEAVY_ROWS=1") ;;
    *) vars+=("PREPUSH_ONLY=$share") ;;
esac

if [ "$what" = env ]; then printf '%s\n' "${vars[@]}"; exit 0; fi
# the fast-only set is the sbt check and the linters, all the rest's: nothing for another share
if [ "$mode" = --fast-only ] && [ "$share" != rest ] && [ "$share" != plan ]; then echo; exit 0; fi
env "${vars[@]}" PREPUSH_LIST=1 "$(dirname "$0")/../hooks/pre-push" "$mode" "$range" | tr '\n' ' ' | sed 's/ *$//'
echo
