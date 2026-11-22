#!/usr/bin/env bash
# Purpose: The landing check's shares (.github/workflows/precommit-check.yml)
# THE LANDING CHECK'S SHARES (.github/workflows/precommit-check.yml): how one job of the check calls the pre-push hook, and
# what the hook would run there -- so a job sets up sbt, Node and the tools only when its share has a check to run.
#
#   precommit-share.sh env <share>     the job's hook environment, KEY=VALUE lines for $GITHUB_ENV
#   precommit-share.sh checks <share>  the checks the hook runs for <share>, space-separated (none: an empty line)
#
# <share>: `rest` (every selected check but the other shares', Main's heavy rows among them: no job of their own,
# which waited up to 17 min for a slot of its own, 2026-11-18), `identity`, or `plan` (every check of the
# jobs after it: the plan job's). The mode is the branch's:
# check/fast-<id> is agentctl's batch check (--fast-only: the sbt check and the touched files' linters, all in `rest`),
# check/verify-<full base sha>-<id> a change's combined-tree verification (--verify-only, all in `rest` too: the change
# fully checked on that older main, rebased onto one that moved with no file overlap; its range is `<that sha>..HEAD`,
# what landed since and the change together), check/verifyareas-<full base sha>-<id> the same verification plus the area
# tests (agentctl's MODULE OVERLAP, the owner 2026-11-18: the two sides touch one sbt module, so the area shares --
# `identity` -- run the change's own checks, --checks-only, over that same range; the rest is the verify round's), every
# other branch the change's own checks (--checks-only); a red- after
# check/ (a gate-red fix's: check/red-fast-<id>, check/red-<id>) changes nothing here. The range is
# otherwise the change since it left main (`<merge-base with origin/main>..HEAD`); origin/main is fetched before.
#
# `checks` is the hook's own selection (PREPUSH_LIST=1, scripts/hooks/pre-push), under the very environment `env` gives the
# hook step, so a job that sets nothing up for an empty share runs exactly what it ran when it always set up: and its
# hook step still runs, so a check this missed fails there for want of sbt rather than going unchecked.
set -euo pipefail

usage() { echo "usage: $0 env|checks rest|identity|plan" >&2; exit 2; }
[ $# -eq 2 ] || usage
what=$1 share=$2
case "$what" in env|checks) ;; *) usage ;; esac
case "$share" in rest|identity|plan) ;; *) usage ;; esac

ref="${GITHUB_REF_NAME:-$(git rev-parse --abbrev-ref HEAD)}"
# a gate-red fix's check is check/red-<the rest>: the prefix names the runner lane (precommit-check.yml), not the mode
ref="${ref#check/}"; ref="${ref#red-}"
mode=--checks-only areas=""
case "$ref" in fast-*) mode=--fast-only ;; verify-*) mode=--verify-only ;; verifyareas-*) mode=--verify-only areas=1 ;; esac
if [ "$mode" = --verify-only ]; then
    full="${ref#verify-}"; full="${full#verifyareas-}"; full="${full:0:40}"
    git rev-parse --verify -q "$full^{commit}" > /dev/null || { echo "$0: $ref names no commit ($full)" >&2; exit 1; }
    range="$full..HEAD"
else
    range="$(git merge-base HEAD origin/main)..HEAD"
fi

# the area shares of a verifyareas- round: the change's own checks of the area, over the verification's range
area_mode=$mode; [ -z "$areas" ] || [ "$share" != identity ] || area_mode=--checks-only
vars=("PREPUSH_MODE=$area_mode" "PREPUSH_RANGE=$range")
case "$share" in
    rest)
        elsewhere=""; [ "$mode" != --checks-only ] || elsewhere="identity"
        vars+=("PREPUSH_HEAVY_ROWS=1" "PREPUSH_ELSEWHERE=$elsewhere") ;;
    plan) ;;
    *) vars+=("PREPUSH_ONLY=$share") ;;
esac

if [ "$what" = env ]; then printf '%s\n' "${vars[@]}"; exit 0; fi
# the fast-only and verify-only sets are the sbt check and the linters, all the rest's: nothing for another share
# (a verifyareas- round's identity share aside: the area's own checks)
if [ "$area_mode" != --checks-only ] && [ "$share" != rest ] && [ "$share" != plan ]; then echo; exit 0; fi
listed="$(env "${vars[@]}" PREPUSH_LIST=1 "$(dirname "$0")/../hooks/pre-push" "$area_mode" "$range")"
if [ -n "$areas" ] && [ "$share" = plan ]; then # the plan names the area the identity share runs
    listed+=$'\n'"$(env "PREPUSH_MODE=--checks-only" "PREPUSH_RANGE=$range" PREPUSH_ONLY=identity PREPUSH_LIST=1 \
        "$(dirname "$0")/../hooks/pre-push" --checks-only "$range")"
fi
printf '%s\n' "$listed" | { grep -v '^$' || true; } | tr '\n' ' ' | sed 's/ *$//'
echo
