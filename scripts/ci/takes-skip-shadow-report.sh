#!/usr/bin/env bash
# THE SHADOW'S TALLY (scripts/ci/takes-skip-gate.sh): every `takes skip gate (<cc>)` status the takes rows posted on the
# newest <commits> commits of a ref (default main, 300) -- how often the gate said SKIP, how often the takes then moved
# (a DISAGREEMENT: the gate was wrong, and must never be enforced), and how often it said RUN. Enforcing the gate
# (the convergence workflows' `takes-skip: enforce`) waits for at least 20 SKIPs and none disagreeing.
#
# usage: takes-skip-shadow-report.sh [ref] [commits]     env: GH_REPO (owner/repo), GH_TOKEN
# Prints one line a status, then the tally; exit 0 when there are >= 20 SKIPs and 0 disagreements (ready to enforce),
# 1 otherwise, 2 when the statuses could not be read (a read that fails is never a clean tally).
set -uo pipefail
ref=${1:-main} want=${2:-300}
[ -n "${GH_REPO:-}" ] || { echo "takes-skip-shadow-report: GH_REPO names no repository" >&2; exit 2; }
page=1 seen=0 skip=0 disagree=0 run=0
while [ "$seen" -lt "$want" ]; do
  shas=$(timeout 60 gh api "repos/$GH_REPO/commits?sha=$ref&limit=50&per_page=50&page=$page" --jq '.[].sha') \
    || { echo "takes-skip-shadow-report: could not list $ref's commits (page $page)" >&2; exit 2; }
  [ -n "$shas" ] || break
  while read -r sha; do
    [ "$seen" -lt "$want" ] || break
    seen=$((seen + 1))
    lines=$(timeout 60 gh api "repos/$GH_REPO/commits/$sha/statuses?limit=100&per_page=100" \
        --jq '.[] | select(.context | startswith("takes skip gate (")) | "\(.context)\t\(.state)\t\(.description)"') \
      || { echo "takes-skip-shadow-report: could not read ${sha:0:9}'s statuses" >&2; exit 2; }
    # a context's newest status first: one line per context
    while IFS=$'\t' read -r context state description; do
      [ -n "$context" ] || continue
      echo "${sha:0:9} $context $state $description"
      case "$description" in
        *"gate SKIP"*DISAGREE*) disagree=$((disagree + 1)); skip=$((skip + 1)) ;;
        *"gate SKIP"*) skip=$((skip + 1)) ;;
        *"gate RUN"*) run=$((run + 1)) ;;
      esac
    done < <(awk -F'\t' '!seen[$1]++' <<< "$lines")
  done <<< "$shas"
  page=$((page + 1))
done
echo "takes-skip-shadow-report: $seen commit(s) of $ref: gate SKIP $skip (disagreeing $disagree), gate RUN $run"
if [ "$disagree" -eq 0 ] && [ "$skip" -ge 20 ]; then
  echo "takes-skip-shadow-report: READY -- $skip SKIPs, none disagreeing: the gate may be enforced"; exit 0
fi
echo "takes-skip-shadow-report: NOT READY -- enforcing needs >= 20 SKIPs and 0 disagreements"
exit 1
