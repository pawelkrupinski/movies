#!/usr/bin/env bash
# Measure a resolver variant on GitHub's runners (.github/workflows/identity-measure.yml) and
# download its reports, one directory per variant, in the layout a local
# IdentityShadowIntegrationSpec run writes (full-<cc>-report.txt, full-<cc>-decisions.txt, ...).
#
# The variant is this worktree's diff against its merge-base with origin/main — committed and
# uncommitted changes and new files alike — sent as a patch, so no branch is ever pushed.
#
# usage: scripts/identity-measure-ci.sh <variant> [out-dir]
#   COUNTRIES=pl,uk,de,us,es  ROBUSTNESS=off  FOCUS="phrase,phrase"  BASE_ONLY=1 (measure the base, no patch)
#   RECORDING=<Record scrape fixtures run id>: replay that recording in every country, so measurements compare
#   on one input (the default, each country's pinned pair, moves whenever a new recording is pinned)
set -euo pipefail
variant=${1:?usage: identity-measure-ci.sh <variant> [out-dir]}
out=${2:-target/identity-measure/$variant}
workflow=identity-measure.yml

git fetch -q origin main
base=$(git merge-base HEAD origin/main)
patch=""
if [ -z "${BASE_ONLY:-}" ]; then
  # A throwaway index sees new files too, without touching the worktree's own index.
  index=$(mktemp); trap 'rm -f "$index"' EXIT
  GIT_INDEX_FILE=$index git read-tree HEAD
  GIT_INDEX_FILE=$index git add -A
  patch=$(GIT_INDEX_FILE=$index git diff --cached --binary "$base" | gzip -9 | base64 | tr -d '\n')
  # GitHub caps a dispatch's inputs at 65,535 characters together.
  [ "${#patch}" -lt 60000 ] || { echo "patch is ${#patch} chars after gzip+base64 — over the dispatch limit" >&2; exit 1; }
fi
echo "variant $variant: base $(git log --oneline -1 "$base"), patch ${#patch} chars"

# A measurement takes one runner per country from the 20 Main needs: wait for Main to be idle.
while [ -n "$(gh run list --workflow main.yml --limit 5 --json status --jq '.[] | select(.status != "completed") | .status')" ]; do
  echo "waiting for Main to finish before dispatching…"; sleep 30
done

since=$(date -u +%Y-%m-%dT%H:%M:%SZ)
gh workflow run "$workflow" --ref main -f variant="$variant" -f base="$base" -f patch="$patch" \
  -f countries="${COUNTRIES:-pl,uk,de,us,es}" -f robustness="${ROBUSTNESS:-off}" -f focus="${FOCUS:-}" -f recording="${RECORDING:-}"

run=""
for _ in $(seq 1 30); do
  run=$(gh run list --workflow "$workflow" --event workflow_dispatch --limit 20 \
          --json databaseId,displayTitle,createdAt \
          --jq "[.[] | select(.displayTitle == \"identity measure $variant\" and .createdAt >= \"$since\")][0].databaseId // empty")
  [ -n "$run" ] && break
  sleep 5
done
[ -n "$run" ] || { echo "dispatched, but no run named 'identity measure $variant' appeared" >&2; exit 1; }
echo "run $run: $(gh run view "$run" --json url --jq .url)"

status=0
gh run watch "$run" --exit-status --interval 60 > /dev/null || status=$?

rm -rf "$out"; mkdir -p "$out"
dl=$(mktemp -d)
# A run that failed or was cancelled before uploading has nothing to download: say so and still
# exit with the RUN's status below, rather than dying here under `set -e` with the download's.
if gh run download "$run" --pattern "identity-measure-$variant-*" --dir "$dl"; then
  for d in "$dl"/*/; do if [ -d "$d" ]; then cp -R "$d". "$out/"; fi; done
else
  echo "run $run left no reports to download" >&2
fi
rm -rf "$dl"
echo "reports in $out:"; find "$out" -maxdepth 1 -name "full-*-decisions.txt"
exit "$status"
