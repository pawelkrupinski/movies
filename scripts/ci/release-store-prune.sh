#!/usr/bin/env bash
# MAKE ROOM ON A ROLLING STORE RELEASE before storing one result on it: a CI cache kept as release assets (the drift
# store, identity-drift; the takes store, identity-takes) whose results are keyed by inputs, and of which only the
# newest are ever read again.
#
# GitHub refuses every upload to a release past 1000 assets. The drift store reached it on 2026-11-12 (48 results a
# kind and country, x 5 kinds x 5 countries = 1200): nothing was stored after, and every drift row resolved its base
# again (UK rows 15 -> 25 min, Identity takes 30 -> 45). The takes store stood at ~725 on 2026-11-15 on the same road.
# So a store keeps, counting the result about to be stored, PER_PREFIX of the assets named <prefix>* (one kind and
# country) and TOTAL in all, the newest by creation; the others are deleted, oldest first. TOTAL under 1000 leaves room
# for the rows of the runs storing at once.
#
# Best effort, never failing its caller: a listing that fails prunes nothing now, and the next store does it.
#
# Usage: release-store-prune.sh <release> <prefix> <per-prefix> <total>
#   RELEASE_PRUNE_TIMEOUT   seconds the listing may take (default 120)
# Test: bash scripts/ci/release-store-prune-test.sh
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
[ $# -eq 4 ] || { echo "usage: release-store-prune.sh <release> <prefix> <per-prefix> <total>" >&2; exit 2; }
release=$1 prefix=$2 per=$(( $3 - 1 )) room=$(( $4 - 1 ))
# allow-silenced: pruning is housekeeping -- a listing that fails to read prunes nothing now and everything next time
stale=$(timeout "${RELEASE_PRUNE_TIMEOUT:-120}" gh release view "$release" --json assets --jq ".assets | sort_by(.createdAt) | reverse \
    | ((map(select(.name | startswith(\"$prefix\"))) | .[$per:][]), .[$room:][]) | .name" 2> /dev/null | sort -u) || exit 0
while read -r name; do
  [ -n "$name" ] || continue
  # allow-silenced: an asset another run deleted first is gone either way
  "$here/gh-release.sh" delete-asset "$release" "$name" -y > /dev/null 2>&1 || true
done <<< "$stale"
exit 0
