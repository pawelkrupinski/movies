#!/usr/bin/env bash
# THE FAMILY EXPLORER'S WALK ROW (.github/workflows/identity-family-explorer.yml; the owner, 2026-11-06, option A): one
# shard of one country's families, walked over the pinned recording the row laid. services.identity.FamilyConvergence
# walks every family's POPULATION LATTICE -- every subset of a family of 7 or fewer, the levels 0..L and n-L..n of one of
# 8..63, seeded arrival orders of a larger one -- through the model AND the final take, and names every listing some
# arrival or add/remove sequence takes A -> B -> A (docs/design/identity-no-drift.md, "The family explorer").
# NOT A GATE: it reports flips and fails only when it could not run; no commit is judged by it.
#
# THE CACHE (FAMILY_STORE, a release; empty: nothing stored or read) is one rolling asset a country and shard, under a
# rules key that names no code. A night re-walks only the families the code changed since the cache's commit can move
# (scripts/ci/family_reach.py: the what-if's rule attribution; anything it cannot attribute -- or no cache commit to
# compare with -- walks every family), and FamilyConvergence serves the rest from the cache while their reads' digests hold.
# The cache and its commit are stored only after a walk that finished, so a failed night leaves the last good pair.
#
# usage: identity-family-nightly.sh <cc> <out dir>
#   env: FAMILY_SHARD (`i/N`, default 1/1), FAMILY_LEVELS (default 1), FAMILY_STORE, HEAP (sbt -Xmx, default 6g),
#        GITHUB_SHA (the commit walked, default HEAD), GH_TOKEN.
# Writes <out>/family-flips-<cc>.tsv and family-walks-<cc>.tsv (the walks' seconds a family), sbt.log, report.txt, reach.txt.
# Exit: 0 walked, 2 it could not run. Tested by identity-family-nightly-test.sh.

set -uo pipefail

cc=${1:?country code}
out=${2:?out dir}
heap=${HEAP:-6g}
shard=${FAMILY_SHARD:-1/1}
levels=${FAMILY_LEVELS:-1}
store=${FAMILY_STORE:-}
here=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
fixtures="$here/test/resources/fixtures"
mkdir -p "$out"
[[ $shard =~ ^[0-9]+/[0-9]+$ ]] || { echo "family explorer ($cc): FAMILY_SHARD '$shard' is not i/N" >&2; exit 2; }
[ -f "$fixtures/corpus/cinema-scrapes-$cc.json.gz" ] || { echo "family explorer ($cc): no recorded corpus in $fixtures/corpus" >&2; exit 2; }
stem="family-cache-$cc-${shard%/*}of${shard#*/}-L$levels"
head_sha=${GITHUB_SHA:-$(git -C "$here" rev-parse HEAD)}

fetch() { # <asset> <file>: the asset gunzipped to the file; false when there is none
  [ -n "$store" ] || return 1
  # allow-silenced: no store, an unreadable one or a missing asset are one answer -- nothing cached, every family walks
  timeout 120 gh release view "$store" --json assets --jq '.assets[].name' 2> /dev/null | grep -qxF "$1" || return 1
  timeout 300 gh release download "$store" -p "$1" -O "$2.gz" --clobber > /dev/null 2>&1 && gunzip -f "$2.gz" && [ -s "$2" ]
}
keep() { # <file> <asset>: store it, never failing the walk
  [ -n "$store" ] && [ -s "$1" ] || return 0
  timeout 120 gh release view "$store" > /dev/null 2>&1 \
    || "$here/scripts/ci/gh-release.sh" create "$store" --prerelease --title "Identity family explorer store (rolling)" \
         --notes "The nightly family explorer's verdict caches (scripts/ci/identity-family-nightly.sh). A cache, not a version of anything." > /dev/null 2>&1 || true
  gzip -c "$1" > "${RUNNER_TEMP:-/tmp}/$2" && "$here/scripts/ci/gh-release.sh" upload "$store" "${RUNNER_TEMP:-/tmp}/$2" --clobber > /dev/null 2>&1 \
    || echo "family explorer ($cc): could not store $2 -- a later night walks it again"
}

# ---- what to walk again: the families the code changed since the cache's commit can move ----
reach_args=()
cache="$out/cache.json"
if fetch "$stem.json.gz" "$cache" && fetch "$stem.sha.gz" "$out/cache.sha"; then
  cached_sha=$(tr -d '[:space:]' < "$out/cache.sha")
  # allow-silenced: a commit not fetched leaves the reach to say ALL (it cannot compare), so every family walks again
  git -C "$here" cat-file -e "$cached_sha^{commit}" 2> /dev/null \
    || timeout 300 git -C "$here" fetch -q --no-tags --depth=1 origin "$cached_sha" 2> /dev/null || true
  if python3 "$here/scripts/ci/family_reach.py" reach --repo "$here" --base "$cached_sha" --head "$head_sha" --out "$out/reach.txt" > "$out/reach-why.txt" 2>&1; then
    reach_args=(--reach "$out/reach.txt")
  else
    echo "family explorer ($cc): the reach of ${cached_sha:0:12}..${head_sha:0:12} could not be read -- every family walks again"
    rm -f "$cache"
  fi
  cat "$out/reach-why.txt"
else
  echo "family explorer ($cc): no verdict cache stored -- every family walks"
  rm -f "$cache" "$out/cache.sha"
fi

# ---- the walk ----
walk_code=0
( cd "$here" && sbt -J-Xmx"$heap" 'set worker / Test / run / fork := false' \
    "worker/Test/runMain services.identity.FamilyConvergence --corpus $cc --recording $fixtures --families $fixtures/identity-unmatched/$cc.json.gz --shard $shard --levels $levels --rules family-explorer-L$levels --cache $cache ${reach_args[*]} --out $out" ) \
  > "$out/sbt.log" 2>&1 || walk_code=$?
grep -E '^## |^  [a-z]+	' "$out/sbt.log" > "$out/report.txt"
if [ ! -f "$out/family-flips-$cc.tsv" ] || ! grep -qE "^## $cc family check: " "$out/report.txt"; then
  tail -40 "$out/sbt.log" >&2
  echo "family explorer ($cc, shard $shard): no verdict (sbt exit $walk_code)" >&2
  exit 2
fi
grep -E '^## ' "$out/report.txt"
echo "$head_sha" > "$out/cache.sha"
keep "$cache" "$stem.json.gz"
keep "$out/cache.sha" "$stem.sha.gz"
rm -f "$cache"   # the cache is a store's, not an artifact
exit 0
