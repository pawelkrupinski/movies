#!/usr/bin/env bash
# THE IDENTITY TAKES CHECK, for one country inside its hermetic convergence row: resolve the pinned
# recording the row just replayed with the BASE commit and with the HEAD, and fail on any listing the
# code moved to another film (lost or changed) that nobody accepted (scripts/ci/identity_takes_diff.py).
#
# Base vs head on ONE recording is what makes a red verdict mean "the code did it": a new nightly
# recording changes both sides at once and so moves nothing here, where an expected-takes snapshot
# would go stale with every recording.
#
# THE BASE is the newest commit before the head whose own check passed (its commit status
# `identity takes (<cc>)` is `success`), so a failed check stays failed on every later push until
# the move is fixed or accepted -- a check against the parent alone would let the next push absorb
# it. No such commit in the last 40 (the first run) takes the parent.
#
# The base is resolved in a worktree of its own beside the checkout (the row has overwritten tracked
# corpus fixtures with the recording), its sbt seeded with the head's compiled classes so zinc
# compiles only what differs. A base whose identity code is the head's skips its resolve: nothing
# can have moved.
#
# usage: identity-takes-check.sh <cc> <out dir>
#   env: GH_TOKEN, GITHUB_REPOSITORY, GITHUB_SHA, MONGODB_URI, HEAP (sbt -Xmx, default 6g),
#        STATUS_CONTEXT (default "identity takes (<cc>)"), GITHUB_STEP_SUMMARY (optional)
# Writes <out>/{base,head}/decisions-<cc>.jsonl, <out>/base.txt (the base commit) and <out>/verdict.txt.
# Exit: 0 passed, 1 an unaccepted move, 2 the check could not run.
set -uo pipefail

cc=${1:?country code}
out=${2:?out dir}
head=${GITHUB_SHA:-$(git rev-parse HEAD)}
context=${STATUS_CONTEXT:-identity takes ($cc)}
heap=${HEAP:-6g}
here=$(pwd)
mkdir -p "$out"

# The paths whose change can move a take. Anything else -- docs, the web, workflows -- cannot.
identity_paths=(common/src/main worker/src/main worker/src/it/scala/IdentityResolveDumpIntegrationSpec.scala
                worker/src/it/scala/IdentityShadow.scala testkit/src/main build.sbt project)

# A failed read fails the pick (exit 2 below), never reads as "no green commit": that would diff from the parent
# and quietly let an earlier unaccepted move through.
pick_base() {
  local shas sha state
  shas=$(gh api "repos/$GITHUB_REPOSITORY/commits?sha=$head&per_page=41" --jq '.[1:][].sha') || return 1
  for sha in $shas; do
    state=$(gh api "repos/$GITHUB_REPOSITORY/commits/$sha/statuses?per_page=100" \
      --jq "[.[] | select(.context == \"$context\")][0].state // empty") || return 1
    if [ "$state" = success ]; then echo "$sha"; return 0; fi
  done
  gh api "repos/$GITHUB_REPOSITORY/commits/$head" --jq '.parents[0].sha'
}

dump() { # <checkout> <out dir>
  ( cd "$1" && KINOWO_IDENTITY_DUMP="$2" KINOWO_IDENTITY_FULL="$cc" \
      KINOWO_IDENTITY_CORPUS_DIR="$here/test/resources/fixtures/corpus" KINOWO_FIXTURE_ROOT="$here/test/resources/fixtures" \
      sbt -J-Xmx"$heap" -J-Duser.language=en -J-Duser.country=US \
        "worker/IntegrationTest/testOnly integration.IdentityResolveDumpIntegrationSpec" 2>&1 | tee "$2.log" | grep -E '\] \[full-|error|Total time' )
  [ -s "$2/decisions-$cc.jsonl" ]
}

base=$(pick_base) || { echo "::warning::identity takes ($cc): could not name a base commit"; exit 2; }
[ -n "$base" ] || { echo "::warning::identity takes ($cc): could not name a base commit"; exit 2; }
echo "$base" > "$out/base.txt"
echo "identity takes ($cc): base $base, head $head"
git fetch -q --no-tags --depth=1 origin "$base" || { echo "::warning::identity takes ($cc): could not fetch base $base"; exit 2; }

if [ "$base" = "$head" ] || git diff --quiet "$base" "$head" -- "${identity_paths[@]}"; then
  echo "identity takes ($cc): no identity code changed since $base -- nothing can have moved" | tee "$out/verdict.txt"
  exit 0
fi

SECONDS=0
dump "$here" "$out/head" || { echo "::warning::identity takes ($cc): the head resolved nothing"; exit 2; }
echo "identity takes ($cc): head resolved in ${SECONDS}s"

SECONDS=0
tree="$RUNNER_TEMP/identity-takes-base"
rm -rf "$tree"
git worktree add -q --detach "$tree" "$base" || exit 2
# The head's compiled classes and zinc state, so the base compiles only what differs between them.
# The sbt-target-cache's paths (.github/actions/sbt-target-cache): classes and analysis, no jars.
for d in project/target project/project/target */target/scala-*; do
  [ -d "$here/$d" ] && mkdir -p "$tree/$d" && cp -a "$here/$d/." "$tree/$d/"
done
dump "$tree" "$out/base" || { echo "::warning::identity takes ($cc): the base resolved nothing"; exit 2; }
echo "identity takes ($cc): base compiled and resolved in ${SECONDS}s"
git worktree remove --force "$tree" || true

python3 "$here/scripts/ci/identity_takes_diff.py" "$cc" "$out/base/decisions-$cc.jsonl" "$out/head/decisions-$cc.jsonl" \
  --labels "$here/test/resources/fixtures/identity-unmatched/labels.tsv" \
  --accepted "$here/test/resources/fixtures/identity-takes/accepted.tsv" \
  --base-name "${base:0:9}" --head-name "${head:0:9}" ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"} | tee "$out/verdict.txt"
exit "${PIPESTATUS[0]}"
