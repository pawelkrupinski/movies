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
# it -- however far back that commit is. A check that has only ever failed diffs from its first run's
# base; one that never ran takes the parent.
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

# One page of the head's history before it, a line a commit: `<sha> <state of $context, or -> <parent sha>`, then
# `NEXT <cursor>` when there is more. GraphQL hands a commit's statuses with it: a page is one call, not 100.
history_page() { # [cursor]
  local after=${1:+, after: \$cursor}
  gh api graphql -f owner="${GITHUB_REPOSITORY%/*}" -f name="${GITHUB_REPOSITORY#*/}" -f expr="$head~1" \
    ${1:+-f cursor="$1"} -f query="query(\$owner: String!, \$name: String!, \$expr: String!${1:+, \$cursor: String}) {
      repository(owner: \$owner, name: \$name) { object(expression: \$expr) { ... on Commit {
        history(first: 100$after) { pageInfo { hasNextPage endCursor }
          nodes { oid parents(first: 1) { nodes { oid } } status { contexts { context state } } } } } } } }" \
    --jq ".data.repository.object.history | (.nodes[] | .oid + \" \" +
            ([(.status.contexts // [])[] | select(.context == \"$context\") | .state][0] // \"-\") + \" \" +
            (.parents.nodes[0].oid // \"-\")),
          (if .pageInfo.hasNextPage then \"NEXT \" + .pageInfo.endCursor else empty end)"
}

# The newest commit before the head whose check passed, however far back. A check that has only ever failed diffs
# from the base its first run took -- the parent of the oldest commit it judged -- so its moves stay failed until
# accepted; only a check that never ran takes the head's parent. A failed read fails the pick (exit 2 below), never
# reads as "no green commit": that would diff from the parent and quietly let an earlier unaccepted move through --
# as a 40-commit window once did, absorbing a day's moves the moment the last green commit fell out of it.
pick_base() {
  local cursor="" page sha state parent first_judged="" head_parent="" pages=0
  while [ "$pages" -lt 20 ]; do
    page=$(history_page "$cursor") || return 1
    cursor=""
    while read -r sha state parent; do
      [ "$sha" = NEXT ] && { cursor="$state${parent:+ $parent}"; continue; } # a history cursor is "<sha> <offset>"
      [ -n "$head_parent" ] || head_parent=$sha # the history of head~1 starts with it
      [ "$state" = SUCCESS ] && { echo "$sha"; return 0; }
      [ "$state" != - ] && first_judged=$parent
    done <<< "$page"
    pages=$((pages + 1))
    [ -n "$cursor" ] || break
  done
  if [ -n "$first_judged" ]; then echo "$first_judged"; return 0; fi
  echo "$head_parent" # never judged in the last 2,000 commits: the check's first run
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
