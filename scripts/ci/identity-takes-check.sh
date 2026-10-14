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
#   env: GH_TOKEN, GITHUB_REPOSITORY, GITHUB_SHA, MONGODB_URI, HEAP (sbt -Xmx, default 6g), TAKES_BASE (optional),
#        STATUS_CONTEXT (default "identity takes (<cc>)"), GITHUB_STEP_SUMMARY (optional),
#        TAKES_STORE (the release its resolves are stored on and reused from; empty stores nothing), GITHUB_REF,
#        the pinned pair's KINOWO_CONVERGENCE_PIN_CORPUS_RUN / _TREE_ASSET / _FILL_ASSETS, RUNNER_TEMP/laid-fill
#
# THE BASE IS USUALLY ALREADY RESOLVED. It is the newest commit whose check passed -- most often the previous run's
# head, which that run resolved over the same pinned recording minutes before. A resolve is a pure function of its
# commit and what it reads (the base-equals-head skip below rests on the same determinism), so main stores each head's
# decisions and final takes on the TAKES_STORE release, keyed by the commit and every input it read: the pinned corpus,
# its enrichment tree and fills, the fill this row laid over them, and -- for the final takes -- the capture they replay.
# A base whose key is stored is downloaded, not compiled and resolved: half of the step, 6-12 minutes of the US row
# (run 38548739769). A key not stored -- other inputs, a fill this row could not publish, a first run -- resolves as before.
# Writes <out>/{base,head}/decisions-<cc>.jsonl and ratchet-takes.tsv (the model's takes and the final ones, below),
# <out>/base.txt (the base commit) and <out>/verdict.txt.
# Exit: 0 passed, 1 an unaccepted move, 2 the check could not run -- no verdict, which the leg posts as an `error` status
# and fails on: a check that cannot compute its diff never passes.
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

# TAKES_BASE (a hand dispatch's `takes-base`) names the base outright: how to re-judge a range a check once passed
# without judging it, e.g. one the old 40-commit window absorbed.
base=${TAKES_BASE:-$(pick_base)} || { echo "::error::identity takes ($cc): could not name a base commit" | tee "$out/verdict.txt"; exit 2; }
[ -n "$base" ] || { echo "::error::identity takes ($cc): could not name a base commit" | tee "$out/verdict.txt"; exit 2; }
# A base named by hand may be abbreviated ("c2974e3aa"), and a fetch asks the server for a FULL object name: run
# 38533191835's every leg failed to fetch its short takes-base and passed unjudged. Expanded through the API first.
if ! [[ "$base" =~ ^[0-9a-f]{40}$ ]]; then
  full=$(gh api "repos/$GITHUB_REPOSITORY/commits/$base" --jq .sha) \
    || { echo "::error::identity takes ($cc): no commit $base in $GITHUB_REPOSITORY" | tee "$out/verdict.txt"; exit 2; }
  base=$full
fi
echo "$base" > "$out/base.txt"
echo "identity takes ($cc): base $base, head $head"
git fetch -q --no-tags --depth=1 origin "$base" \
  || { echo "::error::identity takes ($cc): could not fetch base $base" | tee "$out/verdict.txt"; exit 2; }

if [ "$base" = "$head" ] || git diff --quiet "$base" "$head" -- "${identity_paths[@]}"; then
  echo "identity takes ($cc): no identity code changed since $base -- nothing can have moved" | tee "$out/verdict.txt"
  exit 0
fi

# THE STORE (see the header). A fill laid over the tree but never published names nothing a later run could match:
# such a row neither reuses nor stores.
store=${TAKES_STORE:-}
laid=$(cat "${RUNNER_TEMP:-/nonexistent}/laid-fill" 2>/dev/null || true)
[ "$laid" = unpublished ] && store=""
decisions_key() { # <sha>: the decisions dump's inputs
  printf '%s\n' "$1" "$cc" "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" "${KINOWO_CONVERGENCE_TREE_ASSET:-}" \
    "${KINOWO_CONVERGENCE_FILL_ASSETS:-}" "$laid" | sha256sum | cut -c1-24
}
capture_key() { # <sha>: the final takes' inputs -- the code and the capture it replays (always the head's)
  { printf '%s\n' "$1" "$cc"; cat "$here"/test/resources/fixtures/identity-unmatched/*.json.gz \
      "$here"/test/resources/fixtures/identity-unmatched/*.inputs 2>/dev/null; } | sha256sum | cut -c1-24
}
reused() { # <asset> <file>: whether a stored result was fetched into <file>
  [ -n "$store" ] || return 1
  mkdir -p "$(dirname "$2")"
  gh release download "$store" -p "$1" -O "$2.gz" --clobber > /dev/null 2>&1 && gunzip -f "$2.gz" && [ -s "$2" ]
}
keep() { # <file> <asset>: store a head's result, on main only (the only commits a later base is), never failing the check
  [ -n "$store" ] && [ "${GITHUB_REF:-}" = refs/heads/main ] && [ -s "$1" ] || return 0
  gzip -c "$1" > "$RUNNER_TEMP/$2" \
    && "$here/scripts/ci/gh-release.sh" upload "$store" "$RUNNER_TEMP/$2" --clobber > /dev/null \
    || { echo "::warning::identity takes ($cc): could not store $2 -- the next run resolves its base again"; return 0; }
  # Only the newest few of a kind are ever a base again; the rest go (best effort -- one left is pruned next time).
  local kind=${2#takes-}; kind=${kind%%-*}
  # allow-silenced: pruning is housekeeping -- a listing that fails to read prunes nothing now and everything next time.
  gh release view "$store" --json assets --jq ".assets | map(select(.name | startswith(\"takes-$kind-$cc-\"))) \
      | sort_by(.createdAt) | reverse | .[8:][] | .name" 2>/dev/null \
    | while read -r stale; do "$here/scripts/ci/gh-release.sh" delete-asset "$store" "$stale" -y > /dev/null || true; done
}
asset() { echo "takes-$1-$cc-$2.gz"; } # <decisions|final> <key>

SECONDS=0
dump "$here" "$out/head" || { echo "::warning::identity takes ($cc): the head resolved nothing"; exit 2; }
echo "identity takes ($cc): head resolved in ${SECONDS}s"
keep "$out/head/decisions-$cc.jsonl" "$(asset decisions "$(decisions_key "$head")")"

SECONDS=0
tree="$RUNNER_TEMP/identity-takes-base"
base_tree() { # the base checked out beside the head, once, for whichever of its resolves is not stored
  [ -d "$tree" ] && return 0
  git worktree add -q --detach "$tree" "$base" || return 1
  # The head's compiled classes and zinc state, so the base compiles only what differs between them.
  # The sbt-target-cache's paths (.github/actions/sbt-target-cache): classes and analysis, no jars.
  for d in project/target project/project/target */target/scala-*; do
    [ -d "$here/$d" ] && mkdir -p "$tree/$d" && cp -a "$here/$d/." "$tree/$d/"
  done
  return 0
}
rm -rf "$tree"
if reused "$(asset decisions "$(decisions_key "$base")")" "$out/base/decisions-$cc.jsonl"; then
  echo "identity takes ($cc): base resolve reused from $(asset decisions "$(decisions_key "$base")") in ${SECONDS}s"
else
  base_tree || exit 2
  dump "$tree" "$out/base" || { echo "::warning::identity takes ($cc): the base resolved nothing"; exit 2; }
  echo "identity takes ($cc): base compiled and resolved in ${SECONDS}s"
fi

# THE FINAL TAKES: the decisions above are the MODEL's, before the agreement stage reads them again -- 9c8170f5e's range
# moved 213 UK listings in prod (and three right DE takes) with them at 0/0/0. The unmatched-cluster ratchet replays the
# model AND the agreement stage over the checked-in capture and its families' answers; run by the head and by the base
# over the head's capture, its takes are diffed as the decisions are. The base reads the head's capture: one input.
ratchet() { # <checkout> <out file>
  rm -f "$1/target/identity-unmatched/ratchet-takes.tsv"
  ( cd "$1" && sbt -J-Xmx"$heap" "worker/Test/testOnly services.identity.UnmatchedClustersRatchetSpec" > "$2.log" 2>&1 )
  cp "$1/target/identity-unmatched/ratchet-takes.tsv" "$2" 2>/dev/null
}
SECONDS=0
final=0
ratchet "$here" "$out/head/ratchet-takes.tsv" || final=2
[ "$final" = 0 ] && keep "$out/head/ratchet-takes.tsv" "$(asset final "$(capture_key "$head")")"
if [ "$final" = 0 ] && reused "$(asset final "$(capture_key "$base")")" "$out/base/ratchet-takes.tsv"; then
  echo "identity takes ($cc): base final takes reused"
elif [ "$final" = 0 ]; then
  base_tree || exit 2
  cp "$here"/test/resources/fixtures/identity-unmatched/*.json.gz "$here"/test/resources/fixtures/identity-unmatched/*.inputs \
     "$tree/test/resources/fixtures/identity-unmatched/" 2>/dev/null
  ratchet "$tree" "$out/base/ratchet-takes.tsv" || final=2
fi
echo "identity takes ($cc): final takes replayed in ${SECONDS}s"
[ -d "$tree" ] && { git worktree remove --force "$tree" || true; }

diff_takes() { # <base> <head> [--final]
  python3 "$here/scripts/ci/identity_takes_diff.py" "$cc" "$1" "$2" "${@:3}" \
    --labels "$here/test/resources/fixtures/identity-unmatched/labels.tsv" \
    --accepted "$here/test/resources/fixtures/identity-takes/accepted.tsv" \
    --base-name "${base:0:9}" --head-name "${head:0:9}" ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"}
}
diff_takes "$out/base/decisions-$cc.jsonl" "$out/head/decisions-$cc.jsonl" | tee "$out/verdict.txt"
model=${PIPESTATUS[0]}
if [ "$final" = 0 ]; then
  diff_takes "$out/base/ratchet-takes.tsv" "$out/head/ratchet-takes.tsv" --final | tee -a "$out/verdict.txt"
  final=${PIPESTATUS[0]}
else
  echo "::error::identity takes ($cc): the final takes could not be replayed (the ratchet wrote no takes; $out/*/ratchet-takes.tsv.log)" \
    | tee -a "$out/verdict.txt"
fi
# An unaccepted move anywhere fails; failing that, a half that could not run is no verdict (2), never a pass.
if [ "$model" = 1 ] || [ "$final" = 1 ]; then exit 1; fi
if [ "$model" != 0 ] || [ "$final" != 0 ]; then exit 2; fi
exit 0
