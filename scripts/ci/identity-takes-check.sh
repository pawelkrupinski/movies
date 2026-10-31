#!/usr/bin/env bash
# THE IDENTITY TAKES CHECK, for one country inside its hermetic convergence row: resolve the pinned
# recording the row just replayed with the BASE commit and with the HEAD, and fail on any listing the
# code moved to another film (lost or changed) that nobody accepted, or onto a suspect take nobody
# judged (scripts/ci/identity_takes_diff.py); and run the corpus-shape specs over the same recording at
# both, failing on a finding only the head has (scripts/ci/corpus_shape_diff.py).
#
# Base vs head on ONE recording is what makes a red verdict mean "the code did it": a new nightly
# recording changes both sides at once and so moves nothing here, where an expected-takes snapshot
# would go stale with every recording. What the recording itself brings -- a suspect take, a marker
# left in a query -- stands at base and head alike: listed on the summary, never failed, and triaged
# from the Identity findings report (.github/workflows/identity-findings-report.yml).
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
#        the pinned pair's KINOWO_CONVERGENCE_PIN_CORPUS_RUN / _TREE_ASSET / _FILL_ASSETS, RUNNER_TEMP/laid-fill,
#        SHAPE_COMMAND (the country's `corpusShape*` sbt alias; empty checks no corpus shape),
#        TAKES_SIDE (both, head or base: THE BASE ROW below), TAKES_BASE_JOB (the base row's job name), GITHUB_RUN_ID,
#        TAKES_BASE_WAIT / TAKES_BASE_POLL (seconds: how long, and how often, the head waits for the base row)
#
# THE BASE IS USUALLY ALREADY RESOLVED. It is the newest commit whose check passed -- most often the previous run's
# head, which that run resolved over the same pinned recording minutes before. A resolve is a pure function of its
# commit and what it reads (the base-equals-head skip below rests on the same determinism), so main stores each head's
# decisions and final takes on the TAKES_STORE release, keyed by the commit's identity code and every input it read: the pinned corpus,
# its enrichment tree and fills, the fill this row laid over them, and -- for the final takes -- the capture they replay.
# A base whose key is stored is downloaded, not compiled and resolved: half of the step, 6-12 minutes of the US row
# (run 38548739769). A key not stored -- other inputs, a fill this row could not publish, a first run -- resolves as before.
# Each head's corpus-shape findings are stored the same way, keyed by the checks' code and allowlists and the recording.
# Writes <out>/{base,head}/decisions-<cc>.jsonl and ratchet-takes.tsv (the model's takes and the final ones, below),
# <out>/{base,head}/shape/ (the corpus-shape findings), <out>/base.txt (the base commit), <out>/recording.txt and <out>/verdict.txt.
# THE BASE ROW. Neither side's results stored -- a base main has not run yet (a measurement dispatched while main's
# own run for its base is still queued), or a recording pinned since -- the base was resolved AFTER the head's session,
# on the same runner: 5.5 of run 38735006233's 13.8-minute US row. A takes-only dispatch (scripts/identity-measure)
# runs it on a runner of its own instead: its `takes-base` row (TAKES_SIDE=base) stores the base's side -- downloaded
# when stored, else resolved -- while the `takes` row (TAKES_SIDE=head, TAKES_BASE_JOB naming that row's job) runs the
# head's and then waits for each base result to be stored, resolving one itself only when that row ended, or never
# started, without storing it. TAKES_SIDE unset (both) is the one-row check every hermetic leg runs.
# Exit: 0 passed, 1 an unaccepted move or a new suspect or corpus-shape finding, 2 the check could not run -- no verdict, which the leg posts as an `error` status
# and fails on: a check that cannot compute its diff never passes.
set -uo pipefail

cc=${1:?country code}
out=${2:?out dir}
head=${GITHUB_SHA:-$(git rev-parse HEAD)}
context=${STATUS_CONTEXT:-identity takes ($cc)}
heap=${HEAP:-6g}
side=${TAKES_SIDE:-both}
case $side in both|head|base) ;; *) echo "::error::identity takes ($cc): TAKES_SIDE is both, head or base, not $side"; exit 2 ;; esac
[ "$side" = head ] && [ -z "${TAKES_BASE_JOB:-}" ] && side=both # no base row to wait for
here=$(pwd)
mkdir -p "$out"
out=$(cd "$out" && pwd) # absolute: every sbt run below runs in a checkout of its own
# The runner's scratch directory (the base's checkouts, the stored results' gzips); outside Actions one of this run's
# own, removed at exit with what it holds.
own_temp=""
[ -n "${RUNNER_TEMP:-}" ] || { RUNNER_TEMP=$(mktemp -d) || exit 2; own_temp=$RUNNER_TEMP; }
base_final_pid=""
# shellcheck disable=SC2329 # run by the EXIT trap below
on_exit() {
  # an exit before base_final_finish's wait leaves no replay behind (best effort)
  [ -z "$base_final_pid" ] || kill "$base_final_pid" 2> /dev/null
  [ -z "$own_temp" ] || { rm -rf "$own_temp"; git worktree prune 2> /dev/null; }
  return 0
}
trap on_exit EXIT
# shellcheck source=scripts/identity-tools.sh
. "$(dirname "${BASH_SOURCE[0]}")/../identity-tools.sh" # IDENTITY_SBT, and tool-conventions.sh's `bounded`
# Every gh call is bounded: a hung API read is no verdict (exit 2), never a row that waits out the job's own timeout.
gh_read=${IDENTITY_TAKES_GH_TIMEOUT:-120}
gh_download=${IDENTITY_TAKES_DOWNLOAD_TIMEOUT:-600}

# The paths whose change can move a take. Anything else -- docs, the web, workflows -- cannot.
# (IdentityResolveDumpIntegrationSpec: the resolve as a spec, before it was a tool -- still a base's)
identity_paths=(geodata/src/main common/src/main identity/src/main scrapers/src/main worker/src/main worker/src/it/scala/IdentityResolveDump.scala
                worker/src/it/scala/IdentityResolveDumpIntegrationSpec.scala worker/src/it/scala/IdentityShadow.scala testkit/src/main
                build.sbt project)

# THE HISTORY FLOOR (github.com only): a status older than .github/github-takes-history-floor counts as none. github.com's
# checks stopped on 2026-10-16 (the suspension) and came back in shadow on 2026-10-31 while Forgejo judged every commit
# between, so the newest green github.com status was 1,429 commits back (run 41209071921: every takes row failed on
# moves Forgejo had accepted one commit at a time). Past the floor github.com picks its base as a check that never ran
# would -- the head's parent -- until its own statuses take over. Forgejo reads no floor. TAKES_HISTORY_FLOOR overrides.
history_floor=""
if [ "${GITHUB_SERVER_URL:-}" = "https://github.com" ]; then
  history_floor=${TAKES_HISTORY_FLOOR-$(grep -vE '^\s*(#|$)' "$here/.github/github-takes-history-floor" 2>/dev/null | head -1)}
fi
[ -z "$history_floor" ] || [[ "$history_floor" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$ ]] \
  || { echo "::error::identity takes ($cc): the history floor is an ISO time like 2026-10-31T15:00:00Z, not $history_floor"; exit 2; }

# One page of the head's history before it, a line a commit: `<sha> <state of $context, or -> <parent sha>`, then
# `NEXT <cursor>` when there is more. GraphQL hands a commit's statuses with it: a page is one call, not 100.
history_page() { # [cursor]
  local after=${1:+, after: \$cursor}
  bounded "$gh_read" gh api graphql -f owner="${GITHUB_REPOSITORY%/*}" -f name="${GITHUB_REPOSITORY#*/}" -f expr="$head~1" \
    ${1:+-f cursor="$1"} -f query="query(\$owner: String!, \$name: String!, \$expr: String!${1:+, \$cursor: String}) {
      repository(owner: \$owner, name: \$name) { object(expression: \$expr) { ... on Commit {
        history(first: 100$after) { pageInfo { hasNextPage endCursor }
          nodes { oid parents(first: 1) { nodes { oid } } status { contexts { context state createdAt } } } } } } } }" \
    --jq ".data.repository.object.history | (.nodes[] | .oid + \" \" +
            ([(.status.contexts // [])[] | select(.context == \"$context\" and (.createdAt // \"\") >= \"$history_floor\") | .state][0] // \"-\") + \" \" +
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

# THE NORMALIZER TRACE (scripts/identity-measure --footprint): every title reading a side's resolve and its ratchet made
# (services.movies.NormalizerTrace), stored per side's identity code and recording (trace_asset): a title-rule change
# reading every one of them alike moves no take, which the footprint measure proves on the laptop in minutes instead of
# dispatching this check. Asked only of a tool that takes --trace: a base, or a measured commit, from before it does not.
traces="$RUNNER_TEMP/identity-trace"
traced() { # <tool's source> <trace file> -> " --trace <file>" when the tool takes one and a file is named
  [ -n "$2" ] && grep -q '"trace"' "$1" 2> /dev/null && printf ' --trace %s' "$2"
  return 0
}

# The resolve of one checkout over the recording: its exit and its decisions both say it ran. A checkout from before the
# resolve was a tool (a base, often) runs it as the spec it was, configured by its environment.
dump() { # <checkout> <out dir> [trace file]
  local corpus="$here/test/resources/fixtures/corpus" fixtures="$here/test/resources/fixtures" status
  if [ -f "$1/worker/src/it/scala/IdentityResolveDump.scala" ]; then
    ( cd "$1" && "${IDENTITY_SBT[@]}" -J-Xmx"$heap" \
        "worker/IntegrationTest/runMain integration.IdentityResolveDump --out $2 --full $cc --corpus-dir $corpus --fixtures $fixtures$(traced "$1/worker/src/it/scala/IdentityResolveDump.scala" "${3:-}")" 2>&1 \
        | tee "$2.log" | grep -E '\[full-|error|Total time'; exit "${PIPESTATUS[0]}" )
  else
    ( cd "$1" && KINOWO_IDENTITY_DUMP="$2" KINOWO_IDENTITY_FULL="$cc" KINOWO_IDENTITY_CORPUS_DIR="$corpus" KINOWO_FIXTURE_ROOT="$fixtures" \
        "${IDENTITY_SBT[@]}" -J-Xmx"$heap" "worker/IntegrationTest/testOnly integration.IdentityResolveDumpIntegrationSpec" 2>&1 \
        | tee "$2.log" | grep -E '\[full-|error|Total time'; exit "${PIPESTATUS[0]}" )
  fi
  status=$?
  [ "$status" = 0 ] || { echo "::warning::identity takes ($cc): the resolve exited $status (its log: $2.log)"; return 1; }
  [ -s "$2/decisions-$cc.jsonl" ]
}

# TAKES_BASE (a hand dispatch's `takes-base`) names the base outright: how to re-judge a range a check once passed
# without judging it, e.g. one the old 40-commit window absorbed.
base=${TAKES_BASE:-$(pick_base)} || { echo "::error::identity takes ($cc): could not name a base commit" | tee "$out/verdict.txt"; exit 2; }
[ -n "$base" ] || { echo "::error::identity takes ($cc): could not name a base commit" | tee "$out/verdict.txt"; exit 2; }
# A base named by hand may be abbreviated ("c2974e3aa"), and a fetch asks the server for a FULL object name: run
# 38533191835's every leg failed to fetch its short takes-base and passed unjudged. Expanded through the API first.
if ! [[ "$base" =~ ^[0-9a-f]{40}$ ]]; then
  full=$(bounded "$gh_read" gh api "repos/$GITHUB_REPOSITORY/commits/$base" --jq .sha) \
    || { echo "::error::identity takes ($cc): no commit $base in $GITHUB_REPOSITORY" | tee "$out/verdict.txt"; exit 2; }
  base=$full
fi
echo "$base" > "$out/base.txt"
# Which recording the takes are over: `named <run>` for a dispatch's `recording` (convergence-setup.sh's bare pair), else
# `pinned <run>` -- scripts/identity-measure reuses only a pinned one's run for a later measurement.
echo "${KINOWO_CONVERGENCE_NAMED_RECORDING:+named $KINOWO_CONVERGENCE_NAMED_RECORDING}${KINOWO_CONVERGENCE_NAMED_RECORDING:-pinned ${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}}" > "$out/recording.txt"
echo "identity takes ($cc): base $base, head $head"
bounded "$gh_download" git fetch -q --no-tags --depth=1 origin "$base" \
  || { echo "::error::identity takes ($cc): could not fetch base $base" | tee "$out/verdict.txt"; exit 2; }

unchanged() { [ "$base" = "$head" ] || git diff --quiet "$base" "$head" -- "$@"; } # <paths...>
# THE CORPUS SHAPE, judged the same way: the corpus-shape specs (SearchQueryMarkersSpec, CinemaSlotInvariantsSpec: the
# `corpusShape*` alias the leg hands in as SHAPE_COMMAND) over this recording at the base and the head, and only a
# finding the head made fails (scripts/ci/corpus_shape_diff.py). What can move one: the identity code (title rules, the
# slot builder), the checks and their allowlists. Anything else cannot, and then neither side is run.
shape_paths=("${identity_paths[@]}" e2e/src/test/scala/services/movies worker/src/fixtures)
shape_command=${SHAPE_COMMAND:-}
if [ -n "$shape_command" ] && ! unchanged "${shape_paths[@]}"; then shape_moves=1; else shape_moves=""; fi

if unchanged "${identity_paths[@]}" && [ -z "$shape_moves" ]; then
  echo "identity takes ($cc): no identity code changed since $base, nor a corpus-shape check or allowlist -- nothing can have moved" | tee "$out/verdict.txt"
  exit 0
fi
if unchanged "${identity_paths[@]}"; then takes_moves=""; else takes_moves=1; fi

# THE STORE (see the header). A fill laid over the tree but never published names nothing a later run could match:
# such a row neither reuses nor stores.
store=${TAKES_STORE:-}
laid=$(cat "$RUNNER_TEMP/laid-fill" 2>/dev/null || true)
[ "$laid" = unpublished ] && store=""
# A key names the commit by its identity code, not its sha: a commit touching none of it (whose own check stored nothing,
# above) is then the base of the next run under the key its last identity change stored -- run 38599662543's base had.
code_of() { git ls-tree -r "$1" -- "${identity_paths[@]}" | sha256sum | cut -c1-24; } # <sha>
decisions_key() { # <sha>: the decisions dump's inputs
  printf '%s\n' "$(code_of "$1")" "$cc" "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" "${KINOWO_CONVERGENCE_TREE_ASSET:-}" \
    "${KINOWO_CONVERGENCE_FILL_ASSETS:-}" "$laid" | sha256sum | cut -c1-24
}
capture_key() { # <sha>: the final takes' inputs -- the code and the country's capture it replays (always the head's)
  { printf '%s\n' "$(code_of "$1")" "$cc"; cat "$here/test/resources/fixtures/identity-unmatched/$cc.json.gz" \
      "$here/test/resources/fixtures/identity-unmatched/$cc.inputs" 2>/dev/null; } | sha256sum | cut -c1-24
}
fetch_stored() { # <asset> <file>: whether the stored result was fetched into <file>, quietly
  [ -n "$store" ] && [ -n "$1" ] || return 1
  mkdir -p "$(dirname "$2")"
  bounded "$gh_download" gh release download "$store" -p "$1" -O "$2.gz" --clobber > /dev/null 2>&1 && gunzip -f "$2.gz" && [ -s "$2" ]
}
reused() { # <asset> <file> <what>: whether a stored result was fetched into <file>; says why not when it was not
  local why=""
  how=reused
  fetch_stored "$1" "$2" && return 0
  if [ -z "$store" ]; then why="no store (TAKES_STORE empty, or a fill laid unpublished)"
  elif [ -z "$1" ]; then why="no key (no recording pinned)"
  else why="$1 not on release $store"
  fi
  echo "identity takes ($cc): $3 not stored -- $why: resolving it"
  return 1
}
# THE BASE ROW (TAKES_SIDE=head, the header): this run's `takes-base` row resolves and stores the base's side while this
# row runs the head's, and this row waits for each result to be stored -- for as long as that row is RUNNING. One done
# (or failed) without storing it, or still queued for a runner when this row needs it, leaves it to this row.
base_row_state() { # -> the takes-base row's job status (queued, in_progress, completed, ...), empty when there is none
  bounded "$gh_read" gh api "repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID/jobs?per_page=100" \
    --jq '.jobs[] | select(.name | endswith(env.TAKES_BASE_JOB)) | .status' # `takes-base (<country>)`, github.com prefixing `<country> / `
}
awaited() { # <asset> <file> <what>: reused, or -- beside a base row -- stored by it meanwhile; sets $how
  [ "$side" = head ] && [ -n "$store" ] && [ -n "$1" ] || { reused "$@"; return; }
  local started state deadline
  started=$(date +%s); deadline=$((started + ${TAKES_BASE_WAIT:-1800}))
  how=reused
  while :; do
    fetch_stored "$1" "$2" && { echo "identity takes ($cc): $3 $how ($1, after $(( $(date +%s) - started ))s)"; return 0; }
    how=awaited
    state=$(base_row_state) || state=unread # a failed read is no answer: wait on, until the deadline
    case $state in
      in_progress|unread) ;;
      # done meanwhile: what it stored just before is there now, or never will be
      completed) fetch_stored "$1" "$2" && { echo "identity takes ($cc): $3 awaited ($1)"; return 0; }
                 echo "identity takes ($cc): $3 not stored by the takes-base row (completed) -- resolving it"; return 1 ;;
      *) echo "identity takes ($cc): $3 not stored, and the takes-base row is ${state:-absent} -- resolving it"; return 1 ;;
    esac
    [ "$(date +%s)" -lt "$deadline" ] || { echo "identity takes ($cc): $3 not stored after $((deadline - started))s -- resolving it"; return 1; }
    sleep "${TAKES_BASE_POLL:-15}"
  done
}
keep() { # <file> <asset> [base]: store a result, never failing the check
  # A head's only on main: the only commits a later base is. A BASE's on any ref -- a measure/ branch's base is main's
  # commit, and a key names the code and every input, never the ref that resolved them -- so the next measurement of the
  # same base (five agents measuring against one origin/main) downloads it instead of compiling and resolving it again.
  [ -n "$store" ] && [ -n "$2" ] && { [ "${3:-}" = base ] || [ "${GITHUB_REF:-}" = refs/heads/main ]; } && [ -s "$1" ] || return 0
  # Measurements running side by side resolve one base at once and race to store it: a refused upload of an asset the
  # release holds by then is the race lost to an equal result (the key names every input), not a store missing.
  if ! { gzip -c "$1" > "$RUNNER_TEMP/$2" && "$here/scripts/ci/gh-release.sh" upload "$store" "$RUNNER_TEMP/$2" --clobber > /dev/null; }; then
    # allow-silenced: a read that fails is no proof the asset is there, and falls through to the warning below.
    if bounded "$gh_read" gh release view "$store" --json assets --jq ".assets[] | select(.name == \"$2\") | .name" 2> /dev/null | grep -x "$2" > /dev/null; then # all of it: -q would leave gh a broken pipe, which pipefail counts
      echo "identity takes ($cc): $2 was stored by another run meanwhile"
    else
      echo "::warning::identity takes ($cc): could not store $2 -- the next run resolves its base again"
    fi
    return 0
  fi
  # Only the newest of a kind are ever a base again; the rest go (best effort -- one left is pruned next time). 24 a kind
  # and country, not 8: with ~10 agents measuring against main heads hours apart, 8 held about two hours of bases and
  # the next measurement of an older base resolved it again (2026-10-22). The largest, the US's decisions, is ~13 MB.
  local kind=${2#takes-}; kind=${kind%%-*}
  # allow-silenced: pruning is housekeeping -- a listing that fails to read prunes nothing now and everything next time.
  bounded "$gh_read" gh release view "$store" --json assets --jq ".assets | map(select(.name | startswith(\"takes-$kind-$cc-\"))) \
      | sort_by(.createdAt) | reverse | .[24:][] | .name" 2>/dev/null \
    | while read -r stale; do "$here/scripts/ci/gh-release.sh" delete-asset "$store" "$stale" -y > /dev/null || true; done
}
# <decisions|final|shape> <key>: none for a result over the recording when no recording is pinned -- its key would name
# no data, and match another unpinned run's over other data. The final takes read only the checked-in capture.
# The capture's gaps (`gaps`) are the final takes' companion, under their key.
asset() { [ "$1" = final ] || [ "$1" = gaps ] || [ -n "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" ] || return 0; echo "takes-$1-$cc-$2.gz"; }
# A side's final takes stored, its capture gaps FIRST: a run that finds the final takes stored finds their gaps too (the
# takes row polls for what its base row stores). No gaps file is no gaps, and stores none.
keep_final() { # <side> <sha> [base]
  [ -f "$out/$1/capture-gaps.tsv" ] && keep "$out/$1/capture-gaps.tsv" "$(asset gaps "$(capture_key "$2")")" ${3:+"$3"}
  keep "$out/$1/ratchet-takes.tsv" "$(asset final "$(capture_key "$2")")" ${3:+"$3"}
}
# After a side's final takes were fetched: their capture gaps, when stored (a final stored before the gaps were has none
# to fetch, and the side's blind-spot line is missing -- information only, never the verdict).
final_gaps() { # <side> <sha>
  rm -f "$out/$1/capture-gaps.tsv"
  fetch_stored "$(asset gaps "$(capture_key "$2")")" "$out/$1/capture-gaps.tsv" || rm -f "$out/$1/capture-gaps.tsv"
  return 0
}
# <sha>: a side's normalizer trace (THE NORMALIZER TRACE), named so the laptop can find it without this row's inputs: the
# identity code (code_of) and the recording -- p<run> the pinned pair, n<run> a named one (the bare pair) -- and not its
# fills, which add answers but never take one away: the newest trace of a code over a recording stands for it.
trace_asset() {
  if [ -n "${KINOWO_CONVERGENCE_NAMED_RECORDING:-}" ]; then echo "takes-trace-$cc-$(code_of "$1")-n$KINOWO_CONVERGENCE_NAMED_RECORDING.gz"
  elif [ -n "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" ]; then echo "takes-trace-$cc-$(code_of "$1")-p$KINOWO_CONVERGENCE_PIN_CORPUS_RUN.gz"; fi
}
# <sha>: the same trace under the side's EXACT inputs -- its decisions' key (identity code, recording, tree, fills, the fill
# laid) and its final takes' (the capture) -- for THE SKIP GATE below, whose proof stands only for the readings a resolve
# over these very inputs made: a fill since adds answers, and readings no trace holds. None without a pinned recording.
exact_trace_asset() {
  local decisions; decisions=$(asset decisions "$(decisions_key "$1")")
  [ -n "$decisions" ] || return 0
  echo "takes-tracex-$cc-$(printf '%s\n' "$decisions" "$(capture_key "$1")" | sha256sum | cut -c1-24).gz"
}
keep_trace() { # <side> <sha> [base]: both of the side's traces, as one, stored -- one alone proves nothing
  [ -s "$traces/$1-model.jsonl.gz" ] && [ -s "$traces/$1-final.jsonl.gz" ] || return 0
  cat "$traces/$1-model.jsonl.gz" "$traces/$1-final.jsonl.gz" > "$traces/$1.jsonl.gz" # gzip members concatenated: one stream
  keep "$traces/$1.jsonl.gz" "$(trace_asset "$2")" ${3:+"$3"}
  keep "$traces/$1.jsonl.gz" "$(exact_trace_asset "$2")" ${3:+"$3"}
}

tree="$RUNNER_TEMP/identity-takes-base"
# The base's final takes replay beside its resolve when no head session runs (the base row, or a takes row resolving the
# base itself), and two sbt runs in one checkout compile into one target: that replay gets a checkout of its own.
final_tree=$tree
[ "$side" = both ] || final_tree="$tree-final"
base_tree() { # [dir]: the base checked out beside the head, once, for whichever of its results is not stored
  local dir=${1:-$tree}
  [ -d "$dir" ] && return 0
  git worktree add -q --detach "$dir" "$base" || return 1
  # The head's compiled classes and zinc state, so the base compiles only what differs between them.
  # The sbt-target-cache's paths (.github/actions/sbt-target-cache): classes and analysis, no jars.
  for d in project/target project/project/target */target/scala-*; do
    [ -d "$here/$d" ] && mkdir -p "$dir/$d" && cp -a "$here/$d/." "$dir/$d/"
  done
  return 0
}
# The base ratchet's heap, beside the head's session: a country's capture replays in 1.5g (Germany's, the largest), every
# country's (a base from before --countries) in less than 3g.
ratchet_heap=${TAKES_RATCHET_HEAP:-3g}
# <phase> <how>: the seconds since SECONDS was last reset, as a row of <out>/timings.tsv (uploaded with the check's
# output) -- scripts/identity-measure prints them per country, so a slow measurement says which phase was slow and
# whether a stored result spared it.
timing() { printf '%s\t%s\t%s\n' "$1" "$SECONDS" "$2" >> "$out/timings.tsv"; }
# An earlier run that ended before removing its base checkouts left them registered: their directories go, and so
# does their registration (a missing but registered worktree refuses the next `git worktree add`).
rm -rf "$tree" "$final_tree" "$traces"
mkdir -p "$traces"
git worktree prune
: > "$out/verdict.txt"
printf 'phase\tseconds\thow\n' > "$out/timings.tsv"
model=0 final=0 shape=0

# THE FINAL TAKES: the resolve's decisions are the MODEL's, before the agreement stage reads them again -- 9c8170f5e's
# range moved 213 UK listings in prod (and three right DE takes) with them at 0/0/0. The unmatched-cluster ratchet replays
# the model AND the agreement stage over the checked-in capture and its families' answers; run by the head and by the base
# over the head's capture, its takes are diffed as the decisions are. The base reads the head's capture: one input.
# A lookup the capture lacks -- one a change newly asks (a relay's season search, a Rotten Tomatoes record) -- is read
# as UNKNOWN here (the ratchet's --measure), as identity-slice reads a recording gap, and listed: failed as a stale
# capture, it left EVERY country without final takes (run 38694072183: one UK RT record). The ratchet itself, outside
# a measurement, still fails a capture that lacks them. Measuring, its exit is whether it wrote the takes -- their diff,
# below, is the verdict. Only this row's country is replayed (--countries): the diff reads only its takes, and the other
# four captures were most of every row's replay.
corpus="$here/test/resources/fixtures/corpus" fixtures="$here/test/resources/fixtures"
ratchet_out() { # <checkout> <out file>: what a ratchet run in <checkout> wrote, beside <out file>
  cp "$1/target/identity-unmatched/capture-gaps.tsv" "$(dirname "$2")/capture-gaps.tsv" 2>/dev/null
  cp "$1/target/identity-unmatched/ratchet-takes.tsv" "$2" 2>/dev/null
}
# A BASE's ratchet, in the base's checkout: one from before the ratchet was a tool runs it as the spec it was, whose
# exit was its own verdict -- there the takes file alone says it ran; one from before --countries replays every capture.
ratchet() { # <checkout> <out file> [trace file]
  rm -f "$1/target/identity-unmatched/ratchet-takes.tsv" "$1/target/identity-unmatched/capture-gaps.tsv"
  mkdir -p "$(dirname "$2")"
  local tool="$1/worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala" scope=""
  grep -q '"countries"' "$tool" 2>/dev/null && scope=" --countries $cc"
  scope+=$(traced "$tool" "${3:-}")
  if [ -f "$tool" ]; then
    # Unforked, whatever the base's build says: a base from 82de60cf0 until project/ForkedTests.scala kept `run` in the
    # server forked its tools in worker/, where the repo-relative identity-judgements.tsv is missing (Forgejo run 195, every row).
    ( cd "$1" && sbt -J-Xmx"$ratchet_heap" 'set worker / Test / run / fork := false' 'set worker / Test / runMain / fork := false' \
        "worker/Test/runMain scripts.UnmatchedClustersRatchet --measure --out target/identity-unmatched$scope" > "$2.log" 2>&1 ) \
      || { echo "::warning::identity takes ($cc): the ratchet exited $? ($2.log)"; return 1; }
  else
    ( cd "$1" && KINOWO_RATCHET_CAPTURE_GAPS_UNKNOWN=1 \
        sbt -J-Xmx"$ratchet_heap" "worker/Test/testOnly services.identity.UnmatchedClustersRatchetSpec" > "$2.log" 2>&1 )
  fi
  ratchet_out "$1" "$2"
}
gaps_of() { # <side>: this country's gaps the side's final takes read as unknown, said with up to five of them
  local file="$out/$1/capture-gaps.tsv" n
  [ -f "$file" ] || return 0
  n=$(awk -F'\t' -v cc="$cc" 'NR > 1 && $1 == cc' "$file" | wc -l | tr -d ' ')
  [ "$n" -gt 0 ] || return 0
  echo "identity takes ($cc): the $1's final takes read $n lookup(s) the capture lacks as UNKNOWN -- a blind spot of this" \
    "measurement, not a failure (re-capture to close it): $(awk -F'\t' -v cc="$cc" 'NR > 1 && $1 == cc { printf "%s%s %s", sep, $2, $3; sep = "; " }' "$file" \
    | cut -c1-400)" | tee -a "$out/verdict.txt" ${GITHUB_STEP_SUMMARY:+"$GITHUB_STEP_SUMMARY"}
}
# THE CORPUS SHAPE (above): the specs' findings over the recording this row replays, at the head and at the base; a finding
# fails the specs themselves, so their exit is not the verdict -- the findings files are (no file: the specs did not run).
shape_found() { # <checkout> <out dir>: the findings a shape run in <checkout> wrote, into <out dir>
  mkdir -p "$2"
  cp "$1"/target/convergence-diagnostics/corpus-shape-recorded-"$cc"-*.txt "$2/" 2>/dev/null
}
shape_run() { # <checkout> <out dir>
  rm -f "$1"/target/convergence-diagnostics/corpus-shape-recorded-"$cc"-*.txt
  mkdir -p "$2"
  ( cd "$1" && KINOWO_IDENTITY_CORPUS_DIR="$corpus" "${IDENTITY_SBT[@]}" -J-Xmx"$heap" "$shape_command" > "$2.log" 2>&1 )
  shape_found "$1" "$2"
}
shape_key() { # <sha>: the findings' inputs -- the checks' code and allowlists, and the recording
  printf '%s\n' "$(git ls-tree -r "$1" -- "${shape_paths[@]}" | sha256sum | cut -c1-24)" "$cc" \
    "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" | sha256sum | cut -c1-24
}

# THE BASE'S SIDE, each result stored or made in the base's checkout. Its final takes replay BESIDE the head's session
# below (they read nothing the head writes, and the session leaves the runner's other cores idle most of its time); its
# resolve and its corpus shape follow the session, whose heap they could not share.
base_final_start() { # the base's final takes, replayed in the background (base_final_finish waits for them)
  base_tree "$final_tree" || exit 2
  cp "$here"/test/resources/fixtures/identity-unmatched/*.json.gz "$here"/test/resources/fixtures/identity-unmatched/*.inputs \
     "$final_tree/test/resources/fixtures/identity-unmatched/" 2>/dev/null
  base_started=$(date +%s)
  ratchet "$final_tree" "$out/base/ratchet-takes.tsv" "$traces/base-final.jsonl.gz" > "$out/base-final.out" 2>&1 &
  base_final_pid=$! # on_exit stops it should the check end before base_final_finish waits for it
}
base_final_finish() {
  [ -n "$base_final_pid" ] || return 0
  wait "$base_final_pid" || final=2
  base_final_pid=""
  cat "$out/base-final.out"
  SECONDS=$(( $(date +%s) - base_started )); timing base-final replayed
  [ "$final" = 0 ] && keep_final base "$base" base
}
base_resolve() {
  base_tree || exit 2
  dump "$tree" "$out/base" "$traces/base-model.jsonl.gz" || { echo "::warning::identity takes ($cc): the base resolved nothing"; exit 2; }
  echo "identity takes ($cc): base compiled and resolved in ${SECONDS}s"
  timing base-resolve resolved
  keep "$out/base/decisions-$cc.jsonl" "$(asset decisions "$(decisions_key "$base")")" base
}
base_shape() { # the base's corpus-shape findings into $out/base/shape (copies of the head's when the base wrote none)
  base_tree || exit 2
  if grep -q 'corpus-shape-' "$tree/e2e/src/test/scala/services/movies/CorpusShapeSpec.scala" 2>/dev/null; then
    shape_run "$tree" "$out/base/shape" || shape=2
    timing base-shape ran
    [ "$shape" = 0 ] && tar -cf "$out/base/shape.tar" -C "$out/base/shape" . \
      && keep "$out/base/shape.tar" "$(asset shape "$(shape_key "$base")")" base
  else
    # A base from before the specs wrote their findings has none to diff: the head's are reported, none failed.
    echo "identity takes ($cc): the base predates the corpus-shape findings files -- the head's are reported, not judged" \
      | tee -a "$out/verdict.txt"
    mkdir -p "$out/base/shape" && cp "$out/head/shape"/*.txt "$out/base/shape/" 2>/dev/null
  fi
}
base_shape_stored() { # <how to get it: reused|awaited>
  "$1" "$(asset shape "$(shape_key "$base")")" "$out/base/shape.tar" "the base corpus-shape findings" || return 1
  mkdir -p "$out/base/shape" && tar -xf "$out/base/shape.tar" -C "$out/base/shape"
}

# THE BASE ROW (TAKES_SIDE=base): the base's side alone, each result stored for this run's takes row, which waits for
# them (`awaited`) -- no head, no verdict. Nowhere to store them is nothing to do: the takes row resolves them itself.
if [ "$side" = base ]; then
  [ -n "$store" ] || { echo "identity takes ($cc): no store (TAKES_STORE empty, or a fill laid unpublished) -- the takes row resolves the base itself"; exit 0; }
  mkdir -p "$out/base"
  if [ -n "$takes_moves" ]; then
    SECONDS=0
    if reused "$(asset final "$(capture_key "$base")")" "$out/base/ratchet-takes.tsv" "the base final takes"; then
      echo "identity takes ($cc): base final takes reused"; timing base-final reused; final_gaps base "$base"
    else base_final_start; fi
    SECONDS=0
    if reused "$(asset decisions "$(decisions_key "$base")")" "$out/base/decisions-$cc.jsonl" "the base resolve"; then
      timing base-resolve reused
    else base_resolve; fi
    base_final_finish
    keep_trace base "$base" base
  fi
  if [ -n "$shape_moves" ]; then
    SECONDS=0
    if base_shape_stored reused; then timing base-shape reused; else base_shape; fi
  fi
  for dir in "$tree" "$final_tree"; do [ -d "$dir" ] && { git worktree remove --force "$dir" || true; }; done
  echo "identity takes ($cc): the base's side of ${base:0:9} is stored for the takes row" | tee -a "$out/verdict.txt"
  [ "$final" = 0 ] && [ "$shape" = 0 ] || exit 2
  exit 0
fi

# THE DAILY FULL ROW: every full takes diff on main refreshes `takes-full-<cc>.txt` on the store's release; enforce honours
# a SKIP only while that marker is under 24 hours old, so a gate that proved wrongly is found within a day whatever the
# lane did with the scheduled full run (convergence-daily.yml). Its age in seconds; nothing when it cannot be read.
full_marker_age() {
  [ -n "${TAKES_STORE:-}" ] || return 1
  local created
  # allow-silenced: a marker that cannot be read is no marker -- the takes then run in full (fail-closed)
  created=$(bounded "$gh_read" gh release view "$TAKES_STORE" --json assets \
      --jq ".assets[] | select(.name == \"takes-full-$cc.txt\") | .createdAt" 2> /dev/null | head -1) || return 1
  [ -n "$created" ] || return 1
  python3 -c 'import sys, datetime as d; print(int((d.datetime.now(d.timezone.utc) - d.datetime.fromisoformat(sys.argv[1].replace("Z", "+00:00"))).total_seconds()))' "$created"
}
refresh_full_marker() {
  [ -n "${TAKES_STORE:-}" ] && [ "${GITHUB_REF:-}" = refs/heads/main ] || return 0
  printf '%s %s\n' "$head" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$RUNNER_TEMP/takes-full-$cc.txt"
  # allow-silenced: a marker not refreshed only makes the next enforce run a full one
  "$here/scripts/ci/gh-release.sh" upload "$TAKES_STORE" "$RUNNER_TEMP/takes-full-$cc.txt" --clobber > /dev/null 2>&1 || true
}

# THE SKIP GATE (scripts/ci/takes-skip-gate.sh; TAKES_SKIP_MODE off, shadow or enforce): whether this commit PROVABLY
# moves no take -- a title-rule change alone, the base's every title reading (its trace under its exact inputs) read
# alike under the head's rules. Fail-closed: anything it cannot prove is RUN. SHADOW (the default on CI): the takes run
# as ever, and the gate's answer is held against them after (THE SHADOW, below). ENFORCE: a SKIP leaves out the takes
# diff -- the corpus shape and the drift check still run -- except on every TAKES_SKIP_SAMPLE_EVERY-th such commit (by its
# name, default 10), which runs in full and is held against the gate as in shadow. Never on the base row.
skip_mode=${TAKES_SKIP_MODE:-off} gate=RUN gate_line="" takes_skipped=""
case $skip_mode in shadow|enforce) [ -n "$takes_moves" ] && gate_asked=1 || gate_asked="" ;; *) gate_asked="" ;; esac
if [ -n "$gate_asked" ]; then
  mkdir -p "$out/gate"
  exact=$(exact_trace_asset "$base")
  if [ -z "$exact" ] || ! fetch_stored "$exact" "$out/gate/base-trace.jsonl.gz"; then rm -f "$out/gate/base-trace.jsonl.gz"; fi
  # (TAKES_SKIP_GATE_CMD: the self-test's stand-in for the gate)
  gate_out=$(TAKES_SKIP_MODE=$skip_mode bounded "${TAKES_SKIP_GATE_TIMEOUT:-1200}" "${TAKES_SKIP_GATE_CMD:-$here/scripts/ci/takes-skip-gate.sh}" "$base" "$head" \
      "$out/gate/base-trace.jsonl.gz" -- "$here/scripts/ci/takes-skip-proof.sh" 2> "$out/gate/reasons.txt")
  [ "$(head -1 <<< "$gate_out")" = SKIP ] && gate=SKIP gate_line=$(sed -n 2p <<< "$gate_out")
  echo "identity takes ($cc): skip gate ($skip_mode): $gate -- $(grep '^takes-skip-gate:' "$out/gate/reasons.txt" | tail -1 | sed 's/^takes-skip-gate: [A-Z]* -- //')"
  if [ "$gate" = SKIP ] && [ "$skip_mode" = enforce ]; then
    every=${TAKES_SKIP_SAMPLE_EVERY:-10}
    full_age=$(full_marker_age) || full_age=""
    if [ -z "$full_age" ] || ! [ "$full_age" -lt 86400 ] 2> /dev/null; then
      echo "identity takes ($cc): no full takes row of $cc on main in the last 24 hours (${full_age:-no marker}${full_age:+ s ago}): the takes run in full, held against the gate"
    elif [ "$every" -gt 0 ] 2> /dev/null && [ $(( 0x${head:0:8} % every )) != 0 ]; then
      takes_skipped=1 takes_moves=""
      echo "identity takes ($cc): the takes diff skipped -- $gate_line under the head's rules: no take can have moved" | tee -a "$out/verdict.txt"
    else
      echo "identity takes ($cc): a sampled commit (1 in $every): the takes run in full, held against the gate"
    fi
  fi
fi

if [ -n "$takes_moves" ]; then
  mkdir -p "$out/head" "$out/base"
  # Beside a base row, its final takes are that row's (awaited after the session).
  if [ "$side" != head ]; then
    SECONDS=0
    if reused "$(asset final "$(capture_key "$base")")" "$out/base/ratchet-takes.tsv" "the base final takes"; then
      echo "identity takes ($cc): base final takes reused"; timing base-final reused; final_gaps base "$base"
    else base_final_start; fi
  fi
fi

# THE HEAD'S OWN RESULTS, STORED. A head is a base's commit as often as not: a measurement's base row resolves main's
# newest commit while main's own run for it still waits for a runner, and stores it under the key that names its identity
# code and every input -- the key main's takes row then asks for its head. 36 of 137 main head sessions over 2026-10-21..23
# resolved a key already stored (209 of their 740 minutes). Each of the head's results is fetched when stored under its
# own key, exactly as a base's is, and only the rest are made: none left, and the head runs no sbt at all. A key names
# every input, so a stored result is the one this head would make; a key not stored is made (never a nearer one reused).
head_final="" head_resolve="" head_shape=""   # each `reused` when fetched from the store
mkdir -p "$out/head"
if [ -n "$takes_moves" ]; then
  if reused "$(asset final "$(capture_key "$head")")" "$out/head/ratchet-takes.tsv" "the head final takes"; then
    head_final=reused; final_gaps head "$head"; echo "identity takes ($cc): head final takes reused"
  fi
  if reused "$(asset decisions "$(decisions_key "$head")")" "$out/head/decisions-$cc.jsonl" "the head resolve"; then
    head_resolve=reused; echo "identity takes ($cc): head resolve reused from $(asset decisions "$(decisions_key "$head")")"
  fi
fi
if [ -n "$shape_moves" ] && reused "$(asset shape "$(shape_key "$head")")" "$out/head/shape.tar" "the head corpus-shape findings"; then
  rm -rf "$out/head/shape" && mkdir -p "$out/head/shape" && tar -xf "$out/head/shape.tar" -C "$out/head/shape" \
    && head_shape=reused && echo "identity takes ($cc): head corpus-shape findings reused"
fi

# THE HEAD IN ONE sbt: what of its final takes, its resolve and its corpus shape was not stored, each a command of one
# JVM. Each its own sbt was a JVM start, a project load and a cold JIT apiece -- the head ratchet's 1-2 minutes were
# mostly that, the replay seconds. sbt stops at a command that fails (a tool's exit takes the JVM with it), so each
# command's "[success]" is what says it ran: the ratchet first (seconds, and the resolve is no verdict without it), the
# corpus shape last (its specs FAIL on a finding, which is no failure here).
head_commands=() head_names=()
if [ -n "$takes_moves" ]; then
  [ -n "$head_final" ] || { head_names+=(final); head_commands+=("worker/IntegrationTest/runMain scripts.UnmatchedClustersRatchet --measure --out target/identity-unmatched --countries $cc$(
    traced "$here/worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala" "$traces/head-final.jsonl.gz")"); }
  [ -n "$head_resolve" ] || { head_names+=(resolve); head_commands+=("worker/IntegrationTest/runMain integration.IdentityResolveDump --out $out/head --full $cc --corpus-dir $corpus --fixtures $fixtures$(
    traced "$here/worker/src/it/scala/IdentityResolveDump.scala" "$traces/head-model.jsonl.gz")"); }
fi
[ -n "$shape_moves" ] && [ -z "$head_shape" ] && { head_names+=(shape); head_commands+=("$shape_command"); }
took=()
if [ "${#head_commands[@]}" -gt 0 ]; then
  rm -f "$here/target/identity-unmatched/ratchet-takes.tsv" "$here/target/identity-unmatched/capture-gaps.tsv" \
        "$here"/target/convergence-diagnostics/corpus-shape-recorded-"$cc"-*.txt
  SECONDS=0
  ( cd "$here" && KINOWO_IDENTITY_CORPUS_DIR="$corpus" "${IDENTITY_SBT[@]}" -J-Xmx"$heap" "${head_commands[@]}" > "$out/head.log" 2>&1 )
  session=$?
  grep -E '\[full-|\[ratchet\]|error|Total time' "$out/head.log"
  # each command's seconds, in order: sbt's "[success] Total time: N s" after every command that succeeded
  while read -r t; do took+=("$t"); done < <(sed 's/\x1b\[[0-9;]*m//g' "$out/head.log" | sed -n 's/^\[success\] Total time: \([0-9]*\) s.*/\1/p')
  echo "identity takes ($cc): the head's session ran ${head_names[*]} in ${SECONDS}s (sbt exit $session)"
elif [ -n "$takes_moves$shape_moves" ]; then
  session=0
  echo "identity takes ($cc): every head result was stored -- no head session"
fi
took_by() { # <name>: the seconds the session's <name> command took; fails when it did not run (or not to success)
  local i
  for i in "${!head_names[@]}"; do
    [ "${head_names[$i]}" = "$1" ] && [ "$i" -lt "${#took[@]}" ] && { echo "${took[$i]}"; return 0; }
  done
  return 1
}

if [ -n "$takes_moves" ]; then
  if [ -n "$head_final" ]; then
    SECONDS=0; timing head-final reused
  elif secs=$(took_by final); then
    ratchet_out "$here" "$out/head/ratchet-takes.tsv"
    SECONDS=$secs; timing head-final replayed
    keep_final head "$head"
  else
    echo "::warning::identity takes ($cc): the ratchet exited $session ($out/head.log)"; final=2
  fi
  if [ -n "$head_resolve" ]; then
    SECONDS=0; timing head-resolve reused
  elif secs=$(took_by resolve) && [ -s "$out/head/decisions-$cc.jsonl" ]; then
    SECONDS=$secs; timing head-resolve resolved
    keep "$out/head/decisions-$cc.jsonl" "$(asset decisions "$(decisions_key "$head")")"
    keep_trace head "$head"
  else
    echo "::warning::identity takes ($cc): the resolve exited $session (its log: $out/head.log)"; exit 2
  fi
  if [ "$side" = head ]; then
    SECONDS=0
    if awaited "$(asset final "$(capture_key "$base")")" "$out/base/ratchet-takes.tsv" "the base final takes"; then
      timing base-final "$how"; final_gaps base "$base"
    else base_final_start; fi # beside the base's resolve below
  else
    base_final_finish
  fi

  SECONDS=0
  if awaited "$(asset decisions "$(decisions_key "$base")")" "$out/base/decisions-$cc.jsonl" "the base resolve"; then
    echo "identity takes ($cc): base resolve $how from $(asset decisions "$(decisions_key "$base")") in ${SECONDS}s"
    timing base-resolve "$how"
  else
    base_resolve
  fi
  base_final_finish
  keep_trace base "$base" base
  gaps_of head; gaps_of base
elif [ -z "$takes_skipped" ]; then
  echo "identity takes ($cc): no identity code changed since $base -- no take can have moved" | tee -a "$out/verdict.txt"
fi

if [ -n "$shape_moves" ]; then
  shaped=0  # the head's seconds, set only when it wrote findings
  [ -n "$head_shape" ] || shape_found "$here" "$out/head/shape"
  if [ -n "$(ls "$out/head/shape" 2>/dev/null)" ]; then
    if [ -n "$head_shape" ]; then
      SECONDS=0; timing head-shape reused
    else
      SECONDS=$(took_by shape || echo 0); timing head-shape ran; shaped=$SECONDS
      tar -cf "$out/head/shape.tar" -C "$out/head/shape" . && keep "$out/head/shape.tar" "$(asset shape "$(shape_key "$head")")"
    fi
    SECONDS=0
    if base_shape_stored awaited; then
      echo "identity takes ($cc): base corpus-shape findings $how"
      timing base-shape "$how"
    else
      base_shape
    fi
  else
    shape=2
  fi
  echo "identity takes ($cc): corpus-shape specs ran in $((shaped + SECONDS))s"
elif [ -n "$shape_command" ]; then
  echo "identity takes ($cc): no corpus-shape check, allowlist or identity code changed since $base -- no finding can be new" \
    | tee -a "$out/verdict.txt"
else
  echo "identity takes ($cc): corpus shape not checked (no SHAPE_COMMAND)" | tee -a "$out/verdict.txt"
fi
for dir in "$tree" "$final_tree"; do [ -d "$dir" ] && { git worktree remove --force "$dir" || true; }; done

diff_takes() { # <base> <head> [--final]
  python3 "$here/scripts/ci/identity_takes_diff.py" "$cc" "$1" "$2" "${@:3}" \
    --judgements "$here/test/resources/fixtures/identity-judgements.tsv" \
    --base-name "${base:0:9}" --head-name "${head:0:9}" ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"}
}
# The verdict's first line is the commit status's description: the takes' line when they ran, else the shape's.
# Beside the out dir, never in TMPDIR: nothing outside it to leak on an early exit, and no TMPDIR to fail on.
verdict="$out/verdict.txt.part"
: > "$verdict"
if [ -n "$takes_moves" ]; then
  diff_takes "$out/base/decisions-$cc.jsonl" "$out/head/decisions-$cc.jsonl" | tee -a "$verdict"
  model=${PIPESTATUS[0]}
  refresh_full_marker
  if [ "$final" = 0 ]; then
    diff_takes "$out/base/ratchet-takes.tsv" "$out/head/ratchet-takes.tsv" --final | tee -a "$verdict"
    final=${PIPESTATUS[0]}
  else
    echo "::error::identity takes ($cc): the final takes could not be replayed (the ratchet wrote no takes; $out/*/ratchet-takes.tsv.log)" \
      | tee -a "$verdict"
  fi
fi
if [ -n "$shape_moves" ]; then
  if [ "$shape" = 0 ]; then
    python3 "$here/scripts/ci/corpus_shape_diff.py" "$cc" "$out/base/shape" "$out/head/shape" \
      --base-name "${base:0:9}" --head-name "${head:0:9}" ${GITHUB_STEP_SUMMARY:+--summary "$GITHUB_STEP_SUMMARY"} | tee -a "$verdict"
    shape=${PIPESTATUS[0]}
  else
    echo "::error::identity takes ($cc): the corpus-shape specs wrote no findings (their log: $out/*/shape.log)" | tee -a "$verdict"
  fi
fi
cat "$out/verdict.txt" >> "$verdict" && mv "$verdict" "$out/verdict.txt"

# THE SHADOW: the gate's answer held against the takes it was asked about, when they ran -- `moved` when the model's or
# the final takes differ between base and head at all (lost, changed or gained), `unknown` when a side could not be read.
# A gate that said SKIP where a take moved is the one thing it must never do: said as an error, posted, and the row fails.
# Every answer is posted as the commit status `takes skip gate (<cc>)` and logged as one `takes-skip-shadow` line, which
# scripts/ci/takes-skip-shadow-report.sh counts before skipping is enabled (and while it is, on the sampled commits).
if [ -n "$gate_asked" ] && [ -z "$takes_skipped" ]; then
  actual=$(python3 - "$out/base/decisions-$cc.jsonl" "$out/head/decisions-$cc.jsonl" "$out/base/ratchet-takes.tsv" "$out/head/ratchet-takes.tsv" <<'PY' 2> /dev/null || echo unknown
import csv, json, sys
def model(path):
    with open(path) as f:
        return {d["key"]: d.get("film") for d in map(json.loads, filter(str.strip, f))}
def final(path):
    with open(path) as f:
        return {(r["country"], r["venue"], r["rawTitle"]): (r.get("status"), r.get("film")) for r in csv.DictReader(f, delimiter="\t")}
b, h, bf, hf = sys.argv[1:]
print("moved" if model(b) != model(h) or final(bf) != final(hf) else "unmoved")
PY
)
  if [ "$model" = 2 ] || [ "$final" = 2 ]; then actual=unknown; fi
  agree=yes; [ "$gate" = SKIP ] && [ "$actual" != unmoved ] && agree=no
  line="takes-skip-shadow ($cc): mode=$skip_mode gate=$gate actual=$actual agree=$agree base=${base:0:9} head=${head:0:9}"
  echo "$line" | tee -a "$out/verdict.txt" ${GITHUB_STEP_SUMMARY:+"$GITHUB_STEP_SUMMARY"}
  if [ -n "${GITHUB_REPOSITORY:-}" ] && [ -n "${GITHUB_SHA:-}" ]; then
    # allow-silenced: the status reports the comparison; a disagreement fails the row below whatever the post did
    bounded "$gh_read" gh api -X POST "repos/$GITHUB_REPOSITORY/statuses/$GITHUB_SHA" -f state="$([ "$agree" = yes ] && echo success || echo failure)" \
      -f context="takes skip gate ($cc)" -f description="$skip_mode: gate $gate, takes $actual -- $([ "$agree" = yes ] && echo agree || echo DISAGREE)" \
      > /dev/null 2>&1 || true
  fi
  if [ "$agree" = no ]; then
    echo "::error title=takes skip gate ($cc)::the gate said SKIP but the takes were $actual -- the gate is unsound; see $out/gate/reasons.txt" \
      | tee -a "$out/verdict.txt"
    exit 1
  fi
fi
# A move or a new finding anywhere fails; failing that, a half that could not run is no verdict (2), never a pass.
if [ "$model" = 1 ] || [ "$final" = 1 ] || [ "$shape" = 1 ]; then exit 1; fi
if [ "$model" != 0 ] || [ "$final" != 0 ] || [ "$shape" != 0 ]; then exit 2; fi
exit 0
