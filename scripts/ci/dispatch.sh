#!/usr/bin/env bash
# Dispatching a workflow and finding, then waiting on, the run it made -- the one copy of what
# .github/scripts/kick-convergence.sh and scripts/identity-measure each
# re-implemented, with every gh call and every poll under a deadline.
#
# Sourced; side-effect free. Every function returns non-zero (saying why on stderr) rather than exiting,
# so the caller decides what a failure means -- and a failed READ is never taken for "nothing there".
#
#   dispatch_workflow <workflow> <ref> [gh workflow run args...]
#       `gh workflow run`, a 5xx (GitHub's own blip) retried twice after DISPATCH_RETRY_SECONDS (15); a
#       refusal for the token's spent API rate limit waited out to its reset when that is at most
#       DISPATCH_RATE_LIMIT_MAX_WAIT (900 s) away (DISPATCH_RATE_LIMIT_SECONDARY_SECONDS, 60, for a secondary
#       limit), then tried again; any other
#       refusal (403, 404, 422) fails at once.
#   find_dispatched_run <workflow> <since ISO-8601> <jq predicate over a run> [gh run list args...]
#       Sets DISPATCHED_RUN to the newest run created at or after <since> matching the predicate (fields:
#       databaseId, headSha, createdAt, displayTitle), polled for DISPATCH_FIND_SECONDS (120): a dispatch's
#       run appears in the list seconds after it.
#   run_in_flight <workflow> <ref> <sha>
#       Sets IN_FLIGHT_RUN to a run of <workflow> on <ref> for <sha> that is still queued or running (none: empty) --
#       the run a second dispatch of the same commit would only wait behind (run 3656, 2026-10-29: 54 min queued
#       behind 3653, the same commit, then run in full). Non-zero when the list cannot be read: the caller dispatches.
#   wait_until_no_runs <workflow> <jq predicate over a run> <timeout seconds> <what is waited for> [gh run list args...]
#       Polls the workflow's newest 20 runs until none matches (fields: status, conclusion).
#   wait_for_run <run id> <timeout seconds>
#       Until the run completes; sets RUN_CONCLUSION (cancelled included: never followed to a superseding run).
#       scripts/ci-wait --run --no-follow: ten unreadable reads in a row end the wait -- one is a blip, and a
#       null or partial answer is never a conclusion.
#   dispatch_backoff_next <seconds>
#       The next poll's delay after one of <seconds>: x1.5, at least 1, at most DISPATCH_BACKOFF_CAP (300) -- the backoff
#       ~/bin/agentctl's `wait run` polls with, so an hour's wait asks the API about fifteen times, not 120.
#   run_eta <workflow> <branch prefix> [gh run list args...]
#       Sets RUN_ETA_SAID to "~N min (median of K recent runs)": the wall time (createdAt to updatedAt) of the workflow's
#       newest 30 runs that ended success or failure on a branch starting <branch prefix>. Non-zero, RUN_ETA_SAID empty,
#       when none is listed or the list cannot be read. An estimate to say, never a deadline.
#
# The answers are variables, not stdout: a wait run as `x=$(wait_for_run ...)` is a subshell, and a SIGTERM
# to the caller is deferred until that subshell's wait ends -- hours -- so its clean-up trap never ran.
#
# DISPATCH_GH_TIMEOUT (60 s) bounds each gh call; DISPATCH_POLL_SECONDS (30) spaces the waits' polls.
# Tested by scripts/ci/dispatch-test.sh against a stub `gh`.
# shellcheck disable=SC2034  # DISPATCHED_RUN and RUN_CONCLUSION are the sourcing caller's.

_dispatch_say() { echo "dispatch: $*" >&2; }
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/../tool-conventions.sh" # bounded
# DISPATCH_GH: the gh to run (default gh; scripts/identity-measure passes its $KINOWO_GH).
_dispatch_gh() { bounded "${DISPATCH_GH_TIMEOUT:-60}" "${DISPATCH_GH:-gh}" "$@"; }

# Sets RATE_LIMIT_WAIT to the seconds until the token's core limit resets (+DISPATCH_RATE_LIMIT_PAD, 5; a reset
# already past waits 0), or DISPATCH_RATE_LIMIT_SECONDARY_SECONDS (60) when the core limit is not spent (a secondary
# limit, which asks for a pause); fails when that is beyond
# DISPATCH_RATE_LIMIT_MAX_WAIT (900) or the limit is unreadable. Reading /rate_limit costs no request.
_dispatch_rate_limit_wait() {
  local limit remaining reset
  limit=$(_dispatch_gh api rate_limit --jq '.resources.core | "\(.remaining) \(.reset)"' 2> /dev/null) || return 1
  read -r remaining reset <<< "$limit"
  [[ "$remaining" =~ ^[0-9]+$ && "$reset" =~ ^[0-9]+$ ]] || return 1
  if [ "$remaining" -gt 0 ]; then RATE_LIMIT_WAIT=${DISPATCH_RATE_LIMIT_SECONDARY_SECONDS:-60}; else RATE_LIMIT_WAIT=$((reset - $(date +%s) + ${DISPATCH_RATE_LIMIT_PAD:-5})); fi
  [ "$RATE_LIMIT_WAIT" -ge 0 ] || RATE_LIMIT_WAIT=0
  [ "$RATE_LIMIT_WAIT" -le "${DISPATCH_RATE_LIMIT_MAX_WAIT:-900}" ]
}

dispatch_workflow() {
  local workflow=$1 ref=$2 attempt err
  shift 2
  err=$(mktemp)
  for attempt in 1 2 3; do
    if _dispatch_gh workflow run "$workflow" --ref "$ref" "$@" > /dev/null 2> "$err"; then rm -f "$err"; return 0; fi
    cat "$err" >&2
    [ "$attempt" -lt 3 ] || break
    if grep -Eq 'HTTP 5[0-9][0-9]' "$err"; then
      _dispatch_say "$workflow: GitHub answered 5xx, retrying in ${DISPATCH_RETRY_SECONDS:-15}s (attempt $attempt of 3)"
      sleep "${DISPATCH_RETRY_SECONDS:-15}"
    elif grep -Eqi 'rate limit' "$err" && _dispatch_rate_limit_wait; then
      _dispatch_say "$workflow: the API rate limit is spent, retrying in ${RATE_LIMIT_WAIT}s, at its reset (attempt $attempt of 3)"
      sleep "$RATE_LIMIT_WAIT"
    else
      break
    fi
  done
  rm -f "$err"
  _dispatch_say "could not dispatch $workflow on $ref"
  return 1
}

find_dispatched_run() {
  local workflow=$1 since=$2 predicate=$3 found deadline
  shift 3
  DISPATCHED_RUN=""
  deadline=$((SECONDS + ${DISPATCH_FIND_SECONDS:-120}))
  while :; do
    found=$(_dispatch_gh run list --workflow "$workflow" --event workflow_dispatch --limit 20 "$@" \
      --json databaseId,headSha,createdAt,displayTitle \
      --jq "[.[] | select(.createdAt >= \"$since\") | select($predicate)] | sort_by(.createdAt) | last | .databaseId // empty") \
      || { _dispatch_say "could not list $workflow's runs"; return 1; }
    [ -n "$found" ] && { DISPATCHED_RUN=$found; return 0; }
    [ "$SECONDS" -lt "$deadline" ] || break
    sleep 5
  done
  _dispatch_say "dispatched $workflow, but no run of it appeared in ${DISPATCH_FIND_SECONDS:-120}s"
  return 1
}

run_in_flight() {
  local workflow=$1 ref=$2 sha=$3 found
  IN_FLIGHT_RUN=""
  found=$(_dispatch_gh run list --workflow "$workflow" --branch "$ref" --limit 20 --json databaseId,headSha,status \
    --jq "[.[] | select(.headSha == \"$sha\" and .status != \"completed\")] | first | .databaseId // empty") \
    || { _dispatch_say "could not list $workflow's runs on $ref"; return 1; }
  IN_FLIGHT_RUN=$found
}

wait_until_no_runs() {
  local workflow=$1 predicate=$2 timeout=$3 what=$4 count deadline
  shift 4
  deadline=$((SECONDS + timeout))
  while :; do
    count=$(_dispatch_gh run list --workflow "$workflow" --limit 20 "$@" --json status,conclusion \
      --jq "[.[] | select($predicate)] | length") \
      || { _dispatch_say "could not list $workflow's runs"; return 1; }
    [ "$count" = 0 ] && return 0
    [ "$SECONDS" -lt "$deadline" ] || { _dispatch_say "still $what after ${timeout}s"; return 1; }
    _dispatch_say "$what: waiting"
    sleep "${DISPATCH_POLL_SECONDS:-30}"
  done
}

wait_for_run() {
  local run=$1 timeout=$2 out status
  RUN_CONCLUSION=""
  # a deadline already past (a caller's `deadline - SECONDS` that ticked over) still reads the run once
  [ "$timeout" -ge 0 ] 2> /dev/null || timeout=0
  out=$(mktemp)
  # In the background and `wait`ed for: a signal to the caller is handled at once, not after the run ends -- and
  # ci-wait, its parent gone, stops at its next poll.
  "$(dirname "${BASH_SOURCE[0]}")/../ci-wait" --run "$run" --no-follow --timeout "${timeout}s" \
    --interval "${DISPATCH_POLL_SECONDS:-30}s" > "$out" &
  # `|| status=$?`: a failed run is ci-wait's exit 1, and a bare `wait` would end a `set -e` caller right there
  status=0
  wait $! || status=$?
  RUN_CONCLUSION=$(cut -f3 "$out")
  rm -f "$out"
  case "$status" in
    0|1|3) return 0 ;;
    *) RUN_CONCLUSION=""; _dispatch_say "run $run gave no conclusion within ${timeout}s (or could not be read)"; return 1 ;;
  esac
}

dispatch_backoff_next() {
  local next=$(($1 * 3 / 2)) cap=${DISPATCH_BACKOFF_CAP:-300}
  [ "$next" -ge 1 ] || next=1
  [ "$next" -le "$cap" ] || next=$cap
  echo "$next"
}

run_eta() {
  local workflow=$1 prefix=$2 out minutes count
  shift 2
  RUN_ETA_SAID=""
  out=$(_dispatch_gh run list --workflow "$workflow" --limit 30 "$@" --json status,conclusion,createdAt,updatedAt,headBranch \
    --jq "[.[] | select(.status == \"completed\" and (.conclusion == \"success\" or .conclusion == \"failure\")
                       and ((.headBranch // \"\") | startswith(\"$prefix\")))
               | (.updatedAt | fromdateiso8601) - (.createdAt | fromdateiso8601)] | sort
           | if length == 0 then empty else \"\\(.[length / 2 | floor] / 60 | ceil) \\(length)\" end") \
    || { _dispatch_say "could not list $workflow's runs for an ETA"; return 1; }
  read -r minutes count <<< "$out"
  [[ "$minutes" =~ ^[0-9]+$ ]] && [[ "$count" =~ ^[0-9]+$ ]] || return 1
  RUN_ETA_SAID="~$minutes min (median of $count recent runs)"
}
