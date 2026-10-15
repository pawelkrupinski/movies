#!/usr/bin/env bash
# Dispatching a workflow and finding, then waiting on, the run it made -- the one copy of what
# .github/scripts/kick-convergence.sh, scripts/identity-measure and scripts/identity-measure-ci.sh each
# re-implemented, with every gh call and every poll under a deadline.
#
# Sourced; side-effect free. Every function returns non-zero (saying why on stderr) rather than exiting,
# so the caller decides what a failure means -- and a failed READ is never taken for "nothing there".
#
#   dispatch_workflow <workflow> <ref> [gh workflow run args...]
#       `gh workflow run`, a 5xx (GitHub's own blip) retried twice after DISPATCH_RETRY_SECONDS (15);
#       any other refusal (403, 404, 422) fails at once.
#   find_dispatched_run <workflow> <since ISO-8601> <jq predicate over a run> [gh run list args...]
#       Sets DISPATCHED_RUN to the newest run created at or after <since> matching the predicate (fields:
#       databaseId, headSha, createdAt, displayTitle), polled for DISPATCH_FIND_SECONDS (120): a dispatch's
#       run appears in the list seconds after it.
#   wait_until_no_runs <workflow> <jq predicate over a run> <timeout seconds> <what is waited for> [gh run list args...]
#       Polls the workflow's newest 20 runs until none matches (fields: status, conclusion).
#   wait_for_run <run id> <timeout seconds>
#       Until the run completes; sets RUN_CONCLUSION. Ten failed reads in a row end the wait -- one is a blip.
#
# The answers are variables, not stdout: a wait run as `x=$(wait_for_run ...)` is a subshell, and a SIGTERM
# to the caller is deferred until that subshell's wait ends -- hours -- so its clean-up trap never ran.
#
# DISPATCH_GH_TIMEOUT (60 s) bounds each gh call; DISPATCH_POLL_SECONDS (30) spaces the waits' polls.
# Tested by scripts/ci/dispatch-test.sh against a stub `gh`.
# shellcheck disable=SC2034  # DISPATCHED_RUN and RUN_CONCLUSION are the sourcing caller's.

_dispatch_say() { echo "dispatch: $*" >&2; }
_dispatch_gh() { timeout "${DISPATCH_GH_TIMEOUT:-60}" gh "$@"; }

dispatch_workflow() {
  local workflow=$1 ref=$2 attempt err
  shift 2
  err=$(mktemp)
  for attempt in 1 2 3; do
    if _dispatch_gh workflow run "$workflow" --ref "$ref" "$@" > /dev/null 2> "$err"; then rm -f "$err"; return 0; fi
    cat "$err" >&2
    grep -Eq 'HTTP 5[0-9][0-9]' "$err" && [ "$attempt" -lt 3 ] || break
    _dispatch_say "$workflow: GitHub answered 5xx, retrying in ${DISPATCH_RETRY_SECONDS:-15}s (attempt $attempt of 3)"
    sleep "${DISPATCH_RETRY_SECONDS:-15}"
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
  local run=$1 timeout=$2 state status conclusion misses=0 deadline
  RUN_CONCLUSION=""
  deadline=$((SECONDS + timeout))
  while :; do
    if state=$(_dispatch_gh run view "$run" --json status,conclusion --jq '[.status, .conclusion] | @tsv'); then
      misses=0
      IFS=$'\t' read -r status conclusion <<< "$state"
      [ "$status" = completed ] && { RUN_CONCLUSION=$conclusion; return 0; }
    else
      misses=$((misses + 1))
      [ "$misses" -lt 10 ] || { _dispatch_say "could not read run $run ($misses reads in a row)"; return 1; }
      _dispatch_say "could not read run $run (read $misses of 10): retrying"
    fi
    [ "$SECONDS" -lt "$deadline" ] || { _dispatch_say "run $run still ${status:-unread} after ${timeout}s"; return 1; }
    sleep "${DISPATCH_POLL_SECONDS:-30}"
  done
}
