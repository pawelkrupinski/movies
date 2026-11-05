#!/usr/bin/env bash
# scripts/ci/dispatch.sh against a stub `gh`: a 5xx dispatch is retried, a spent rate limit waited out, and nothing else is, every poll ends at its
# deadline, and a failed read is a failure -- never "no run", "no runs waiting" or "done".
# Run: bash scripts/ci/dispatch-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/ci/dispatch.sh"

printf '\033[36m▸\033[0m dispatch.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# Each call is logged; $ANSWERS/<n> (n = the call's ordinal) is "fail <stderr>" or the stdout to print, else $DEFAULT.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
n=$(wc -l < "$WORK/calls" | tr -d ' ')
answer=${DEFAULT:-}
[ -f "$WORK/answers/$n" ] && answer=$(cat "$WORK/answers/$n")
case "$answer" in fail*) echo "${answer#fail }" >&2; exit 1 ;; *) [ -z "$answer" ] || echo "$answer" ;; esac
STUB
chmod +x "$work/bin/gh"
export PATH="$work/bin:$PATH" WORK="$work" DISPATCH_RETRY_SECONDS=0 DISPATCH_POLL_SECONDS=0 DISPATCH_RATE_LIMIT_PAD=0 DISPATCH_RATE_LIMIT_SECONDARY_SECONDS=0
reset() { rm -rf "$work/calls" "$work/answers"; mkdir -p "$work/answers"; : > "$work/calls"; export DEFAULT=""; }
answer() { echo "$2" > "$work/answers/$1"; }
calls() { wc -l < "$work/calls" | tr -d ' '; }

reset; answer 1 "fail HTTP 500: Failed to run workflow dispatch"
dispatch_workflow w.yml main -f a=b 2> /dev/null; check "a 5xx dispatch is retried" "0 2" "$? $(calls)"
check "...with the ref and inputs as given" "workflow run w.yml --ref main -f a=b" "$(sed -n 2p "$work/calls")"
reset; DEFAULT="fail HTTP 500"
dispatch_workflow w.yml main 2> /dev/null; check "...three times at most" "1 3" "$? $(calls)"
reset; answer 1 "fail HTTP 403: Resource not accessible"
dispatch_workflow w.yml main 2> /dev/null; check "any other refusal fails at once" "1 1" "$? $(calls)"

spent="fail HTTP 403: API rate limit exceeded for installation."
reset; answer 1 "$spent"; answer 2 "0 $(date +%s)"
dispatch_workflow w.yml main 2> /dev/null; check "a spent rate limit is waited out to its reset, then retried" "0 3" "$? $(calls)"
check "...reading the reset from the rate limit" "api rate_limit" "$(sed -n 2p "$work/calls" | cut -d' ' -f1-2)"
reset; answer 1 "$spent"; answer 2 "0 $(($(date +%s) + 3600))"
dispatch_workflow w.yml main 2> /dev/null; check "...but not a reset further off than the cap" "1 2" "$? $(calls)"
reset; answer 1 "$spent"; answer 2 "fail HTTP 502"
dispatch_workflow w.yml main 2> /dev/null; check "...nor an unreadable one" "1 2" "$? $(calls)"
reset; answer 1 "$spent"; answer 2 "0 $(($(date +%s) - 120))"
dispatch_workflow w.yml main 2> "$work/err"; check "...a reset already past waits 0s, then retries" "0 3" "$? $(calls)"
check "...saying so" "true" "$(grep -q 'retrying in 0s' "$work/err" && echo true || echo false)"
secondary="fail HTTP 403: You have exceeded a secondary rate limit"
reset; answer 1 "$secondary"; answer 2 "17 $(date +%s)"
dispatch_workflow w.yml main 2> "$work/err"
check "a secondary limit (core not spent) pauses, then retries" "0 3" "$? $(calls)"
check "...for DISPATCH_RATE_LIMIT_SECONDARY_SECONDS" "true" "$(grep -q 'retrying in 0s' "$work/err" && echo true || echo false)"
reset; answer 1 "$secondary"; answer 2 "17 $(date +%s)"
(unset DISPATCH_RATE_LIMIT_SECONDARY_SECONDS; DISPATCH_RATE_LIMIT_MAX_WAIT=59 dispatch_workflow w.yml main 2> /dev/null)
check "...a minute by default: past a 59 s cap" "1 2" "$? $(calls)"

reset; answer 2 "4242"
find_dispatched_run w.yml 2026-10-15T00:00:00Z '.headSha == "x"' --branch b 2> /dev/null
check "the dispatched run is polled for until it is listed" "0 4242" "$? $DISPATCHED_RUN"
check "...filtered by the caller's predicate and arguments" "true" \
  "$(grep -q -- '--branch b .*select(.createdAt >= "2026-10-15T00:00:00Z") | select(.headSha == "x")' "$work/calls" && echo true || echo false)"
reset
DISPATCH_FIND_SECONDS=0 find_dispatched_run w.yml 2026-10-15T00:00:00Z true 2> /dev/null
check "no run listed by the deadline is a failure" "1 ''" "$? '$DISPATCHED_RUN'"
reset; answer 1 "fail HTTP 502"
find_dispatched_run w.yml 2026-10-15T00:00:00Z true 2> /dev/null
check "a failed listing is a failure, not 'not listed yet'" "1 1" "$? $(calls)"

reset; answer 1 "4711"
run_in_flight w.yml main abc 2> /dev/null; check "a queued or running run of the same commit is in flight" "0 4711" "$? $IN_FLIGHT_RUN"
check "...asked of the ref's runs, by head and status" "true" \
  "$(grep -q -- 'run list --workflow w.yml --branch main --limit 20 --json databaseId,headSha,status --jq .*headSha == "abc" and .status != "completed"' "$work/calls" && echo true || echo false)"
reset
run_in_flight w.yml main abc 2> /dev/null; check "none: nothing in flight" "0 ''" "$? '$IN_FLIGHT_RUN'"
reset; answer 1 "fail HTTP 500"
run_in_flight w.yml main abc 2> /dev/null; check "an unreadable list is a failure, never 'none in flight'" "1 ''" "$? '$IN_FLIGHT_RUN'"
reset; answer 1 "2"; DEFAULT=0
wait_until_no_runs w.yml 'true' 60 "busy" 2> /dev/null; check "runs matching are waited out" "0 2" "$? $(calls)"
reset; DEFAULT=1
wait_until_no_runs w.yml 'true' 0 "busy" 2> /dev/null; check "...until the deadline" "1 1" "$? $(calls)"
reset; answer 1 "fail HTTP 502"
wait_until_no_runs w.yml 'true' 60 "busy" 2> /dev/null; check "a failed listing is not 'none waiting'" "1 1" "$? $(calls)"

# gh run view's JSON, as GitHub answers it: <status> [<conclusion>]
view() { printf '{"databaseId":7,"headSha":"%s","headBranch":"b","workflowName":"w","status":"%s","conclusion":"%s"}' \
  "$(printf 'a%.0s' $(seq 40))" "$1" "${2:-}"; }
reset; answer 1 "fail connection refused"; answer 2 "$(view in_progress)"; DEFAULT=$(view completed failure)
wait_for_run 7 60 2> /dev/null; check "a run is waited on through a read blip to its conclusion" "0 failure 3" "$? $RUN_CONCLUSION $(calls)"
reset; DEFAULT=$(view completed failure)
check "...and a failed run is a conclusion under the caller's set -e (the drift explorer's step), never its exit" "0 failure" \
  "$(set -e; wait_for_run 7 60 2> /dev/null; echo "$? $RUN_CONCLUSION")"
reset; answer 1 'null'; answer 2 '{"databaseId":null,"headSha":null,"status":null}'; DEFAULT=$(view completed success)
wait_for_run 7 60 2> /dev/null; check "...through null answers, never taken for one" "0 success 3" "$? $RUN_CONCLUSION $(calls)"
reset; DEFAULT=$(view completed cancelled)
wait_for_run 7 60 2> /dev/null; check "a cancelled run is a conclusion, not followed" "0 cancelled 1" "$? $RUN_CONCLUSION $(calls)"
reset; DEFAULT="fail connection refused"
wait_for_run 7 60 2> /dev/null; check "ten failed reads in a row end the wait" "1 10" "$? $(calls)"
reset; DEFAULT=$(view completed success)
wait_for_run 7 -1 2> /dev/null; check "a deadline already past still reads the run once" "0 success 1" "$? $RUN_CONCLUSION $(calls)"
reset; DEFAULT=$(view in_progress)
wait_for_run 7 0 2> /dev/null; check "a run not done by the deadline ends the wait" "1 ''" "$? '$RUN_CONCLUSION'"

check "the backoff grows x1.5 to five minutes, and never polls at once" "90 135 202 300 300 1" \
  "$(for d in 60 90 135 202 300 0; do printf '%s ' "$(dispatch_backoff_next "$d")"; done | sed 's/ $//')"
check "...its cap is DISPATCH_BACKOFF_CAP's" "120" "$(DISPATCH_BACKOFF_CAP=120 dispatch_backoff_next 100)"
# run_eta through a gh that applies --jq itself, as gh does, to $RUNS
mkdir -p "$work/jqbin"
printf '#!/usr/bin/env bash\necho "$*" >> "$WORK/calls"\nf=""; p=""; for a in "$@"; do [ "$p" = --jq ] && f=$a; p=$a; done\n[ -n "${RUNS_FAIL:-}" ] && exit 1\njq -r "$f" <<< "$RUNS"\n' > "$work/jqbin/gh"
chmod +x "$work/jqbin/gh"
eta_run() { printf '{"status":"%s","conclusion":"%s","headBranch":"%s","createdAt":"2026-10-23T10:00:00Z","updatedAt":"2026-10-23T%s:00Z"}' "$@"; }
eta() { reset; DISPATCH_GH="$work/jqbin/gh" RUNS="$1" run_eta w.yml measure/ 2> /dev/null; echo "$? $RUN_ETA_SAID"; }
check "the ETA is the median wall time of the prefix's decided runs, rounded up" "0 ~40 min (median of 3 recent runs)" \
  "$(eta "[$(eta_run completed success measure/a 10:20),$(eta_run completed failure measure/b 10:40),$(eta_run completed success measure/c 11:30),$(eta_run completed cancelled measure/d 10:01),$(eta_run in_progress '' measure/e 10:02),$(eta_run completed success main 12:00)]")"
check "...asking for the workflow's newest 30" "run list --workflow w.yml --limit 30" "$(head -n 1 "$work/calls" | cut -d' ' -f1-6)"
check "no decided run of the prefix: no ETA" "1 " "$(eta "[$(eta_run completed success main 10:20)]")"
check "an unreadable list: no ETA, and said" "1 " "$(RUNS_FAIL=1 eta '[]')"

spec_summary
