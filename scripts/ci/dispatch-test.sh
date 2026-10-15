#!/usr/bin/env bash
# scripts/ci/dispatch.sh against a stub `gh`: a 5xx dispatch is retried and nothing else is, every poll ends at its
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
export PATH="$work/bin:$PATH" WORK="$work" DISPATCH_RETRY_SECONDS=0 DISPATCH_POLL_SECONDS=0
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

reset; answer 1 "2"; DEFAULT=0
wait_until_no_runs w.yml 'true' 60 "busy" 2> /dev/null; check "runs matching are waited out" "0 2" "$? $(calls)"
reset; DEFAULT=1
wait_until_no_runs w.yml 'true' 0 "busy" 2> /dev/null; check "...until the deadline" "1 1" "$? $(calls)"
reset; answer 1 "fail HTTP 502"
wait_until_no_runs w.yml 'true' 60 "busy" 2> /dev/null; check "a failed listing is not 'none waiting'" "1 1" "$? $(calls)"

reset; answer 1 "fail connection refused"; answer 2 "$(printf 'in_progress\t')"; DEFAULT=$(printf 'completed\tfailure')
wait_for_run 7 60 2> /dev/null; check "a run is waited on through a read blip to its conclusion" "0 failure 3" "$? $RUN_CONCLUSION $(calls)"
reset; DEFAULT="fail connection refused"
wait_for_run 7 60 2> /dev/null; check "ten failed reads in a row end the wait" "1 10" "$? $(calls)"
reset; DEFAULT=$(printf 'in_progress\t')
wait_for_run 7 0 2> /dev/null; check "a run not done by the deadline ends the wait" "1 ''" "$? '$RUN_CONCLUSION'"

spec_summary
