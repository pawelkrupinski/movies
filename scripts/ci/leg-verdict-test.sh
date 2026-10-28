#!/usr/bin/env bash
# leg-verdict.sh against a stub `gh` that records its calls (and fails when STUB_FAIL is set).
# Run: bash scripts/ci/leg-verdict-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
[ -z "${STUB_FAIL:-}" ]
STUB
chmod +x "$stub_dir/gh"

export GITHUB_SERVER_URL=https://github.com GITHUB_REPOSITORY=o/r GITHUB_RUN_ID=42 GITHUB_SHA=abc \
       GITHUB_WORKFLOW="Record scrape fixtures" STUB_LOG="$stub_dir/log" GITHUB_STEP_SUMMARY="$stub_dir/summary"

run() {  # run <job status> <leg>  -> prints the gh call
  : > "$STUB_LOG"; : > "$GITHUB_STEP_SUMMARY"
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/leg-verdict.sh" "$@" >/dev/null
  cat "$STUB_LOG"
}

check "a failed leg posts a failure status named after the workflow and the leg" \
  "api --silent repos/o/r/statuses/abc -f state=failure -f context=Record scrape fixtures / scrapes (spain) -f target_url=https://github.com/o/r/actions/runs/42 -f description=scrapes (spain) FAILED" \
  "$(run failure "scrapes (spain)")"
check "...and says so in the job summary" "### scrapes (spain) FAILED" "$(head -n 1 "$GITHUB_STEP_SUMMARY")"
check "a passing leg posts success" "state=success" \
  "$(run success "convergence (poland)" | grep -o 'state=[a-z]*')"
check "a cancelled leg (a timeout) posts error, never success" "state=error" \
  "$(run cancelled "convergence (united-states)" | grep -o 'state=[a-z]*')"
export RUNNER_TEMP="$stub_dir/runner-temp"
mkdir -p "$RUNNER_TEMP"
echo "timed out under load: 30-min suite ceiling, load 61.2 / 58.0 / 40.1 on 32 cores" > "$RUNNER_TEMP/leg-verdict-why"
check "a leg whose step named its cause carries it in the status, which outlives a job log the runner lost (run #2301)" \
  "description=order-independence-p12 (germany) FAILED: timed out under load: 30-min suite ceiling, load 61.2 / 58.0 / 40.1 on 32 cores" \
  "$(run failure "order-independence-p12 (germany)" | grep -o 'description=.*')"
check "...and in its error annotation" \
  "::error title=order-independence-p12 (germany) FAILED::order-independence-p12 (germany) FAILED: timed out under load: 30-min suite ceiling, load 61.2 / 58.0 / 40.1 on 32 cores — the other legs keep running and publish their own recordings" \
  "$(PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/leg-verdict.sh" failure "order-independence-p12 (germany)" | grep '^::error')"
printf 'x%.0s' $(seq 1 200) > "$RUNNER_TEMP/leg-verdict-why"
check "a long cause is cut to the 140 characters a github.com status description holds" "140" \
  "$(run failure "leg" | grep -o 'description=.*' | sed 's/^description=//' | tr -d '\n' | wc -m | tr -d ' ')"
rm -f "$RUNNER_TEMP/leg-verdict-why"
check "a passing leg never carries a cause" "description=convergence (poland) passed" \
  "$(echo stale > "$RUNNER_TEMP/leg-verdict-why"; run success "convergence (poland)" | grep -o 'description=.*')"
rm -f "$RUNNER_TEMP/leg-verdict-why"

check "a status the API refuses does not fail the leg it reports on" "0" \
  "$(STUB_FAIL=1 PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/leg-verdict.sh" failure x >/dev/null 2>&1; echo $?)"

spec_summary
