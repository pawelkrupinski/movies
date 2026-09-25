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
check "a status the API refuses does not fail the leg it reports on" "0" \
  "$(STUB_FAIL=1 PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/leg-verdict.sh" failure x >/dev/null 2>&1; echo $?)"

spec_summary
