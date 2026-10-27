#!/usr/bin/env bash
# roster-audit.sh against a stub `sbt` that exits STUB_EXIT. Run: bash scripts/ci/roster-audit-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/sbt" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" > "$STUB_ARGS"
exit "$STUB_EXIT"
STUB
chmod +x "$stub_dir/sbt"

run() {  # run <sbt exit>  -> "<script exit>|<GITHUB_OUTPUT>"
  : > "$stub_dir/output"
  STUB_EXIT="$1" STUB_ARGS="$stub_dir/args" GITHUB_OUTPUT="$stub_dir/output" PATH="$stub_dir:$PATH" \
    bash "$REPO_ROOT/scripts/ci/roster-audit.sh" summary.md >/dev/null 2>&1
  echo "$?|$(cat "$stub_dir/output")"
}

check "a clean audit is green, nothing overdue" "0|overdue=false" "$(run 0)"
check "...and runs the audit with the report file" "worker/runMain tools.RosterAudit summary.md" "$(cat "$stub_dir/args")"
check "drift found is green and overdue: the owner's issue, not a red run" "0|overdue=true" "$(run 3)"
check "an audit that could not judge stays red" "1|overdue=false" "$(run 1)"
check "sbt's own failure keeps its exit" "2|overdue=false" "$(run 2)"

spec_summary
