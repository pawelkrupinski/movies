#!/usr/bin/env bash
# scheduled-failure-issue.sh against a stub `gh` that records its calls and answers `issue list`
# from STUB_OPEN_ISSUE. Run: bash scripts/ci/scheduled-failure-issue-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
# `issue list` answers as the real one does with --json: a lookalike the fuzzy search also
# finds (a title that merely CONTAINS the searched one), then STUB_OPEN_ISSUE under the exact title.
if [ "$1 $2" = "issue list" ]; then
  while [ $# -gt 0 ] && [ "$1" != --search ]; do shift; done
  title="${2%\" in:title}"; title="${title#\"}"
  jq -nc --arg t "$title" --arg n "${STUB_OPEN_ISSUE:-}" \
    '[{number: 99, title: ("Re: " + $t)}] + (if $n == "" then [] else [{number: ($n | tonumber), title: $t}] end)'
fi
exit 0
STUB
chmod +x "$stub_dir/gh"

run() {  # run <open issue number or ""> <args...>  -> prints the gh calls after `issue list`
  export STUB_LOG="$stub_dir/log" STUB_OPEN_ISSUE="$1"; shift
  : > "$STUB_LOG"
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/scheduled-failure-issue.sh" "$@" >/dev/null
  grep -v '^issue list' "$STUB_LOG" | cut -c1-60
}

check "a first failure opens an issue" \
  "issue create --title Scheduled workflow failing: OG cards --" \
  "$(run "" failed "OG cards" "https://run/1" pawelkrupinski | head -n 1)"
check "...and the assignee is the one passed" "1" \
  "$(export STUB_LOG="$stub_dir/log" STUB_OPEN_ISSUE=""; : > "$STUB_LOG"; PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/scheduled-failure-issue.sh" failed "OG cards" "u" pawelkrupinski >/dev/null; grep -c -- '--assignee pawelkrupinski' "$STUB_LOG")"
check "a failure while one is open comments on it instead of opening a second" \
  "issue comment 7 --body Failed again: https://run/2" \
  "$(run 7 failed "OG cards" "https://run/2" pawelkrupinski)"
check "an issue whose title only contains the workflow's is not its issue" \
  "issue create --title Scheduled workflow failing: OG cards --" \
  "$(run "" failed "OG cards" "https://run/1" pawelkrupinski | head -n 1)"
check "a workflow name with a quote still finds its open issue" \
  "issue comment 7 --body Failed again: https://run/5" \
  "$(run 7 failed 'Say "cheese"' "https://run/5" pawelkrupinski)"
check "a pass closes the open issue" \
  "issue close 7 --comment Passed again: https://run/3" \
  "$(run 7 recovered "OG cards" "https://run/3")"
check "a pass with nothing open does nothing" "" "$(run "" recovered "OG cards" "https://run/4")"
check "an unknown verdict is refused" "2" \
  "$(STUB_LOG=/dev/null PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/scheduled-failure-issue.sh" nonsense w u >/dev/null 2>&1; echo $?)"

spec_summary
