#!/usr/bin/env bash
# bot-branch-ready.sh against a stub `gh` that records its calls and answers `issue list` from STUB_OPEN_ISSUE.
# Run: bash scripts/ci/bot-branch-ready-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
if [ "$1 $2" = "issue list" ]; then
  while [ $# -gt 0 ] && [ "$1" != --search ]; do shift; done
  title="${2%\" in:title}"; title="${title#\"}"
  jq -nc --arg t "$title" --arg n "${STUB_OPEN_ISSUE:-}" \
    '[{number: 99, title: ("Re: " + $t)}] + (if $n == "" then [] else [{number: ($n | tonumber), title: $t}] end)'
fi
# The body the script hands over, kept for the checks below.
for arg in "$@"; do [ -f "$arg" ] && cp "$arg" "$STUB_BODY"; done
exit 0
STUB
chmod +x "$stub_dir/gh"
printf 'Wired 2 venues.\n' > "$stub_dir/report.md"

run() {  # run <open issue number or ""> <args...>  -> prints the gh calls after `issue list`, body paths cut
  export STUB_LOG="$stub_dir/log" STUB_BODY="$stub_dir/body" STUB_OPEN_ISSUE="$1"; shift
  : > "$STUB_LOG"
  GH_REPO=o/movies GITHUB_SERVER_URL=http://forge PATH="$stub_dir:$PATH" \
    bash "$REPO_ROOT/scripts/ci/bot-branch-ready.sh" "$@" >/dev/null
  grep -v '^issue list' "$STUB_LOG" | sed 's/ --body-file .*/ --body-file/'
}

check "a first ready branch opens an issue for the owner" \
  "issue create --title Ready to land: New UK cinemas (chore/discover-cinemas) --assignee o --body-file" \
  "$(run "" chore/discover-cinemas "New UK cinemas" "$stub_dir/report.md" https://run/1 o)"
check "...whose body links the branch, then carries the report" \
  "**New UK cinemas** is on branch [\`chore/discover-cinemas\`](http://forge/o/movies/src/branch/chore/discover-cinemas), from https://run/1.|Wired 2 venues." \
  "$(head -n 1 "$stub_dir/body")|$(tail -n 1 "$stub_dir/body")"
check "an open one is refreshed and commented on, never a second issue" \
  "issue edit 7 --body-file|issue comment 7 --body Refreshed: https://run/2" \
  "$(run 7 chore/discover-cinemas "New UK cinemas" "$stub_dir/report.md" https://run/2 o | paste -sd '|' -)"
check "an issue whose title only contains this one's is not its issue" \
  "issue create --title Ready to land: New UK cinemas (chore/discover-cinemas) --body-file" \
  "$(run "" chore/discover-cinemas "New UK cinemas" "$stub_dir/report.md" https://run/3)"
check "a missing report is refused before any issue" "2|" \
  "$(STUB_LOG="$stub_dir/log"; : > "$STUB_LOG"; GH_REPO=o/movies GITHUB_SERVER_URL=http://forge PATH="$stub_dir:$PATH" \
      bash "$REPO_ROOT/scripts/ci/bot-branch-ready.sh" b t "$stub_dir/none.md" u >/dev/null 2>&1; echo "$?|$(cat "$STUB_LOG")")"

spec_summary
