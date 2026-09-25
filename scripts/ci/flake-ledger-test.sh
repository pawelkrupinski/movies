#!/usr/bin/env bash
# flake-ledger.sh against a stub `gh` that records its calls and answers `issue list` from
# STUB_OPEN_ISSUE. Run: bash scripts/ci/flake-ledger-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m flake-ledger.sh\n'

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
printf 'unit tests\tdeploy.FooSpec\tFoo should work\t2/3\n' > "$stub_dir/unit.tsv"
printf 'page tests (webkit-phones-3-7)\twebkit-iphone-se tests/card.spec.ts\ttaps | opens\t1/3\n' > "$stub_dir/pw.tsv"
: > "$stub_dir/empty.tsv"

run() {  # run <open issue or ""> <args...> -> the gh calls after `issue list`
  export STUB_LOG="$stub_dir/log" STUB_OPEN_ISSUE="$1"; shift
  : > "$STUB_LOG"
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/flake-ledger.sh" "$@" >/dev/null
  echo "$?"
}
sha=0123456789abcdef0123456789abcdef01234567

check "no flaky test touches nothing" "0" \
  "$(run 7 --run-url u --sha $sha "$stub_dir/empty.tsv" "$stub_dir/missing.tsv" >/dev/null; grep -c . "$STUB_LOG")"
check "a flaky test with a ledger open comments on it" "issue comment 7 --body Flaky in 012345678 — https://run/1" \
  "$(run 7 --run-url https://run/1 --sha $sha "$stub_dir/unit.tsv" >/dev/null; grep -v '^issue list' "$STUB_LOG" | head -n 1 | cut -d'(' -f1 | sed 's/ $//')"
check "with none open it opens the ledger, assigned" "1" \
  "$(run "" --run-url u --sha $sha --assignee pawelkrupinski "$stub_dir/unit.tsv" >/dev/null; grep -c '^issue create --title Flaky test ledger --assignee pawelkrupinski' "$STUB_LOG")"
check "every file's rows land in one comment" "2" \
  "$(run 7 --run-url u --sha $sha "$stub_dir/unit.tsv" "$stub_dir/pw.tsv" >/dev/null; grep -c '^| .* | .*/3 |$' "$STUB_LOG")"
check "a '|' in a test name is escaped, not a column" "1" \
  "$(run 7 --run-url u --sha $sha "$stub_dir/pw.tsv" >/dev/null; grep -c 'taps \\| opens' "$STUB_LOG")"
check "no run url is refused" "2" "$(run 7 --sha $sha "$stub_dir/unit.tsv" 2>/dev/null)"

spec_summary
