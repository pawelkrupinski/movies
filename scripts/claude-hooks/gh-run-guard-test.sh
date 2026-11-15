#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/gh-run-guard: `gh run rerun` is denied (exit 2); `gh run list|view|watch` runs but
# is ADVISED toward scripts/ci-status / agentctl ci-why / scripts/ci-wait; download, cancel, other gh calls and a
# `gh run` inside a heredoc body or a quoted string pass silently.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
guard="$REPO_ROOT/scripts/claude-hooks/gh-run-guard"
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}
run() { # <command> -> "<exit code> <stdout>"
  local out
  out=$(python3 -c 'import json, sys; print(json.dumps({"tool_name": "Bash", "tool_input": {"command": sys.argv[1]}}))' "$1" |
    python3 "$guard" 2>/dev/null)
  echo "$? $out"
}
code() { run "$1" | cut -d' ' -f1; }
advised() { run "$1" | grep -c 'scripts/ci-status\|ci-why\|ci-wait'; }

printf '\033[36m▸\033[0m denied\n'
check "gh run rerun" 2 "$(code 'gh run rerun 123 --failed')"
check "a rerun after a cd and &&" 2 "$(code 'cd ~/projects/movies && gh run rerun 123')"
check "gh by absolute path, in a for loop" 2 "$(code 'for r in 1 2; do /opt/homebrew/bin/gh run rerun $r; done')"

printf '\033[36m▸\033[0m advised, still runs\n'
check "gh run list: exit 0 and the ci-status advice" "0 1" "$(code 'gh run list --workflow Main --branch main --limit 4' | tr -d '\n') $(advised 'gh run list --workflow Main --branch main --limit 4')"
check "gh run view --json jobs" 1 "$(advised 'gh run view 42 --json jobs --jq .jobs')"
check "gh run view --log-failed piped to grep" 1 "$(advised 'gh run view 42 --log-failed 2>&1 | grep FAILED | head')"
check "gh run watch" 1 "$(advised 'gh run watch 42 --exit-status')"

printf '\033[36m▸\033[0m silent\n'
check "gh run download: no advice" "0 0" "$(code 'gh run download 42 -n x') $(advised 'gh run download 42 -n x')"
check "gh run cancel: no advice" 0 "$(advised 'gh run cancel 42')"
check "gh api: no advice" 0 "$(advised 'gh api repos/o/r/actions/runs')"
check "a gh run inside a heredoc body is not a call" "0 0" "$(code $'cat <<\'EOF\'\ngh run rerun 1\nEOF') $(advised $'cat <<\'EOF\'\ngh run rerun 1\nEOF')"
check "scripts/ci-status itself" 0 "$(advised 'scripts/ci-status --run 42')"
check "garbage input never blocks" 0 "$(echo 'not json' | python3 "$guard" >/dev/null 2>&1; echo $?)"

if [ "$fails" -eq 0 ]; then echo "all passed"; else echo "$fails failed"; exit 1; fi
