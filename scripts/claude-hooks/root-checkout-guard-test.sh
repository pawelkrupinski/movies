#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/root-checkout-guard: a scratch clone with a linked worktree, and the hook's
# JSON fed to it the way Claude Code does. The root checkout's edits are denied, everyone else's allowed, and a Bash
# command that dirties the root (or switches its branch) is reported after it runs.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/scratch-git.sh
. "$REPO_ROOT/scripts/scratch-git.sh"
guard="$REPO_ROOT/scripts/claude-hooks/root-checkout-guard"

testroot="$(cd "$(mktemp -d)" && pwd -P)"; trap 'rm -rf "$testroot"' EXIT
export TMPDIR="$testroot/tmp"; mkdir -p "$TMPDIR" # the guard's per-session state lands here, not in a shared temp
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

root="$testroot/clone"; wt="$testroot/wt"
scratch_repo "$root" -b main
printf 'target/\nlocal.json\n' > "$root/.gitignore"; echo a > "$root/a.txt"
scratch_git "$root" add -A; scratch_git "$root" commit -qm base
scratch_git "$root" worktree add -q -b feature "$wt"
mkdir -p "$root/.claude/worktrees"; scratch_git "$root" worktree add -q -b nested "$root/.claude/worktrees/agent"

edit() { # <file path> -> the exit code of the edit guard for a Write to it
  printf '{"session_id":"s1","tool_name":"Write","tool_input":{"file_path":"%s"}}' "$1" |
    python3 "$guard" edit --repo "$wt" 2>/dev/null; echo $?
}
printf '\033[36m▸\033[0m edits\n'
check "a tracked file of the root checkout is denied" 2 "$(edit "$root/a.txt")"
check "a new file in a new directory of the root checkout is denied" 2 "$(edit "$root/web/src/New.scala")"
check "an ignored file of the root checkout is allowed" 0 "$(edit "$root/local.json")"
check "a file under an ignored directory of the root checkout is allowed" 0 "$(edit "$root/target/x/y.class")"
check "a file of a linked worktree is allowed" 0 "$(edit "$wt/a.txt")"
check "a file of a worktree nested under the root (.claude/worktrees) is allowed" 0 "$(edit "$root/.claude/worktrees/agent/a.txt")"
check "a file outside every checkout is allowed" 0 "$(edit "$testroot/elsewhere/notes.md")"
other="$testroot/other"; scratch_repo "$other"
check "another repository's main checkout is left alone" 0 "$(edit "$other/x.txt")"
check "the guard says why and where to work instead" 1 "$(printf '{"tool_input":{"file_path":"%s"}}' "$root/a.txt" |
  python3 "$guard" edit --repo "$wt" 2>&1 | grep -c 'ROOT checkout.*git worktree add')"
check "a guard that cannot read its input never blocks" 0 "$(echo 'not json' | python3 "$guard" edit --repo "$wt" 2>/dev/null; echo $?)"

bash_run() { # <session> <command run between the pre and post hooks> -> post's exit code, then its stderr
  local ev="{\"session_id\":\"$1\",\"tool_name\":\"Bash\"}"
  printf '%s' "$ev" | python3 "$guard" bash-pre --repo "$wt" || return
  (cd "$testroot" && eval "$2")
  printf '%s' "$ev" | python3 "$guard" bash-post --repo "$wt" 2> "$testroot/post.err"; echo $?
}
printf '\033[36m▸\033[0m bash\n'
check "a command touching only a worktree passes" 0 "$(bash_run b1 'echo x >> "$wt/a.txt"')"
check "a command writing an ignored root file passes" 0 "$(bash_run b2 'echo {} > "$root/local.json"')"
check "a command dirtying a root file is reported" 2 "$(bash_run b3 'echo y >> "$root/a.txt"')"
check "...naming the file" 1 "$(grep -c 'M a.txt' "$testroot/post.err")"
check "a root file already dirty before the command is not blamed on it" 0 "$(bash_run b4 'echo z >> "$wt/a.txt"')"
check "a command adding an untracked root file is reported" 2 "$(bash_run b5 'echo n > "$root/new.txt"')"
check "a command switching the root checkout's branch is reported" 2 "$(bash_run b6 'scratch_git "$root" checkout -q -b side 2>/dev/null')"
check "...naming both branches" 1 "$(grep -c 'from main to side' "$testroot/post.err")"
check "a post with no pre for its session never blocks" 0 "$(printf '{"session_id":"none"}' | python3 "$guard" bash-post --repo "$wt"; echo $?)"

if [ "$fails" -ne 0 ]; then echo "root-checkout-guard: $fails failed"; exit 1; fi
echo "root-checkout-guard: all checks passed"
