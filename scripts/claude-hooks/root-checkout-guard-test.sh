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
# two Bash calls of one session overlapping (parallel tool calls, a session's subagents): each post judges its own pre
pre_post() { printf '{"session_id":"par","tool_use_id":"%s","tool_name":"Bash"}' "$2" | python3 "$guard" "bash-$1" --repo "$wt" 2>/dev/null; }
scratch_git "$root" checkout -q -f main 2>/dev/null; rm -f "$root/new.txt" # the root clean, on main, again
pre_post pre call-a; echo w >> "$root/a.txt"; pre_post pre call-b
check "a call's dirt is reported though another call of its session began after it" 2 "$(pre_post post call-a; echo $?)"
check "...and not blamed on that other call" 0 "$(pre_post post call-b; echo $?)"
check "a post with no pre for its session never blocks" 0 "$(printf '{"session_id":"none"}' | python3 "$guard" bash-post --repo "$wt"; echo $?)"

launch() { # <cwd> <command> -> the exit code of the bash-pre guard for that command run in that directory
  jq -nc --arg cwd "$1" --arg cmd "$2" '{session_id:"dev",cwd:$cwd,tool_name:"Bash",tool_input:{command:$cmd}}' |
    python3 "$guard" bash-pre --repo "$wt" 2>/dev/null; echo $?
}
printf '\033[36m▸\033[0m dev server\n'
check "sbt web/run in the root checkout is denied" 2 "$(launch "$root" 'sbt web/run')"
check "...with flags and an environment prefix" 2 "$(launch "$root" 'FOO=1 sbt -J-Xmx2500m web/run')"
check "scripts/dev-server.sh in the root checkout is denied" 2 "$(launch "$root" 'scripts/dev-server.sh')"
check "...from a subdirectory of it, through an absolute path" 2 "$(launch "$root/target" "$root/scripts/dev-server.sh")"
check "...and the fix names scripts/review-server" 1 "$(jq -nc --arg cwd "$root" '{cwd:$cwd,tool_input:{command:"sbt web/run"}}' |
  python3 "$guard" bash-pre --repo "$wt" 2>&1 | grep -c 'scripts/review-server')"
check "sbt web/run in a linked worktree is allowed" 0 "$(launch "$wt" 'sbt web/run')"
check "...and in a nested worktree" 0 "$(launch "$root/.claude/worktrees/agent" 'scripts/dev-server.sh')"
check "a command that cd's into a worktree first is allowed" 0 "$(launch "$root" "cd $wt && sbt web/run")"
check "scripts/review-server in the root checkout is allowed (it works in its own worktree)" 0 "$(launch "$root" 'scripts/review-server')"
check "an unrelated sbt command in the root checkout is allowed" 0 "$(launch "$root" 'sbt web/test')"

start() { # <cwd> -> how many times the session-start context names the root checkout
  printf '{"session_id":"s2","cwd":"%s"}' "$1" | python3 "$guard" session-start --repo "$wt" | grep -c 'started in the shared ROOT checkout'
}
printf '\033[36m▸\033[0m session start\n'
check "a session starting in the root checkout is told to make a worktree" 1 "$(start "$root")"
check "...also from a subdirectory of it" 1 "$(start "$root/target")"
check "a session starting in a linked worktree is told nothing" 0 "$(start "$wt")"
check "a session starting in a nested worktree is told nothing" 0 "$(start "$root/.claude/worktrees/agent")"
check "a session starting outside every checkout is told nothing" 0 "$(start "$testroot")"

if [ "$fails" -ne 0 ]; then echo "root-checkout-guard: $fails failed"; exit 1; fi
echo "root-checkout-guard: all checks passed"
