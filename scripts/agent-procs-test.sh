#!/usr/bin/env bash
# agent-procs: the list/suggest logic against fixture ps / lsof output and a real scratch repository,
# the kill scoping against real throwaway processes, and prune against real worktrees.
# Run: bash scripts/agent-procs-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
TOOL="$REPO_ROOT/scripts/agent-procs"

printf '\033[36m▸\033[0m agent-procs\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
victim="" bystander="" session=""
cleanup() {
  [ -n "$victim" ] && kill "$victim" 2>/dev/null
  [ -n "$bystander" ] && kill "$bystander" 2>/dev/null
  [ -n "$session" ] && kill "$session" 2>/dev/null
  rm -rf "$work"
}
trap cleanup EXIT

origin="$work/origin.git"
root="$work/root"
scratch_repo "$origin" --bare
scratch_repo "$root"
echo base > "$root/f"
scratch_git "$root" add f
scratch_git "$root" commit -qm base
scratch_git "$root" remote add origin "$origin"
scratch_git "$root" push -q origin main
scratch_git "$root" fetch -q origin

# add_worktree <name> [commit-a-change]: a worktree on its own branch at main; with a second argument
# the branch gets one commit that origin/main does not have.
add_worktree() {
  scratch_git "$root" worktree add -q -b "$1" "$work/$1" main
  if [ -n "${2:-}" ]; then
    echo "$1" > "$work/$1/$1.txt"
    scratch_git "$work/$1" add "$1.txt"
    scratch_git "$work/$1" commit -qm "change $1"
  fi
}
add_worktree wt-merged
add_worktree wt-unmerged change
add_worktree wt-dirty
echo wip > "$work/wt-dirty/wip.txt"
add_worktree wt-live
scratch_git "$root" worktree lock --reason "claude agent live (pid $$ start Mon Oct 12 07:13:13 2026)" "$work/wt-live"
true & dead_pid=$!; wait "$dead_pid"
add_worktree wt-deadlock
scratch_git "$root" worktree lock --reason "claude agent dead (pid $dead_pid start Mon Oct 12 07:13:13 2026)" "$work/wt-deadlock"
add_worktree wt-orphan
# A worktree git cannot read (its .git link broken): its dirty and unmerged counts are unknown, never "0".
add_worktree wt-unreadable
echo wip > "$work/wt-unreadable/wip.txt"
echo "gitdir: $work/nowhere" > "$work/wt-unreadable/.git"
add_worktree wt-other
# A branch whose change reached origin/main by cherry-pick: a different commit, the same patch.
add_worktree wt-rebased change
scratch_git "$root" cherry-pick -n wt-rebased > /dev/null
scratch_git "$root" commit -qm "change wt-rebased (landed)"
scratch_git "$root" push -q origin main
scratch_git "$root" fetch -q origin

sleep 300 & victim=$!
sleep 300 & bystander=$!
sleep 300 & session=$!   # listed below as an agent session (`claude`) sitting in wt-orphan
sleep 300 & mcp=$!       # ...an MCP server that session started (its direct child, no shell between)
sleep 300 & toolshell=$! # ...and a tool call's shell of that session, running real work
sleep 300 & toolwork=$!
# Processes that exist only in the fake table get pids no kernel hands out (Linux caps them below 2^22, macOS below
# 100000). Fixed 9001..9004 collided with this test's own `sleep`s in a CI container, where real pids run low: the
# victim got pid 9003 and the bystander 9004, the fake rows replaced theirs, and four checks failed (Main #2390).
stray=4194401 small=4194402 hog=4194403 slice=4194404

{
  printf '%s 1 4096 0:01.00 05:00 /bin/zsh\n' "$$"
  printf '%s %s 800000 3:00.00 01:00:00 /jdk/bin/java -Xmx4g sbt-launch.jar\n' "$victim" "$$"
  printf '%s 1 90000 0:01.00 10:00 sleep 300\n' "$bystander"
  printf '%s 1 400000 9:00.00 02:00:00 claude --resume\n' "$session"
  printf '%s %s 60000 0:05.00 02:00:00 node /npx/mongodb-mcp-server\n' "$mcp" "$session"
  printf '%s %s 4096 0:00.10 01:00 /bin/zsh -c eval scripts/sbt-warm\n' "$toolshell" "$session"
  printf '%s %s 900000 1:00.00 01:00 /jdk/bin/java -Xmx4g sbt-launch.jar\n' "$toolwork" "$toolshell"
  printf '%s 1 2500000 1:00.00 2-00:00:00 /jdk/bin/java -Xmx6g strayer\n' "$stray"
  printf '%s 1 200000 0:10.00 2-00:00:00 /jdk/bin/java -Xmx1g small\n' "$small"
  printf '%s 1 100000 50:00.00 01:00:00 python3 busy.py\n' "$hog"
  printf '%s 1 3000 0:01.00 01:00:00 bash scripts/identity-slice de --batch x\n' "$slice"
} > "$work/ps.txt"
{
  printf 'p%s\nfcwd\nn%s\n' "$$" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s/subdir\n' "$victim" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$bystander" "$work/wt-other"
  printf 'p%s\nfcwd\nn%s\n' "$session" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$mcp" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$toolshell" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$toolwork" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$slice" "$work/wt-live"
  printf 'p%s\nfcwd\nn/\n' "$stray"
} > "$work/lsof.txt"
export AGENT_PROCS_PS_COMMAND="cat $work/ps.txt" AGENT_PROCS_CWD_COMMAND="cat $work/lsof.txt"

# alive_after_term <pid> -> "no" once a SIGTERMed process is gone, "yes" if it outlives a grace of up to 10 s. A fixed
# 0.3 s nap read the last pid the tool signalled before this shell had reaped it, on a loaded CI runner (Main #1936).
alive_after_term() {
  local _
  for _ in $(seq 1 100); do kill -0 "$1" 2>/dev/null || { echo no; return; }; command sleep 0.1; done
  echo yes
}

listing="$(cd "$root" && "$TOOL")"
suggestion() { printf '%s\n' "$listing" | sed -n "s|^\([A-Z-]*\) *$work/$1\$|\1|p"; }
block() { printf '%s\n' "$listing" | sed -n "\|^[A-Z-]* *$work/$1\$|,/^[A-Z-]* *\//p"; }

check "a root checkout on main and clean: no warning" "0" "$(printf '%s\n' "$listing" | grep -c 'WARNING: the root checkout')"
scratch_git "$root" checkout -q --detach
detached="$(cd "$work/wt-merged" && "$TOOL")"
scratch_git "$root" checkout -q main
check "a detached root checkout is warned about first, with the fix" "1" \
  "$(printf '%s\n' "$detached" | head -1 | grep -c "WARNING: the root checkout $root is on a DETACHED HEAD.*Fix: .*$root checkout main .*$root merge --ff-only origin/main")"
check "a merged, clean, unowned worktree is removable" "REMOVABLE" "$(suggestion wt-merged)"
check "a worktree with commits origin/main lacks is kept" "KEEP" "$(suggestion wt-unmerged)"
check "a worktree with uncommitted files is kept" "KEEP" "$(suggestion wt-dirty)"
check "a worktree locked by a live pid is kept" "KEEP" "$(suggestion wt-live)"
check "a clean merged worktree locked by a dead pid is removable" "REMOVABLE" "$(suggestion wt-deadlock)"
check "a worktree whose change landed rebased (patch-equal) is removable" "REMOVABLE" "$(suggestion wt-rebased)"
check "work processes with no live owner make it orphan-procs" "ORPHAN-PROCS" "$(suggestion wt-orphan)"
check "the root checkout is always kept" "KEEP" "$(suggestion root)"
check "a worktree git cannot read is kept, not judged clean and merged" "KEEP" "$(suggestion wt-unreadable)"
check "...and says why" "1" "$(block wt-unreadable | grep -c 'git could not read it')"
check "the unmerged count is reported" "1" "$(block wt-unmerged | sed -n 's/.*unmerged \([0-9]*\).*/\1/p' | head -1)"
check "the dirty count is reported" "1" "$(block wt-dirty | sed -n 's/.*dirty \([0-9]*\).*/\1/p' | head -1)"
check "the rebased worktree counts 0 unmerged" "0" "$(block wt-rebased | sed -n 's/.*unmerged \([0-9]*\).*/\1/p' | head -1)"
check "a live lock owner is named" "locked by pid $$ (alive)" \
  "$(block wt-live | sed -n 's/.*\(locked by pid [0-9]* (alive)\).*/\1/p' | head -1)"
check "a dead lock owner is named" "locked by pid $dead_pid (DEAD)" \
  "$(block wt-deadlock | sed -n 's/.*\(locked by pid [0-9]* (DEAD)\).*/\1/p' | head -1)"
check "a process with its cwd in a subdirectory is attributed to the worktree" "1" \
  "$(block wt-orphan | grep -c "pid $victim ")"
check "...with its RSS" "1" "$(block wt-orphan | grep -c "pid $victim .*rss   781MB")"
check "an identity-slice run is listed under its worktree" "1" "$(block wt-live | grep -c identity-slice)"
heavy="$(printf '%s\n' "$listing" | sed -n '/^Heavy processes/,$p')"
check "a >1 GB process outside every worktree is listed as heavy" "1" "$(printf '%s\n' "$heavy" | grep -c "pid $stray ")"
check "a lifetime-CPU hog outside every worktree is listed as heavy" "1" "$(printf '%s\n' "$heavy" | grep -c "pid $hog ")"
check "a small, cool process is not" "0" "$(printf '%s\n' "$heavy" | grep -c "pid $small ")"
check "a process inside a worktree is not listed as heavy" "0" "$(printf '%s\n' "$heavy" | grep -c "pid $victim ")"

# kill: only the named worktree's processes, never the caller's own ancestors.
dry="$(cd "$work/wt-orphan" && "$TOOL" kill --mine --dry-run)"
check "kill --dry-run names the victim" "1" "$(printf '%s\n' "$dry" | grep -c "would kill pid $victim ")"
check "kill --dry-run kills nothing" "yes" "$(kill -0 "$victim" 2>/dev/null && echo yes || echo no)"
check "kill --dry-run never offers the caller's shell" "0" "$(printf '%s\n' "$dry" | grep -c "pid $$ ")"
(cd "$work/wt-orphan" && "$TOOL" kill --mine > /dev/null)
check "kill --mine ends the current worktree's process" "no" "$(alive_after_term "$victim")"
toolwork_alive="$(alive_after_term "$toolwork")" # every signal has landed before the spared ones are asked after
check "...and leaves another worktree's process alone" "yes" "$(kill -0 "$bystander" 2>/dev/null && echo yes || echo no)"
# 2026-10-19: a subagent ran `kill --mine` after its worktree was removed, from the root checkout, where every
# session's `claude` and its MCP servers sit: it SIGTERMed one. An agent session or shell is never a victim, and
# --mine refuses the root checkout outright: no one owns what runs there.
check "...and never an agent session sitting in it" "yes" "$(kill -0 "$session" 2>/dev/null && echo yes || echo no)"
check "...nor the MCP servers it started, though they run node" "yes" "$(kill -0 "$mcp" 2>/dev/null && echo yes || echo no)"
check "...but still the work its tool calls' shells run" "no" "$toolwork_alive"
kill "$mcp" "$toolshell" 2>/dev/null
refused="$(cd "$root" && "$TOOL" kill --mine 2>&1; echo "exit=$?")"
check "kill --mine refuses the shared root checkout" "1" "$(printf '%s\n' "$refused" | grep -c 'root checkout')"
check "...exiting non-zero" "1" "$(printf '%s\n' "$refused" | grep -c '^exit=[1-9]')"
refused="$(cd "$work/wt-other" && "$TOOL" kill --worktree "$root" 2>&1; echo "exit=$?")"
check "...and so does kill --worktree naming it" "1 1" \
  "$(printf '%s\n' "$refused" | grep -c 'root checkout') $(printf '%s\n' "$refused" | grep -c '^exit=[1-9]')"
# The caller's shell is spared even where `ps -p <pid>` cannot read a parent (a sandboxed agent shell): the
# ancestry comes from the real process table already read, not one ps call per hop. Real ps and lsof here.
mkdir -p "$work/bin"
printf '#!/bin/sh\nfor a in "$@"; do [ "$a" = -p ] && exit 1; done\nexec /bin/ps "$@"\n' > "$work/bin/ps"
chmod +x "$work/bin/ps"
(cd "$work/wt-orphan" && exec sleep 300) & victim=$!
survived="$(cd "$work/wt-orphan" && env -u AGENT_PROCS_PS_COMMAND -u AGENT_PROCS_CWD_COMMAND PATH="$work/bin:$PATH" \
  sh -c '"$1" kill --mine > /dev/null; echo survived' sh "$TOOL")"
check "kill --mine spares its calling shell when ps -p fails" "survived" "$survived"
check "...and still ends the worktree's own process" "no" "$(alive_after_term "$victim")"
# ...nor the other commands of the caller's own pipeline: `agent-procs kill --mine | tail -3`, run from the worktree,
# SIGTERMed its `tail` (a sibling under the same shell, not an ancestor) and the pipeline exited 143 (2026-10-19).
(cd "$work/wt-orphan" && exec sleep 300) & victim=$!
piped="$(cd "$work/wt-orphan" && env -u AGENT_PROCS_PS_COMMAND -u AGENT_PROCS_CWD_COMMAND \
  sh -c '"$1" kill --mine | tail -1; echo "exit=$?"' sh "$TOOL")"
check "kill --mine spares the other commands of its own pipeline" "exit=0" "$(printf '%s\n' "$piped" | tail -1)"
check "...and still ends the worktree's own process" "no" "$(alive_after_term "$victim")"
check "kill --worktree refuses a path that is no worktree" "1" \
  "$(cd "$root" && "$TOOL" kill --worktree "$work" > /dev/null 2>&1; echo $?)"
(cd "$root" && "$TOOL" kill --worktree "$work/wt-other" > /dev/null)
check "kill --worktree ends that worktree's process" "no" "$(alive_after_term "$bystander")"

# prune: a plan without --yes, then the removal — worktrees and fully merged branches only.
(cd "$root" && "$TOOL" prune > /dev/null)
check "prune without --yes removes nothing" "yes" "$([ -d "$work/wt-merged" ] && echo yes || echo no)"
: > "$work/ps.txt"; : > "$work/lsof.txt"
(cd "$root" && "$TOOL" prune --yes > /dev/null)
gone() { [ -d "$work/$1" ] && echo kept || echo gone; }
has_branch() { scratch_git "$root" rev-parse --verify -q "refs/heads/$1" > /dev/null && echo yes || echo no; }
check "prune removes a merged worktree" "gone" "$(gone wt-merged)"
check "...and its branch" "no" "$(has_branch wt-merged)"
check "prune removes a worktree whose lock owner is dead" "gone" "$(gone wt-deadlock)"
check "prune removes a rebased-equal worktree and its branch" "gone no" "$(gone wt-rebased) $(has_branch wt-rebased)"
check "prune removes the now process-free orphan worktree" "gone" "$(gone wt-orphan)"
check "prune keeps the unmerged worktree and branch" "kept yes" "$(gone wt-unmerged) $(has_branch wt-unmerged)"
check "prune keeps the dirty worktree" "kept" "$(gone wt-dirty)"
check "prune keeps the worktree git cannot read, and its uncommitted file" "kept yes" \
  "$(gone wt-unreadable) $([ -f "$work/wt-unreadable/wip.txt" ] && echo yes || echo no)"
check "prune keeps the live-locked worktree" "kept" "$(gone wt-live)"
check "prune keeps the root checkout and main" "kept yes" "$(gone root) $(has_branch main)"

spec_summary
