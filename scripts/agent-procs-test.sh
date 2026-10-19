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
victim="" bystander=""
cleanup() {
  [ -n "$victim" ] && kill "$victim" 2>/dev/null
  [ -n "$bystander" ] && kill "$bystander" 2>/dev/null
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

{
  printf '%s 1 4096 0:01.00 05:00 /bin/zsh\n' "$$"
  printf '%s %s 800000 3:00.00 01:00:00 /jdk/bin/java -Xmx4g sbt-launch.jar\n' "$victim" "$$"
  printf '%s 1 90000 0:01.00 10:00 sleep 300\n' "$bystander"
  printf '9001 1 2500000 1:00.00 2-00:00:00 /jdk/bin/java -Xmx6g strayer\n'
  printf '9002 1 200000 0:10.00 2-00:00:00 /jdk/bin/java -Xmx1g small\n'
  printf '9003 1 100000 50:00.00 01:00:00 python3 busy.py\n'
  printf '9004 1 3000 0:01.00 01:00:00 bash scripts/identity-slice de --batch x\n'
} > "$work/ps.txt"
{
  printf 'p%s\nfcwd\nn%s\n' "$$" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s/subdir\n' "$victim" "$work/wt-orphan"
  printf 'p%s\nfcwd\nn%s\n' "$bystander" "$work/wt-other"
  printf 'p9004\nfcwd\nn%s\n' "$work/wt-live"
  printf 'p9001\nfcwd\nn/\n'
} > "$work/lsof.txt"
export AGENT_PROCS_PS_COMMAND="cat $work/ps.txt" AGENT_PROCS_CWD_COMMAND="cat $work/lsof.txt"

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
check "a >1 GB process outside every worktree is listed as heavy" "1" "$(printf '%s\n' "$heavy" | grep -c 'pid 9001 ')"
check "a lifetime-CPU hog outside every worktree is listed as heavy" "1" "$(printf '%s\n' "$heavy" | grep -c 'pid 9003 ')"
check "a small, cool process is not" "0" "$(printf '%s\n' "$heavy" | grep -c 'pid 9002 ')"
check "a process inside a worktree is not listed as heavy" "0" "$(printf '%s\n' "$heavy" | grep -c "pid $victim ")"

# kill: only the named worktree's processes, never the caller's own ancestors.
dry="$(cd "$work/wt-orphan" && "$TOOL" kill --mine --dry-run)"
check "kill --dry-run names the victim" "1" "$(printf '%s\n' "$dry" | grep -c "would kill pid $victim ")"
check "kill --dry-run kills nothing" "yes" "$(kill -0 "$victim" 2>/dev/null && echo yes || echo no)"
check "kill --dry-run never offers the caller's shell" "0" "$(printf '%s\n' "$dry" | grep -c "pid $$ ")"
(cd "$work/wt-orphan" && "$TOOL" kill --mine > /dev/null)
sleep 0.3
check "kill --mine ends the current worktree's process" "no" "$(kill -0 "$victim" 2>/dev/null && echo yes || echo no)"
check "...and leaves another worktree's process alone" "yes" "$(kill -0 "$bystander" 2>/dev/null && echo yes || echo no)"
# The caller's shell is spared even where `ps -p <pid>` cannot read a parent (a sandboxed agent shell): the
# ancestry comes from the real process table already read, not one ps call per hop. Real ps and lsof here.
mkdir -p "$work/bin"
printf '#!/bin/sh\nfor a in "$@"; do [ "$a" = -p ] && exit 1; done\nexec /bin/ps "$@"\n' > "$work/bin/ps"
chmod +x "$work/bin/ps"
(cd "$work/wt-orphan" && exec sleep 300) & victim=$!
survived="$(cd "$work/wt-orphan" && env -u AGENT_PROCS_PS_COMMAND -u AGENT_PROCS_CWD_COMMAND PATH="$work/bin:$PATH" \
  sh -c '"$1" kill --mine > /dev/null; echo survived' sh "$TOOL")"
sleep 0.3
check "kill --mine spares its calling shell when ps -p fails" "survived" "$survived"
check "...and still ends the worktree's own process" "no" "$(kill -0 "$victim" 2>/dev/null && echo yes || echo no)"
# ...nor the other commands of the caller's own pipeline: `agent-procs kill --mine | tail -3`, run from the worktree,
# SIGTERMed its `tail` (a sibling under the same shell, not an ancestor) and the pipeline exited 143 (2026-10-19).
(cd "$work/wt-orphan" && exec sleep 300) & victim=$!
piped="$(cd "$work/wt-orphan" && env -u AGENT_PROCS_PS_COMMAND -u AGENT_PROCS_CWD_COMMAND \
  sh -c '"$1" kill --mine | tail -1; echo "exit=$?"' sh "$TOOL")"
sleep 0.3
check "kill --mine spares the other commands of its own pipeline" "exit=0" "$(printf '%s\n' "$piped" | tail -1)"
check "...and still ends the worktree's own process" "no" "$(kill -0 "$victim" 2>/dev/null && echo yes || echo no)"
check "kill --worktree refuses a path that is no worktree" "1" \
  "$(cd "$root" && "$TOOL" kill --worktree "$work" > /dev/null 2>&1; echo $?)"
(cd "$root" && "$TOOL" kill --worktree "$work/wt-other" > /dev/null)
sleep 0.3
check "kill --worktree ends that worktree's process" "no" "$(kill -0 "$bystander" 2>/dev/null && echo yes || echo no)"

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
