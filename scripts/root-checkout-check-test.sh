#!/usr/bin/env bash
# root-checkout-check against a scratch clone: silent while the root is on main and clean, a warning naming the state
# and the fix when it is detached, on another branch or dirty -- read from a worktree as from the root.
# Run: bash scripts/root-checkout-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
TOOL="$REPO_ROOT/scripts/root-checkout-check"

printf '\033[36m▸\033[0m root-checkout-check\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
root="$work/root"
scratch_repo "$root"
echo base > "$root/f"; scratch_git "$root" add f; scratch_git "$root" commit -qm base
echo two > "$root/f"; scratch_git "$root" commit -qam two
scratch_git "$root" worktree add -q -b mine "$work/mine" main

out=$("$TOOL" "$work/mine"); status=$?
check "a root on main and clean: silent" "0 ''" "$status '$out'"

scratch_git "$root" checkout -q --detach HEAD~1
out=$("$TOOL" "$work/mine"); status=$?
check "a detached root is warned about, from a worktree" "1" "$status"
check "...naming the state" "1" "$(printf '%s' "$out" | grep -c "root checkout $root is on a DETACHED HEAD")"
check "...and the fix" "1" "$(printf '%s' "$out" | grep -c "Fix: git -C $root checkout main && git -C $root merge --ff-only origin/main")"
check "...and the root is left as it was" "" "$(scratch_git "$root" symbolic-ref --quiet HEAD)"

scratch_git "$root" checkout -q -b elsewhere
out=$("$TOOL" "$root"); status=$?
check "a root on another branch is warned about" "1 1" "$status $(printf '%s' "$out" | grep -c "on branch 'elsewhere', not main")"

scratch_git "$root" checkout -q main
echo wip > "$root/f"
out=$("$TOOL" "$root"); status=$?
check "a dirty root is warned about" "1 1" "$status $(printf '%s' "$out" | grep -c 'DIRTY (1 path(s) changed')"
check "...and the fix waits for its owner" "1" "$(printf '%s' "$out" | grep -c 'ask who owns the changes')"

"$TOOL" "$work" > /dev/null 2>&1
check "outside a clone: 2" "2" "$?"
spec_summary
