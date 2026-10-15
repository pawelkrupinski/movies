#!/usr/bin/env bash
#
# scripts/fail-before against a scratch repository: a commit adding a failing-first test plus its fix is
# yes/yes; a commit whose test already passes at the parent is "FAIL-BEFORE: no"; one with no test-side
# file is refused; and no worktree is left behind.
#
#   scripts/fail-before-test.sh
#
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/scratch-git.sh"
. "$HERE/shell-spec.sh"

printf '\033[36m▸\033[0m fail-before\n'
tool="$HERE/fail-before"
base="$(mktemp -d)"
trap 'rm -rf "$base"' EXIT
export KINOWO_FAIL_BEFORE_DIR="$base/work"
repo="$base/repo"
scratch_repo "$repo"
printf 'echo broken\n' > "$repo/greet.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm base

# A fix, with the test that asserts it
printf 'echo hello\n' > "$repo/greet.sh"
printf 'test "$(bash greet.sh)" = hello\n' > "$repo/greet-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm fix
out="$(cd "$repo" && "$tool" -- bash greet-test.sh 2>&1)"; rc=$?
check "a fix with its failing-first test: exit 0" "0" "$rc"
check "...FAIL-BEFORE yes" "yes" "$(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p')"
check "...PASS-AFTER yes" "yes" "$(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"

# A test that already passes at the parent
printf 'test -f greet.sh\n' > "$repo/exists-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a vacuous test"
out="$(cd "$repo" && "$tool" -- bash exists-test.sh 2>&1)"; rc=$?
check "a test green at the parent: exit 1" "1" "$rc"
check "...FAIL-BEFORE no" "no" "$(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p')"

# A commit with no test-side file is refused, unless --files names one
printf 'echo hello world\n' > "$repo/greet.sh"
printf 'marker\n' > "$repo/probe.txt"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "production only"
out="$(cd "$repo" && "$tool" -- true 2>&1)"; rc=$?
check "a commit with no test-side file: exit 2" "2" "$rc"
out="$(cd "$repo" && "$tool" --files 'probe.txt' -- 'test "$(bash greet.sh)" = "hello world"' 2>&1)"; rc=$?
check "--files redefines test-side: only probe.txt is carried over, yes/yes" "0" "$rc"

check "no worktree is left in the scratch repository" "1" "$(scratch_git "$repo" worktree list | wc -l | tr -d ' ')"
check "no work directory is left" "0" "$(ls -A "$base/work" | wc -l | tr -d ' ')"

spec_summary
