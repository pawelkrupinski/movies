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

# Both trees are seeded from the caller's own build of the commit, where it has one; the newest older build otherwise
printf 'test "$KINOWO_SBT_WARM_SEED" = "$EXPECTED_SEED"; s=$?; test "$(bash greet.sh)" = hello && exit $s; exit 1\n' > "$repo/seed-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a test reading the seed"
out="$(cd "$repo" && EXPECTED_SEED= "$tool" --files 'seed-test.sh' -- bash seed-test.sh 2>&1)"
check "an unbuilt caller names no seed" "yes" "$(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"
mkdir -p "$repo/worker/target/scala-3.9.0/zinc"
out="$(cd "$repo" && EXPECTED_SEED="$(cd "$repo" && pwd -P)" "$tool" --files 'seed-test.sh' -- bash seed-test.sh 2>&1)"
check "a built caller at the commit seeds both trees" "yes" "$(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"
rm -rf "$repo/worker"

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

# Several arguments are one command, each kept whole: an sbt test filter (`-- -z "some case"`) must reach sbt as one
# argument, not be split by the shell into words that run the whole module.
printf 'echo hello world again\n' > "$repo/greet.sh"
printf 'test "$#" = 1 && test "$1" = "two words" && test "$(bash greet.sh)" = "hello world again"\n' > "$repo/args-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a test taking an argument with a space"
out="$(cd "$repo" && "$tool" -- bash args-test.sh "two words" 2>&1)"; rc=$?
check "a command's argument holding a space arrives whole: yes/yes" "0" "$rc"

# A test-side file whose path is not ASCII is still one: git quotes such a path ("k\303\266ln") unless told not to,
# and a quoted path matches no pattern (an identity slice pinned for DE "Filmhaus Kino Köln" was "nothing to prove").
mkdir -p "$repo/test/resources/köln"
printf 'echo hallo\n' > "$repo/greet.sh"
printf 'test "$(bash greet.sh)" = hallo\n' > "$repo/test/resources/köln/case-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a test under a non-ASCII path"
out="$(cd "$repo" && "$tool" -- bash test/resources/köln/case-test.sh 2>&1)"; rc=$?
check "a test-side file under a non-ASCII path is carried over: yes/yes" "0" "$rc"

# A run cut off by the deadline proves nothing: a hang is no "failed before". And the deadline ends the WHOLE run —
# the JVM an sbt test command forks, not just its shell — or it outlives fail-before, holding the worktree it removes.
# The test never ends on its own (its child blocks until killed), so fail-before gets past it only by its deadline:
# that, said, and the child gone prove it, never elapsed wall time (shell-spec.sh's HUNG STUBS).
printf 'sleep 86400 & echo $! >> "$GRANDCHILD"; wait\n' > "$repo/hang-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a test that hangs"
export GRANDCHILD="$base/grandchild"
: > "$GRANDCHILD"
out="$(cd "$repo" && KINOWO_FAIL_BEFORE_TIMEOUT=1 spec_bounded "$tool" -- bash hang-test.sh 2>&1)"; rc=$?
check "a run past the deadline: exit 2, no verdict, said" "2 0 1" \
  "$rc $(printf '%s\n' "$out" | grep -c '^FAIL-BEFORE: yes') $(printf '%s\n' "$out" | grep -c 'timed out')"
check "...and the run's own children are stopped with it" "0" "$(spec_still_running "$GRANDCHILD")"

check "no worktree is left in the scratch repository" "1" "$(scratch_git "$repo" worktree list | wc -l | tr -d ' ')"
check "no work directory is left" "0" "$(ls -A "$base/work" | wc -l | tr -d ' ')"

spec_summary
