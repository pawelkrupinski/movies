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
out="$(cd "$repo" && EXPECTED_SEED='' "$tool" --files 'seed-test.sh' -- bash seed-test.sh 2>&1)"
check "an unbuilt caller names no seed" "yes" "$(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"
mkdir -p "$repo/worker/target/scala-3.x/zinc"
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

# Which src/ directories are test-side comes from the commit's build.sbt: a configuration extending Test (it, page)
# is; one extending Compile (fixtures: the replay wirings, the fixture-archive code) is code under test, so a fix
# living there stays at the parent. Copied over, the fix met its test in BEFORE and "proved" nothing.
mkdir -p "$repo/worker/src/fixtures/scala" "$repo/worker/src/page/scala"
cat > "$repo/build.sbt" <<'SBT'
lazy val PageTest = config("page") extend Test
lazy val Fixtures = config("fixtures") extend Compile
SBT
printf 'archive=broken\n' > "$repo/worker/src/fixtures/scala/Archive.scala"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a build with a fixtures configuration"
printf 'archive=fixed\n' > "$repo/worker/src/fixtures/scala/Archive.scala"
mkdir -p "$repo/worker/src/test/scala"
printf 'grep -q archive=fixed worker/src/fixtures/scala/Archive.scala\n' > "$repo/worker/src/test/scala/ArchiveSpec.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a fix in src/fixtures, its test in src/test"
# The test file is present on both sides (test -f), so only the fixtures file decides BEFORE's verdict
out="$(cd "$repo" && "$tool" -- 'test -f worker/src/test/scala/ArchiveSpec.sh && bash worker/src/test/scala/ArchiveSpec.sh' 2>&1)"; rc=$?
check "a fix in src/fixtures stays at the parent: yes/yes" "0 yes yes" \
  "$rc $(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p') $(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"
printf 'exit 1\n' > "$repo/worker/src/page/scala/PageSpec.sh"
printf 'archive=fixed again\n' > "$repo/worker/src/fixtures/scala/Archive.scala"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a test in src/page, a configuration extending Test"
out="$(cd "$repo" && "$tool" -- 'test ! -f worker/src/page/scala/PageSpec.sh || bash worker/src/page/scala/PageSpec.sh' 2>&1)"
check "...and a src/page test is carried over: FAIL-BEFORE yes" "yes" "$(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p')"

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

# ON CI (--ci): BEFORE is a commit of the parent with the test-side files laid over it, AFTER the commit, each run by
# scripts/ci-sbt (a stub here: it runs the "sbt command" as a script in that commit's tree, as CI runs it in a checkout
# of it), and a run that gives no verdict (ci-sbt's 2) is no verdict, never a "fails before".
mkdir -p "$base/bin"
cat > "$base/bin/ci-sbt" <<STUB
#!/usr/bin/env bash
c=""; while [ \$# -gt 0 ]; do case "\$1" in --commit) c=\$2; shift 2 ;; --log) l=\$2; shift 2 ;; --) shift; break ;; *) shift ;; esac; done
echo "\$c \$*" >> "$base/ci-runs"
[ -z "\${NO_VERDICT:-}" ] || exit 2
. "$HERE/scratch-git.sh"; t=\$(mktemp -d); scratch_git "\$PWD" archive "\$c" | tar -x -C "\$t"; (cd "\$t" && bash \$1) > "\$l" 2>&1; rc=\$?; rm -rf "\$t"; exit "\$((rc == 0 ? 0 : 1))"
STUB
chmod +x "$base/bin/ci-sbt"
export KINOWO_CI_SBT="$base/bin/ci-sbt"
printf 'echo ciao\n' > "$repo/greet.sh"
printf 'test "$(bash greet.sh)" = ciao\n' > "$repo/ci-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a fix proved on CI"
fix=$(scratch_git "$repo" rev-parse HEAD)
: > "$base/ci-runs"
out="$(cd "$repo" && "$tool" --ci -- scripts/sbt-warm ci-test.sh 2>&1)"; rc=$?
check "--ci: a fix with its failing-first test, yes/yes" "0 yes yes" \
  "$rc $(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p') $(printf '%s\n' "$out" | sed -n 's/^PASS-AFTER: //p')"
check "...two CI runs, the commit one of them" "2 1" "$(wc -l < "$base/ci-runs" | tr -d ' ') $(grep -c "^$fix ci-test.sh$" "$base/ci-runs")"
bc=$(grep -v "^$fix " "$base/ci-runs" | cut -d' ' -f1)
check "...BEFORE: the parent's production file" "echo hallo" "$(scratch_git "$repo" show "$bc:greet.sh")"
check "...with the commit's test-side file over it" "1" "$(scratch_git "$repo" show "$bc:ci-test.sh" | grep -c ciao)"
check "...on the commit's parent" "$(scratch_git "$repo" rev-parse "$fix^")" "$(scratch_git "$repo" rev-parse "$bc^")"
printf 'test -f greet.sh\n' > "$repo/ci-vacuous-test.sh"
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm "a vacuous test on CI"
out="$(cd "$repo" && "$tool" --ci -- 'scripts/sbt-warm "ci-vacuous-test.sh"' 2>&1)"; rc=$?
check "--ci: a test green at the parent is FAIL-BEFORE no, exit 1" "1 no" "$rc $(printf '%s\n' "$out" | sed -n 's/^FAIL-BEFORE: //p')"
out="$(cd "$repo" && NO_VERDICT=1 "$tool" --ci -- scripts/sbt-warm ci-vacuous-test.sh 2>&1)"; rc=$?
check "--ci: a run with no verdict is no verdict (exit 2), never FAIL-BEFORE yes" "2 0" \
  "$rc $(printf '%s\n' "$out" | grep -c '^FAIL-BEFORE: yes')"
out="$(cd "$repo" && "$tool" --ci -- 'scripts/sbt-warm a && rm -rf x' 2>&1)"; rc=$?
check "--ci refuses a command that is not one sbt-warm call (exit 2)" "2" "$rc"
# auto: on CI when the governor would hold it (its `gov route` says ci); here when it says local
cat > "$base/bin/agentctl" <<STUB
#!/usr/bin/env bash
# def cmd_gov def gov_route (what scripts/jvm-gate.sh looks for)
[ "\$1 \$2" = "gov route" ] && { echo "\$ROUTE: stub" >&2; [ "\$ROUTE" = ci ]; exit; }
exit 1
STUB
chmod +x "$base/bin/agentctl"
: > "$base/ci-runs"
out="$(cd "$repo" && KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$base/bin/agentctl" KINOWO_JVM_GATE_OS=Darwin ROUTE=ci KINOWO_GOV_CLASS=agent \
  "$tool" -- scripts/sbt-warm ci-vacuous-test.sh 2>&1)"
check "auto: held by the governor with a free lane, it runs on CI" "2" "$(wc -l < "$base/ci-runs" | tr -d ' ')"
: > "$base/ci-runs"
out="$(cd "$repo" && KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$base/bin/agentctl" KINOWO_JVM_GATE_OS=Darwin ROUTE=local KINOWO_GOV_CLASS=agent \
  "$tool" -- 'test -f ci-vacuous-test.sh' 2>&1)"
check "auto: a command that cannot go to CI runs here" "0" "$(wc -l < "$base/ci-runs" | tr -d ' ')"

check "no worktree is left in the scratch repository" "1" "$(scratch_git "$repo" worktree list | wc -l | tr -d ' ')"
check "no work directory is left" "0" "$(ls -A "$base/work" | wc -l | tr -d ' ')"

spec_summary
