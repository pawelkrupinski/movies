#!/usr/bin/env bash
# run-script-tests over a throwaway tree of fake self-tests: what it discovers (and what a
# `# script-test: <tag>` header keeps out of the default run), its exit status and table, a path
# filter, --changed against a base commit, the per-test timeout, and the bound on how many tests run at once.
# Run: bash scripts/run-script-tests-test.sh
# bash-floor: names the constructs bash 3.2 lacks as data (the floor's own cases), never runs one
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m run-script-tests\n'
runner="$REPO_ROOT/scripts/run-script-tests"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
tree="$work/repo"

put() { # <path> <content>: a file in the scratch tree
  mkdir -p "$tree/$(dirname "$1")"; printf '%s\n' "$2" > "$tree/$1"; }
put scripts/a.sh 'echo tool a'
put scripts/a-test.sh 'echo a passes'
put scripts/ci/b_test.py 'print("b passes")'
put scripts/ci/lib.sh 'echo a library'
put scripts/c-test.sh "$(printf '%s\n' '. scripts/ci/lib.sh' 'echo "  ✗ an early case"; echo "     expected: yes"; echo "     actual:   no"' 'for n in $(seq 1 100); do echo "line $n"; done' 'echo boom; exit 1')"
export RUN_SCRIPT_TESTS_LOG_DIR="$work/logs"
put scripts/d-test.sh "$(printf '%s\n' '#!/usr/bin/env bash' '# script-test: manual needs a live server' 'exit 1')"
put .github/actions/x/e-test.sh 'exit 0'
put android/scripts/f-test.sh 'exit 0'
put scripts/helper.sh 'exit 1'
put scripts/test_old.py 'raise SystemExit(1)'
put scripts/node_modules/pkg/g-test.sh 'exit 1'
put docs/h-test.sh 'exit 1'
scratch_repo "$tree"
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm base
base="$(scratch_git "$tree" rev-parse HEAD)"

run() { (cd "$work" && spec_bounded "${SCRIPT_TESTS_BASH:-bash}" "$runner" --root "$tree" "$@") > "$work/out" 2>&1; echo $?; }
listed() { run --list "$@" >/dev/null; cut -f1 "$work/out" | tr '\n' ' ' | sed 's/ $//'; }

check "discovers *-test.sh and *_test.py under the script roots, sorted, untagged only" \
  ".github/actions/x/e-test.sh android/scripts/f-test.sh scripts/a-test.sh scripts/c-test.sh scripts/ci/b_test.py" "$(listed)"
check "--all lists the tagged ones too, with their tag and reason" \
  "scripts/d-test.sh	manual	needs a live server" "$(run --list --all >/dev/null; grep d-test "$work/out")"

check "a failing test fails the run" "1" "$(run)"
check "...each test has a row in the table" "4 1" "$(grep -c '^PASS' "$work/out") $(grep -c '^FAIL' "$work/out")"
check "...the failing one named" "1" "$(grep -c '^FAIL .*scripts/c-test.sh' "$work/out")"
check "...with its output" "1" "$(grep -c '^ *boom' "$work/out")"
check "...and the total" "1" "$(grep -c '^1 of 5 failed' "$work/out")"
# the report shows a failing log's last 60 lines; the whole log is kept, its path said (a Linux flake's failures were
# lost past those 60 lines, 2026-10-18)
kept=$(sed -n 's/^ *(whole log: \(.*\))$/\1/p' "$work/out")
check "...its whole log kept where the report says" "line 1|boom" \
  "$( [ -f "$kept" ] && { grep -x 'line 1' "$kept"; tail -1 "$kept"; } | paste -sd'|' -)"
# ...and every failing case in it, however far above those 60 lines (Order independence #94: identity-slice-test's five
# failures were printed before its last 60 lines, and its whole log stayed on the CI runner)
check "...its failing cases named from the whole log, with what they expected" "2" \
  "$(grep -cE '^ *(✗ an early case|expected: yes)$' "$work/out")"
check "the tagged test never runs by default" "0" "$(grep -c d-test "$work/out")"

check "a directory filter runs only what is under it" "0" "$(run scripts/ci)"
check "...that is, one test" "1 0" "$(grep -c '^PASS .*b_test.py' "$work/out") $(grep -c 'a-test' "$work/out")"
check "a name filter picks by substring" "scripts/a-test.sh" "$(listed a-test)"
check "a glob filter picks by glob" "scripts/a-test.sh scripts/c-test.sh" "$(listed 'scripts/[ac]-*')"
check "a filter matching nothing fails, never passes silently" "1" "$(run no-such-test)"
check "--tag runs only that tag's tests" "1" "$(run --tag manual)"
check "...the tagged one, failing" "1" "$(grep -c '^FAIL .*d-test' "$work/out")"

# --changed <base>: a test whose own file, whose tool (the same name without -test/_test) or a file it names changed.
echo 'echo tool a, edited' >> "$tree/scripts/a.sh"; scratch_git "$tree" commit -qam 'edit tool a'
check "--changed: a tool's edit selects its test" "scripts/a-test.sh" "$(listed --changed "$base")"
echo '# edited' >> "$tree/scripts/ci/lib.sh"
check "--changed: an uncommitted edit of a file a test names selects that test too" \
  "scripts/a-test.sh scripts/c-test.sh" "$(listed --changed "$base")"
scratch_git "$tree" checkout -q -- scripts/ci/lib.sh
put scripts/ci/b.py 'print(1)'
check "--changed: a new python tool selects its _test.py" "scripts/a-test.sh scripts/ci/b_test.py" "$(listed --changed "$base")"
rm "$tree/scripts/ci/b.py"
head_now="$(scratch_git "$tree" rev-parse HEAD)"
put docs/readme.md 'unrelated'
check "--changed: an unrelated change selects nothing, and passes" "0" "$(run --changed "$head_now")"
check "...saying so" "1" "$(grep -c 'no script tests selected' "$work/out")"
rm "$tree/docs/readme.md"
check "--changed: a base that is not a commit fails" "2" "$(run --changed no-such-ref)"

# --changed follows what a test's tool sources or imports, however deep: a library's edit selects every test reaching
# it, though none of them names it.
mkdir -p "$tree/scripts/lib"
put scripts/k.sh "$(printf '%s\n' '# shellcheck source=scripts/lib/shared.sh' '. "$(dirname "${BASH_SOURCE[0]}")/lib/shared.sh"' 'shared')"
put scripts/k-test.sh 'bash scripts/k.sh'
put scripts/lib/shared.sh "$(printf '%s\n' 'source "$(dirname "${BASH_SOURCE[0]}")/deeper.sh"' 'shared() { deeper; }')"
put scripts/lib/deeper.sh 'deeper() { echo deep; }'
put scripts/ci/m.py "$(printf '%s\n' 'import os' 'from takes_lib import take' 'print(take())')"
put scripts/ci/m_test.py "$(printf '%s\n' 'import m')"
put scripts/ci/n_test.py "$(printf '%s\n' 'import sys' 'import takes_lib')"
put scripts/ci/takes_lib.py 'def take(): return 1'
put scripts/ci/unrelated_test.py 'print(1)'
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm libraries
libs="$(scratch_git "$tree" rev-parse HEAD)"
echo '# edited' >> "$tree/scripts/lib/shared.sh"
check "--changed: a library a test's tool sources selects that test" "scripts/k-test.sh" "$(listed --changed "$libs")"
scratch_git "$tree" checkout -q -- scripts/lib/shared.sh
echo '# edited' >> "$tree/scripts/lib/deeper.sh"
check "...and one that library sources in turn" "scripts/k-test.sh" "$(listed --changed "$libs")"
scratch_git "$tree" checkout -q -- scripts/lib/deeper.sh
echo '# edited' >> "$tree/scripts/ci/takes_lib.py"
check "--changed: a module a test or its tool imports selects it" "scripts/ci/m_test.py scripts/ci/n_test.py" "$(listed --changed "$libs")"
scratch_git "$tree" checkout -q -- scripts/ci/takes_lib.py
# --paths FILE --list: --changed's selection for the paths a file lists, with no git range (the land queue asks it which
# tests two changes share: scripts/hooks/pre-push --script-test-plan)
printf '%s\n' scripts/lib/deeper.sh scripts/ci/takes_lib.py > "$work/paths"
check "--paths: the tests the listed paths select, as --changed would" \
  "scripts/ci/m_test.py scripts/ci/n_test.py scripts/k-test.sh" "$(listed --paths "$work/paths")"
: > "$work/paths"
check "...none for no path" "" "$(listed --paths "$work/paths")"
check "...a file it cannot read fails" "2" "$(run --list --paths "$work/no-such-file")"
check "...and a run (not --list) is a usage error" "64" "$(run --paths "$work/paths")"

# READS: a test holding the repository's own files to a rule names none of them (download_artifact_layout_test.py over
# every workflow, Tool self-tests #259-263), so it says what it reads; and an extensionless tool's edit selects a test
# naming it by its directory and name (scripts/hooks/pre-push and pre-push-gate-test.sh, #71-72).
put scripts/ci/layout_test.py "$(printf '%s\n' '"""Every workflow to a rule."""' '# script-test-reads: .github/workflows/* .github/actions/*' 'print(1)')"
put scripts/ci/floor_test.py "$(printf '%s\n' '# script-test-reads: *.py' 'print(1)')"
put scripts/ci/late_test.py "$(printf '%s\n' $(seq 1 40) '# script-test-reads: .github/workflows/*' 'print(1)')"
put scripts/hooks/gate.sh 'exit 0'
put scripts/hooks/gate-test.sh 'echo runs scripts/hooks/gate.sh'
put scripts/hooks/gatekeeper-test.sh 'echo "the hooks/gatekeeper and a gate"'
put scripts/hooks/gate-plan-test.sh 'echo "runs $REPO_ROOT/scripts/hooks/gate --plan"'
put scripts/hooks/gate 'exit 0'
put .github/workflows/main.yml 'on: push'
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm readers
readers="$(scratch_git "$tree" rev-parse HEAD)"
echo '# edited' >> "$tree/.github/workflows/main.yml"
check "--changed: a workflow's edit selects the test declaring it reads every workflow" "scripts/ci/layout_test.py" \
  "$(listed --changed "$readers")"
check "...never one whose declaration is past its first 40 lines" "0" "$(grep -c late_test "$work/out")"
echo '# edited' >> "$tree/scripts/hooks/gate.sh"
check "--readers: of what --changed selects, only the tests a declared glob selects (the hook's --verify-only)" \
  "scripts/ci/layout_test.py scripts/hooks/gate-test.sh|scripts/ci/layout_test.py" \
  "$(listed --changed "$readers")|$(listed --changed "$readers" --readers)"
check "...and --readers without --changed is a usage error" "64" "$(run --readers)"
scratch_git "$tree" checkout -q -- .github/workflows/main.yml scripts/hooks/gate.sh
mkdir -p "$tree/tools/x" && put tools/x/deep.py 'print(1)'
check "--changed: a .py anywhere selects the test reading every .py" "scripts/ci/floor_test.py" "$(listed --changed "$readers")"
rm -r "$tree/tools"
check "--reads: every glob a test declares, once each, sorted" "*.py .github/actions/* .github/workflows/*" \
  "$(run --reads > /dev/null; tr '\n' ' ' < "$work/out" | sed 's/ $//')"
echo '# edited' >> "$tree/scripts/hooks/gate"
check "--changed: an extensionless tool's edit selects the tests naming it by directory and name, not a longer name" \
  "scripts/hooks/gate-plan-test.sh scripts/hooks/gate-test.sh" "$(listed --changed "$readers")"
scratch_git "$tree" checkout -q -- scripts/hooks/gate

# A test never sees the CI run it is in (GITHUB_SERVER_URL in fleetkit-access-test.sh, Tool self-tests #1-3; the push
# event in commit-rules-ci-test.sh, #21; GITHUB_REF in identity-drift-check-test.sh, #172): what a laptop gives it.
put scripts/runctx-test.sh 'for v in GITHUB_REF GITHUB_EVENT_NAME GITHUB_SERVER_URL GITHUB_SHA GITHUB_REPOSITORY GITHUB_RUN_ID GITHUB_ENV; do eval "[ -z \"\${$v+set}\" ]" || { echo "$v leaked"; exit 1; }; done'
check "a test sees none of the CI run's ref, event, commit, forge or run" "0" \
  "$(GITHUB_REF=refs/heads/feature GITHUB_EVENT_NAME=push GITHUB_SERVER_URL=https://github.com GITHUB_SHA=abc \
     GITHUB_REPOSITORY=o/r GITHUB_RUN_ID=1 GITHUB_ENV=/dev/null run runctx-test)"
check "...the variables a tool reads, the test's to set" "0" "$(grep -c leaked "$work/out")"
rm "$tree/scripts/runctx-test.sh"

# The timeout: a hung test is killed at its bound and FAILS as a timeout. The test never ends on its own (it records
# its pid and blocks until killed), so the runner gets past it only by its bound: that, reported, and no test left
# running prove it, never elapsed wall time (shell-spec.sh's HUNG STUBS).
put scripts/slow-test.sh "echo \$\$ >> '$work/hung'; exec sleep 86400"
: > "$work/hung"
code="$(run --timeout 1 slow-test)"
check "a hung test is cut off at its timeout and fails" "1" "$code"
check "...reported as a timeout, and not left running" "1 0" "$(grep -c '^TIMEOUT .*slow-test' "$work/out") $(spec_still_running "$work/hung")"

# The no-progress bound (Tool self-tests #2857: four tests, passing case after case under a load of 160, cut at a 600 s
# wall clock). A hung test -- no output, no CPU, nothing runnable -- is ended at --stall and reported STALLED, saying
# what its process group was stuck on; one still working past --stall is never cut, however long it takes.
: > "$work/hung"
code="$(KINOWO_STALL_POLL_SECONDS=1 run --stall 2 slow-test)"
check "a test making no progress is ended at its stall bound and fails" "1" "$code"
check "...reported as STALLED, with the stall report, and not left running" "1 1 0" \
  "$(grep -c '^STALLED .*slow-test' "$work/out") $(grep -c 'stall report: process group' "$work/out") $(spec_still_running "$work/hung")"
rm "$tree/scripts/slow-test.sh"
# busy: a line, then a wait on what the line's writer hands it, over and over -- progress at every poll, for longer
# than the bound (its count, not a clock, ends it). The bound is FOUR polls, not two (landing check 45006463015, a
# loaded runner under the floor bash 3.2: its `-nt` reads whole seconds, so a written log can look unwritten for one
# poll, and an oversleeping `sleep 0.2` loop for another -- two quiet polls ended a test that was working).
# shellcheck disable=SC2016 # expanded by the fake test, not here
put scripts/busy-test.sh 'for n in $(seq 1 40); do echo "case $n"; sleep 0.2; done'
check "a test still making progress past its stall bound passes" "0" "$(KINOWO_STALL_POLL_SECONDS=1 run --stall 4 busy-test)"
check "...as PASS" "1" "$(grep -c '^PASS .*busy-test' "$work/out")"
rm "$tree/scripts/busy-test.sh"

# The pool: never more than --jobs tests at once. Each records how many were running while it ran.
mkdir -p "$work/running"
for n in 1 2 3 4; do
  # shellcheck disable=SC2016 # expanded by the fake test, not here
  put "scripts/par/p$n-test.sh" 'touch "$PAR_DIR/$$"; sleep 1; ls "$PAR_DIR" | wc -l >> "$PAR_DIR.max"; rm "$PAR_DIR/$$"'
done
for jobs in 1 2; do
  rm -f "$work/running.max"
  PAR_DIR="$work/running" run --jobs "$jobs" scripts/par > /dev/null
  check "--jobs $jobs: at most $jobs at once" "$jobs" "$(sort -n "$work/running.max" | tail -1 | tr -d ' ')"
done

# Longest first: tests start in descending order of their last recorded seconds; one with none starts first, the larger
# file before the smaller. --jobs 1 makes the start order the run order; each test appends its own name. Recorded
# seconds are seeded, never waited for.
mkdir -p "$tree/scripts/ord"
for n in a b c; do
  # shellcheck disable=SC2016 # expanded by the fake test, not here
  put "scripts/ord/$n-test.sh" 'echo "$0" | sed "s|.*/||" >> "$ORD_LOG"'
done
# shellcheck disable=SC2016 # expanded by the fake test, not here
put scripts/ord/d-test.sh "$(printf '%s\n' 'echo "$0" | sed "s|.*/||" >> "$ORD_LOG"' '# padding that makes d the larger file')"
order() { : > "$work/ord.log"; ORD_LOG="$work/ord.log" run --jobs 1 scripts/ord > /dev/null; paste -sd' ' - < "$work/ord.log"; }
export RUN_SCRIPT_TESTS_DURATIONS="$work/durations.tsv"
rm -f "$RUN_SCRIPT_TESTS_DURATIONS"
check "no recorded times: the larger file first, then by path" "d-test.sh a-test.sh b-test.sh c-test.sh" "$(order)"
check "...and the first run's record holds each test once (no cache file to merge with yet)" "4 4" \
  "$(wc -l < "$RUN_SCRIPT_TESTS_DURATIONS" | tr -d ' ') $(cut -f1 "$RUN_SCRIPT_TESTS_DURATIONS" | sort -u | wc -l | tr -d ' ')"
printf '%s\t%s\n' scripts/ord/a-test.sh 5 scripts/ord/b-test.sh 90 scripts/ord/c-test.sh 40 scripts/ord/gone-test.sh 7 > "$RUN_SCRIPT_TESTS_DURATIONS"
check "recorded times: the slowest first, a test with none before them all" "d-test.sh b-test.sh c-test.sh a-test.sh" "$(order)"
check "...a run records what its tests took, keeping the rows of tests it did not run" "4 1 1" \
  "$(grep -c 'scripts/ord/.-test.sh' "$RUN_SCRIPT_TESTS_DURATIONS" | tr -d ' ') $(grep -c 'gone-test.sh	7' "$RUN_SCRIPT_TESTS_DURATIONS") $(grep -c 'ord/b-test.sh	[0-9]$' "$RUN_SCRIPT_TESTS_DURATIONS")"
check "...and no temporary file is left beside it" "0" "$(find "$work" -maxdepth 1 -name 'durations.tsv.*' | wc -l | tr -d ' ')"
rm -rf "$tree/scripts/ord"; unset RUN_SCRIPT_TESTS_DURATIONS

# The Python floor: a python3 older than every script supports fails the run, loudly, before any test.
mkdir -p "$work/oldpy"
printf '%s\n' '#!/bin/sh' 'case "$1" in -V|--version) echo "Python 3.8.10" ;; -c) exit 1 ;; *) exit 0 ;; esac' > "$work/oldpy/python3"
chmod +x "$work/oldpy/python3"
check "a python3 below the floor fails a run with Python tests" "1" "$(PATH="$work/oldpy:$PATH" run scripts/ci)"
check "...saying which python3 and which floor" "1" "$(grep -c 'Python 3.8.10.*3\.9 or newer' "$work/out")"
check "...and runs no test" "0" "$(grep -c '^PASS' "$work/out")"
check "a run with no Python test does not ask" "0" "$(PATH="$work/oldpy:$PATH" run scripts/a-test.sh)"

# --bash <path>: each .sh test run by that bash, and the tools it starts through `#!/usr/bin/env bash` too.
real_bash="$(command -v bash)"
printf '#!/bin/sh\necho "$*" >> "%s/via-fake"\nexec %s "$@"\n' "$work" "$real_bash" > "$work/fake-bash"
chmod +x "$work/fake-bash"
put scripts/env/tool-test.sh 'scripts/env/tool'
printf '#!/usr/bin/env bash\necho tool ran\n' > "$tree/scripts/env/tool"; chmod +x "$tree/scripts/env/tool"
check "--bash: the run passes under it" "0" "$(run --bash "$work/fake-bash" scripts/env)"
check "...the test run by it, and the tool the test starts through env bash" "scripts/env/tool-test.sh|scripts/env/tool" \
  "$(paste -sd'|' - < "$work/via-fake")"
check "...a --bash that is no executable is a usage error" "64" "$(run --bash "$work/no-such-bash" scripts/env)"
rm -rf "$tree/scripts/env"

# --bash with --changed: THE FLOOR'S OWN SCOPE (the pre-push hook's floor run, which re-ran every selected test under
# 3.2 and cost a landing +70 s). The bash the change touches is parsed by the floor and its added lines held to the
# bash-4-only constructs; the tests re-run are the self-tests of the bash tools it touches and of those sourcing them --
# never a test selected only by a name it mentions or a glob it reads, nor one of a Python tool.
printf '#!/bin/sh\n[ "$1" = -n ] && grep -q FLOOR_CANNOT_PARSE "$2" && { echo "$2: syntax error" >&2; exit 2; }\nexec %s "$@"\n' \
  "$real_bash" > "$work/floor-bash"; chmod +x "$work/floor-bash"
put scripts/fl/tool.sh 'echo tool'
put scripts/fl/tool-test.sh 'bash scripts/fl/tool.sh'
put scripts/fl/mention-test.sh 'echo "reads what scripts/fl/tool.sh prints"'
put scripts/fl/flib.sh 'echo lib'
put scripts/fl/user.sh "$(printf '%s\n' '. "$(dirname "$0")/flib.sh"')"
put scripts/fl/user-test.sh 'bash scripts/fl/user.sh'
put scripts/fl/py.py 'print(1)'
put scripts/fl/py_test.py "$(printf '%s\n' '# script-test-reads: scripts/fl/*.py' 'print(1)')"
printf '#!/usr/bin/env bash\necho bare\n' > "$tree/scripts/fl/bare"
put scripts/fl/bare-test.sh 'bash scripts/fl/bare'
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm floor
floorbase="$(scratch_git "$tree" rev-parse HEAD)"
floor() { run --bash "$work/floor-bash" --changed "$floorbase" "$@"; }
floor_listed() { floor --list > /dev/null; cut -f1 "$work/out" | paste -sd' ' -; }
echo 'echo edited' >> "$tree/scripts/fl/tool.sh"
check "the floor's scope: a bash tool's edit re-runs its own self-test, not one only naming it" \
  "scripts/fl/mention-test.sh scripts/fl/tool-test.sh|scripts/fl/tool-test.sh" \
  "$(listed --changed "$floorbase")|$(floor_listed)"
check "...and runs it, passing" "0 1" "$(floor) $(grep -c '^PASS .*fl/tool-test.sh' "$work/out")"
scratch_git "$tree" checkout -q -- scripts/fl/tool.sh
echo 'echo edited' >> "$tree/scripts/fl/flib.sh"
check "...a sourced library's edit re-runs the self-tests of the tools sourcing it" "scripts/fl/user-test.sh" "$(floor_listed)"
scratch_git "$tree" checkout -q -- scripts/fl/flib.sh
echo 'echo edited' >> "$tree/scripts/fl/bare"
check "...an extensionless bash tool is bash" "scripts/fl/bare-test.sh" "$(floor_listed)"
scratch_git "$tree" checkout -q -- scripts/fl/bare
echo 'print(2)' >> "$tree/scripts/fl/py.py"
check "...a Python change re-runs nothing under it, and passes" "0 1" "$(floor) $(grep -c 'no script tests selected' "$work/out")"
scratch_git "$tree" checkout -q -- scripts/fl/py.py
printf '%s\n' 'declare -A seen' 'seen[x]=1' >> "$tree/scripts/fl/tool.sh"
check "an added bash-4-only construct fails the floor run, though every test re-run under it passes" "1 1" \
  "$(floor) $(grep -c '^PASS .*fl/tool-test.sh' "$work/out")"
check "...naming the file, its line and the construct" "1" \
  "$(grep -c '^FLOOR .*scripts/fl/tool.sh:2: an associative array, declare -A (bash 4.0)' "$work/out")"
check "...while the run on today's bash passes" "0" "$(run --changed "$floorbase" scripts/fl)"
scratch_git "$tree" checkout -q -- scripts/fl/tool.sh
printf '%s\n' 'readarray -t x < f' >> "$tree/scripts/fl/mention-test.sh"
check "...in any bash file the change touches, a test re-run or not" "1" \
  "$(floor > /dev/null; grep -c '^FLOOR .*scripts/fl/mention-test.sh:2:' "$work/out")"
scratch_git "$tree" checkout -q -- scripts/fl/mention-test.sh
for construct in 'mapfile -t lines < f' 'echo "${name,,}"' 'wait -n' 'echo "${arr[-1]}"' 'exec {fd}> f' 'cmd |& tee f' \
    'echo x &>> f' 'printf "%(%s)T" -1' '[[ -v name ]]' 'echo "${name@Q}"' 'local -n ref=x' 'shopt -s globstar' \
    'echo "$EPOCHSECONDS"' 'coproc cat' 'declare -g x=1' 'declare -u name' 'local -l name=x' 'echo {1..9..2}'; do
  printf '%s\n' "$construct" >> "$tree/scripts/fl/tool.sh"
  check "...each bash-4-only construct: $construct" "1" "$(floor > /dev/null; grep -c '^FLOOR .*scripts/fl/tool.sh:2:' "$work/out")"
  scratch_git "$tree" checkout -q -- scripts/fl/tool.sh
done
printf '%s\n' '# mapfile and ${x,,} need bash 4' 'echo "${BASHPID:-$$}" # on 3.2: $$' 'declare -a list' 'echo "${x:-a}|${#y}"' \
  'local -r name=x' 'echo {1..9}' \
  >> "$tree/scripts/fl/tool.sh"
check "...never one only named in a comment, a guarded BASHPID, an indexed array or a 3.2 expansion" "0" "$(floor)"
scratch_git "$tree" checkout -q -- scripts/fl/tool.sh
printf '%s\n' '# bash-floor: a lint naming the constructs as data' 'echo "declare -A is 4.0"' > "$tree/scripts/fl/data.sh"
check "...nor a line of a file that says it names them as data (bash-floor:), parsed all the same" "0" "$(floor)"
rm "$tree/scripts/fl/data.sh"
printf '%s\n' 'mapfile = 1' >> "$tree/scripts/fl/py.py"
check "...nor a non-bash file's line" "0" "$(floor)"
scratch_git "$tree" checkout -q -- scripts/fl/py.py
printf '%s\n' 'echo FLOOR_CANNOT_PARSE' >> "$tree/scripts/fl/user.sh"
check "a changed bash file the floor cannot parse fails the run, saying so" "1 1" \
  "$(floor) $(grep -c '^FLOOR .*scripts/fl/user.sh: bash 3.2 cannot parse it' "$work/out")"
scratch_git "$tree" checkout -q -- scripts/fl/user.sh
printf '%s\n' 'readarray -t x < f' >> "$tree/scripts/fl/tool.sh"; scratch_git "$tree" commit -qam 'a construct committed'
check "...a line committed is held" "1" "$(floor > /dev/null; grep -c '^FLOOR .*scripts/fl/tool.sh:2:' "$work/out")"
printf '%s\n' 'echo tool' 'echo after' > "$tree/scripts/fl/tool.sh"
check "...and none the tree has taken out again" "0" "$(floor > /dev/null; grep -c '^FLOOR' "$work/out")"
scratch_git "$tree" checkout -q "$floorbase" -- scripts/fl/tool.sh; scratch_git "$tree" commit -qam 'the construct gone'
put scripts/fl/new.sh 'readarray -t x < f'
check "...an untracked bash file's lines are all added lines" "1" "$(floor > /dev/null; grep -c '^FLOOR .*scripts/fl/new.sh:1:' "$work/out")"
rm "$tree/scripts/fl/new.sh"
# --floor: BOTH RUNS IN ONE. The hook ran the floor run beside the native one, so a test in the floor's scope ran twice at
# once (pre-push-test.sh, minutes each, slowing both); now each test runs once: the floor's scope under the floor bash,
# the rest natively, in one pool -- and the floor's static half still fails the run.
put scripts/fl/which-test.sh "$(printf '%s\n' 'echo "floor $(command -v bash)" >> "$WHICH_BASH"')"
put scripts/fl/which.sh 'echo which'
put scripts/fl/names-which-test.sh 'echo "reads what scripts/fl/which.sh prints"; echo "native $(command -v bash)" >> "$WHICH_BASH"'
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm 'a tool whose test says which bash runs it'
whichbase="$(scratch_git "$tree" rev-parse HEAD)"
echo 'echo edited' >> "$tree/scripts/fl/which.sh"
export WHICH_BASH="$work/which-bash"; : > "$WHICH_BASH"
code="$(run --floor "$work/floor-bash" --changed "$whichbase")"
check "--floor: a changed tool's self-test runs once, under the floor bash; a test it does not scope runs natively" \
  "0 2 1 1 1" "$code $(grep -c . "$WHICH_BASH") $(grep -c '^floor .*floor-bin/bash$' "$WHICH_BASH") $(
    [ "$(sed -n 's/^floor //p' "$WHICH_BASH")" != "$(sed -n 's/^native //p' "$WHICH_BASH")" ] && echo 1) $(
    grep -c '^PASS .*fl/names-which-test.sh$' "$work/out")"
check "...the floor's tests marked in the table" "1" "$(grep -c '^PASS .*fl/which-test.sh (floor)$' "$work/out")"
echo 'readarray -t x < f' >> "$tree/scripts/fl/which.sh"
check "...and a construct the floor lacks still fails the run" "1 1" \
  "$(run --floor "$work/floor-bash" --changed "$whichbase") $(grep -c '^FLOOR .*scripts/fl/which.sh:3:' "$work/out")"
check "...--floor without --changed, or with --bash, is a usage error" "64 64" \
  "$(run --floor "$work/floor-bash") $(run --floor "$work/floor-bash" --bash "$work/floor-bash" --changed "$whichbase")"
scratch_git "$tree" checkout -q -- scripts/fl
put scripts/fl/pylib.sh 'echo pylib'
put scripts/fl/pytool "$(printf '%s\n' '#!/usr/bin/env bash' '. "$(dirname "$0")/pylib.sh"')"
put scripts/fl/pytool_test.py "$(printf '%s\n' 'import shutil, subprocess' 'subprocess.run(["bash", "-c", "echo py $(command -v bash) >> \"$WHICH_BASH\""], check=True)')"
scratch_git "$tree" add -A && scratch_git "$tree" commit -qm 'a bash tool with a Python self-test'
pybase="$(scratch_git "$tree" rev-parse HEAD)"
echo 'echo edited' >> "$tree/scripts/fl/pylib.sh"; : > "$WHICH_BASH"
code="$(run --floor "$work/floor-bash" --changed "$pybase")"
check "...a Python self-test in the floor's scope runs under python3, the floor bash first on its PATH" "0 1 1" \
  "$code $(grep -c '^PASS .*fl/pytool_test.py (floor)$' "$work/out") $(grep -c '^py .*floor-bin/bash$' "$WHICH_BASH")"
scratch_git "$tree" checkout -q -- scripts/fl
rm -rf "$tree/scripts/fl"; scratch_git "$tree" add -A && scratch_git "$tree" commit -qm 'floor done'

check "an unknown option is a usage error" "64" "$(run --bogus)"
check "a flag given last without its value is a usage error, 64 (not 1)" "64" "$(run --jobs)"
check "...named, before the usage" "1" "$(grep -c 'run-script-tests: --jobs needs a value' "$work/out")"
check "...the usage read from the header" "1" "$(grep -c '^Options: --jobs N' "$work/out")"
# --linux: the selection made here, the tests run in a container (a stub `docker` records what it was asked)
printf '\033[36m▸\033[0m run-script-tests --linux\n'
mkdir -p "$work/docker-bin" "$work/home"
cat > "$work/docker-bin/docker" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$DOCKER_CALLS"
case "$1" in
  info) [ -z "${DOCKER_DOWN:-}" ] || { echo "Cannot connect to the Docker daemon" >&2; exit 1; } ;;
  image) [ -z "${NO_IMAGE:-}" ] || exit 1 ;;
  build) from=$(head -1); cat > /dev/null; echo "$from" >> "$DOCKER_CALLS"
    case "$from" in *mirror.gcr.io*) [ -z "${MIRROR_LOST:-}" ] || { echo "ERROR: $MIRROR_LOST" >&2; exit 1; } ;; esac ;;
  run) cat > "$DOCKER_STDIN"; echo "PASS       0s  in the container" ;;
esac
STUB
chmod +x "$work/docker-bin/docker"
linux() { : > "$work/docker-calls"
  (cd "$work" && HOME="$work/home" PATH="$work/docker-bin:$PATH" DOCKER_CALLS="$work/docker-calls" DOCKER_STDIN="$work/docker-stdin" \
    bash "$runner" --root "${LINUX_ROOT:-$tree}" --linux "$@") > "$work/out" 2>&1; echo $?; }
check "--linux runs the selected tests in a container" "0" "$(linux scripts/ci/b_test.py)"
# Rancher's VM holds a host descriptor for every file and directory a container ever saw through the shared home, and
# keeps it after the directory is gone: a worktree mounted itself left ~25k behind, and on 2026-11-20 two hundred
# thousand of them filled the Mac's file table (ENFILE). A copy in one of a few fixed slots is mounted in its place.
slots="$work/home/.cache/movies-linux-tree"
check "...a copy of the checkout in the first free slot, mounted read-only at the checkout's path, as this user, not root" "1 0 1" \
  "$(grep -c -- "^run .*-v $slots/slot-0:$tree:ro" "$work/docker-calls") $(grep -c -- "-v $tree:" "$work/docker-calls") \
$(grep -c -- "--user $(id -u):$(id -g)" "$work/docker-calls")"
check "...the copy its files and the git dir's mount point" "echo tool a|dir" \
  "$(head -1 "$slots/slot-0/scripts/a.sh")|$([ -d "$slots/slot-0/.git" ] && echo dir)"
check "...naming exactly the tests selected here" "1" "$(grep -c 'run-script-tests --jobs 8 --timeout 0 --stall 300 scripts/ci/b_test.py$' "$work/docker-calls")"
check "...from an ubuntu image pulled from mirror.gcr.io" "1" "$(grep -c 'kinowo-script-tests:' "$work/docker-calls" | grep -c .)"
# its git metadata a snapshot, unpacked into a tmpfs at the git dir's own path, only the objects mounted live: a host git
# rewriting the shared .git/config (lock and rename) made it read as missing in there for most of the next second
git_dir="$(cd "$tree/.git" && pwd -P)"
check "...its git dir a tmpfs, the objects in it mounted read-only, the git dir itself not mounted" "1 1 0" \
  "$(grep -c -- "^run .*--mount type=tmpfs,destination=$git_dir,tmpfs-mode=1777" "$work/docker-calls") \
$(grep -c -- "^run .*-v $git_dir/objects:$git_dir/objects:ro" "$work/docker-calls") $(grep -c -- "-v $git_dir:" "$work/docker-calls")"
check "...unpacked there from its stdin before the tests start" "1" \
  "$(grep -c -- "^run .* bash -c .*tar -xf - -C \"\$0\".* $git_dir bash scripts/run-script-tests " "$work/docker-calls")"
check "...the snapshot its config, HEAD and refs, not its objects" "config HEAD refs/heads/main 0" \
  "$(tar -tf "$work/docker-stdin" | grep -x -e config -e HEAD -e refs/heads/main | paste -sd' ' -) $(tar -tf "$work/docker-stdin" | grep -c '^objects')"
echo scratch > "$tree/main-only.txt"; linux scripts/ci/b_test.py > /dev/null
scratch_git "$tree" worktree add -q "$work/wt" -b wt
LINUX_ROOT="$work/wt" linux scripts/ci/b_test.py > /dev/null
check "...a worktree run after it: the same slot, in step with that worktree, its .git file and not the other's files" \
  "1 file absent" "$(grep -c -- "^run .*-v $slots/slot-0:$work/wt:ro" "$work/docker-calls") \
$([ -f "$slots/slot-0/.git" ] && echo file) $([ -e "$slots/slot-0/main-only.txt" ] && echo present || echo absent)"
rm "$tree/main-only.txt"
# a slot another run holds (its flock) is passed over; every slot held: a clear failure once the wait is up
hold_slot() { rm -f "$work/held"; mkdir -p "$slots"
  python3 -c 'import fcntl, sys, time; f = open(sys.argv[1], "w"); fcntl.flock(f, fcntl.LOCK_EX); open(sys.argv[2], "w").close(); time.sleep(60)' \
    "$slots/slot-$1.lock" "$work/held" & holder=$!
  wait_until "slot $1 held" '[ -e "$work/held" ]'; }
hold_slot 0
LINUX_ROOT="$work/wt" linux scripts/ci/b_test.py > /dev/null
check "...a held slot passed over for the next" "1" "$(grep -c -- "^run .*-v $slots/slot-1:$work/wt:ro" "$work/docker-calls")"
check "...every slot held: exit 2 once the wait is up, no container" "2 1 0" \
  "$(RUN_SCRIPT_TESTS_LINUX_SLOTS=1 RUN_SCRIPT_TESTS_LINUX_WAIT=0 LINUX_ROOT="$work/wt" linux scripts/ci/b_test.py) \
$(grep -c 'every Linux tree slot is busy' "$work/out") $(grep -c '^run ' "$work/docker-calls")"
kill "$holder"; wait "$holder" 2>/dev/null
check "...from a worktree, the clone's metadata and the worktree's own" "config worktrees/wt/HEAD worktrees/wt/commondir worktrees/wt/gitdir" \
  "$(tar -tf "$work/docker-stdin" | grep -x -e config -e 'worktrees/wt/[a-zA-Z]*' | grep -v -e index -e ORIG_HEAD | paste -sd' ' -)"
scratch_git "$tree" worktree remove --force "$work/wt"
# a checkout borrowing objects (a git mirror, by objects/info/alternates) reads them in the container too
scratch_repo "$work/mirror.git" --bare
mirror_objects="$(cd "$work/mirror.git/objects" && pwd -P)"
echo "$mirror_objects" > "$tree/.git/objects/info/alternates"
linux scripts/ci/b_test.py > /dev/null
check "...with the object directories it borrows from mounted read-only at their own paths" "1" \
  "$(grep -c -- "^run .*-v $mirror_objects:$mirror_objects:ro" "$work/docker-calls")"
rm "$tree/.git/objects/info/alternates"
NO_IMAGE=1 linux scripts/ci > /dev/null
check "...built once when it is missing" "1" "$(grep -c '^build ' "$work/docker-calls")"
NO_IMAGE=1 MIRROR_LOST="failed to copy: could not fetch content descriptor sha256:edaf (application/vnd.oci.image.layer.v1.tar+gzip) from remote: not found" \
  linux scripts/ci > /dev/null
# The recipe's own base (tag and digest, which infra/bin/image_pins.py refreshes), from Docker Hub instead.
hub_base="$(sed -n "s#^LinuxImageRecipe='FROM mirror.gcr.io/#FROM docker.io/#p" "$REPO_ROOT/scripts/run-script-tests")"
check "...a blob the mirror lost: built again, from Docker Hub" "2 ${hub_base:-a recipe FROM mirror.gcr.io}" \
  "$(grep -c '^build ' "$work/docker-calls") $(grep '^FROM' "$work/docker-calls" | tail -1)"
check "...any other build failure fails, built once" "2 1" \
  "$(NO_IMAGE=1 MIRROR_LOST="no space left on device" linux scripts/ci) $(grep -c '^build ' "$work/docker-calls")"
scratch_git "$tree" update-ref refs/remotes/origin/main "$base"
want="$(run --list --changed origin/main > /dev/null; grep -c . "$work/out")"
check "with no names, the tests --changed origin/main selects" "0 $want" \
  "$(linux) $(sed -n 's/^run-script-tests: \([0-9]*\) test(s) on Linux.*/\1/p' "$work/out")"
check "Docker not running: a clear failure, exit 2" "2" "$(DOCKER_DOWN=1 linux scripts/ci)"
check "...naming what to start" "1" "$(grep -c -- '--linux needs a running Docker' "$work/out")"

# THE TREE IS LEFT AS FOUND: a test leaving a file in the checkout fails, named, beside a clean one running at the same
# time; one leaving an ignored directory (an `npm ci`'s node_modules) too; a path no test makes again alone is said, not
# failed; the leftovers are where the run left them afterwards
printf 'node_modules/\n' > "$tree/.gitignore"
put scripts/polluter-test.sh 'sleep 1; echo x > scripts/left-behind.txt; mkdir -p scripts/tools/node_modules/pkg; echo y > scripts/tools/node_modules/pkg/i.js'
put scripts/clean-test.sh 'd=$(mktemp -d); echo x > "$d/f"; rm -rf "$d"; sleep 2'
check "a test leaving files in the checkout fails the run" "1" "$(run --jobs 4 polluter-test clean-test)"
check "...its row POLLUTE, the clean one beside it PASS" "1 1" \
  "$(grep -c '^POLLUTE .*scripts/polluter-test.sh' "$work/out") $(grep -c '^PASS .*scripts/clean-test.sh' "$work/out")"
check "...naming each path, the ignored directory too" "!! scripts/tools/|?? scripts/left-behind.txt" \
  "$(sed -n '/── POLLUTE scripts\/polluter-test.sh/,/^$/p' "$work/out" | grep -E '^    (\?\?|!!) ' | sed 's/^ *//' | paste -sd'|' -)"
check "...and the leftovers are where the run left them" "x y" \
  "$(cat "$tree/scripts/left-behind.txt") $(cat "$tree/scripts/tools/node_modules/pkg/i.js")"
rm -rf "$tree/scripts/left-behind.txt" "$tree/scripts/tools"
check "a clean test passes, the check saying nothing" "0 0" "$(run clean-test) $(grep -c 'left in the checkout' "$work/out")"
# macOS metadata is no test's: agentctl's maintain pass drops Spotlight's `.metadata_never_index` in every worktree
# (and keeps it out of `git status` through info/exclude, which --ignored still lists), Finder a `.DS_Store` anywhere --
# made beside a run, they once failed whichever test was running (pre-push-test.sh POLLUTE)
put scripts/finder-test.sh 'touch .metadata_never_index scripts/.DS_Store; sleep 1'
check "macOS metadata made in the checkout during a test is no leftover" "0 0" \
  "$(run finder-test) $(grep -c 'left in the checkout' "$work/out")"
rm -f "$tree/.metadata_never_index" "$tree/scripts/.DS_Store" "$tree/scripts/finder-test.sh"
# made once only (as an edit beside the run would be): not this test's, said and not failed
put scripts/once-test.sh "[ -e '$work/once-made' ] || { touch '$work/once-made'; echo z > scripts/edited-beside.txt; }"
check "a path no test makes again when run alone is said, and does not fail the run" "0 1" \
  "$(run once-test) $(grep -c '^    ?? scripts/edited-beside.txt   (running then: scripts/once-test.sh )' "$work/out")"
# made beside the run all along (the landing check's sbt writing target/ beside its scripttests, run 45008924490): the
# test run alone "makes it again" only because the writer beside it never stopped -- the control window, no test
# running, sees it made again and charges no test. The writer, started by the test's first run, makes target/ again
# each time it is moved aside, until told to stop.
rm -f "$tree/scripts/edited-beside.txt"
printf 'target/\n' > "$tree/.gitignore"
cat > "$work/beside.sh" <<BESIDE
. "$REPO_ROOT/scripts/shell-spec.sh"
mkdir -p "$tree/target"; date > "$tree/target/out"
while [ ! -e "$work/stop" ]; do
  wait_until "target/ moved aside" "[ ! -e '$tree/target' ] || [ -e '$work/stop' ]" || break
  [ -e "$work/stop" ] || { mkdir -p "$tree/target"; date > "$tree/target/out"; }
done
BESIDE
put scripts/quiet-test.sh ". '$REPO_ROOT/scripts/shell-spec.sh'
[ -e '$work/beside.started' ] || { touch '$work/beside.started'; bash '$work/beside.sh' > /dev/null 2>&1 < /dev/null & }
wait_until 'the writer beside the run made target/' \"[ -e '$tree/target/out' ]\""
check "a path a writer beside the run keeps making is no test's, though a test ran alone while it was made" "0 0 1" \
  "$(run quiet-test) $(grep -c POLLUTE "$work/out") $(grep -c '^    !! target/   (running then: scripts/quiet-test.sh )' "$work/out")"
touch "$work/stop"
wait_until "the writer beside the run stopped" "! pgrep -f '$work/beside.sh' > /dev/null"
rm -rf "$tree/target" "$tree/scripts/quiet-test.sh" "$work/beside.started" "$work/stop"
printf 'node_modules/\n' > "$tree/.gitignore"
check "RUN_SCRIPT_TESTS_TREE_CHECK=0: no check" "0 0" \
  "$(rm -f "$tree/scripts/edited-beside.txt" "$work/once-made"; RUN_SCRIPT_TESTS_TREE_CHECK=0 run polluter-test) $(grep -c POLLUTE "$work/out")"
rm -rf "$tree/scripts/left-behind.txt" "$tree/scripts/tools" "$tree/scripts/edited-beside.txt" "$tree/.gitignore" \
  "$tree/scripts/polluter-test.sh" "$tree/scripts/clean-test.sh" "$tree/scripts/once-test.sh"

# STRESS: the named tests N times at once beside CPU burners; none named, the concurrency-heavy ones (marked)
put scripts/racy-test.sh "$(printf '%s\n' '# script-test-stress: its copies share one lock' "mkdir '$work/racy.lock' 2> /dev/null || { echo collided; exit 1; }" "sleep 3; rmdir '$work/racy.lock'")"
put scripts/steady-test.sh "$(printf '%s\n' '# script-test-stress: never races' 'exit 0')"
check "a racy test passes alone" "0" "$(run racy-test)"
check "--stress 4: run 4 times at once, the race caught" "1 4" "$(run --stress 4 --burn 2 racy-test) $(grep -c '^[A-Z]* .*scripts/racy-test.sh (run [1-4]/4)$' "$work/out")"
check "...its verdict: how many of its runs failed, beside the CPU burners asked for" "1 1" \
  "$(grep -c '^STRESS  scripts/racy-test.sh: 3 of 4 run(s) failed$' "$work/out") $(grep -c 'beside 2 CPU burner(s)' "$work/out")"
check "...the burners gone with the run" "0" "$(ps -axo command | grep -c '[r]un-script-tests-burner')"
check "--stress with no names: the concurrency-heavy (marked) tests only" "scripts/racy-test.sh scripts/steady-test.sh" "$(listed --stress 2)"
check "a steady test passes under stress" "0 1" "$(run --stress 3 --burn 1 steady-test) $(grep -c '^STRESS  scripts/steady-test.sh: 0 of 3 run(s) failed$' "$work/out")"
check "a --changed run selecting a concurrency-heavy test names the stress command" "0 1" \
  "$(run --changed "$base" steady-test) $(grep -c '^run-script-tests: concurrency-heavy, selected: scripts/steady-test.sh -- `scripts/run-script-tests --stress 4 scripts/steady-test.sh`' "$work/out")"
check "--stress with --linux, or no count, is a usage error" "64 64" "$(run --stress 2 --linux racy-test) $(run --stress x racy-test)"
rm -rf "$tree/scripts/racy-test.sh" "$tree/scripts/steady-test.sh" "$work/racy.lock"

# THE GOVERNOR: the run waits for its ticket (a starved laptop), charged its parallelism, then starts by itself; its
# tests see no ticket of its own (descriptor 7 closed, KINOWO_JVM_SLOT_HELD unset)
spec_fake_governor "$work/agentctl"
export GOV_CALLS="$work/gov-calls"
put scripts/sees-test.sh 'echo "fd7 $({ : >&7; } 2>/dev/null && echo open || echo closed) held=${KINOWO_GOV_CPU_HELD:-} slot=${KINOWO_JVM_SLOT_HELD:-} width=${AGENTCTL_GOV_WIDTH:-}" > "$SEEN"'
: > "$GOV_CALLS"; rm -f "$work/seen"
(cd "$work" && SEEN="$work/seen" GOV_HOLD="$work/admit" spec_governed "$work/agentctl" bash "$runner" --root "$tree" --jobs 3 scripts/sees-test.sh scripts/a-test.sh) \
  > "$work/out" 2>&1 &
governed_run=$!
wait_until "the run asked the governor" '[ -e "$work/admit.waiting" ]'
check "governed: not admitted, no test has started" "absent" "$([ -e "$work/seen" ] && echo ran || echo absent)"
touch "$work/admit"; wait "$governed_run"; status=$?
check "...admitted, the run starts by itself and passes" "0 fd7 closed held= slot= width=1" "$status $(cat "$work/seen")"
check "...one ticket, charged min(--jobs, tests) cores as script-tests" "1" \
  "$(grep -c -- '^gov admit .*--class agent --mem 2 --label run-script-tests .*--job script-tests --cores 2$' "$GOV_CALLS")"
: > "$GOV_CALLS"
(cd "$work" && SEEN="$work/seen" spec_governed "$work/agentctl" "$REPO_ROOT/scripts/gov-run" --job outer -- bash "$runner" --root "$tree" scripts/sees-test.sh) > "$work/out" 2>&1
check "...run under a ticket already (a gated parent): it rides it, no second ticket" "1 fd7 closed held= slot= width=1" \
  "$(wc -l < "$GOV_CALLS" | tr -d ' ') $(cat "$work/seen")"
: > "$GOV_CALLS"
(cd "$work" && SEEN="$work/seen" KINOWO_GOV_CPU_HELD=1 KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin \
  bash "$runner" --root "$tree" scripts/sees-test.sh
  SEEN="$work/seen" KINOWO_GOVERNOR=0 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin bash "$runner" --root "$tree" scripts/sees-test.sh
  SEEN="$work/seen2" spec_governed "$work/agentctl" env KINOWO_JVM_GATE_OS=Linux bash "$runner" --root "$tree" scripts/sees-test.sh) > "$work/out" 2>&1
check "...ungated in a check's hook, with KINOWO_GOVERNOR=0 and on Linux: no ticket asked, the tests run (no width off the governor)" "0 fd7 closed held= slot= width=|fd7 closed held= slot= width=" \
  "$(wc -l < "$GOV_CALLS" | tr -d ' ') $(cat "$work/seen")|$(cat "$work/seen2")"
: > "$GOV_CALLS"
(cd "$work" && HOME="$work/home" PATH="$work/docker-bin:$PATH" DOCKER_CALLS="$work/docker-calls" DOCKER_STDIN="$work/docker-stdin" \
  spec_governed "$work/agentctl" bash "$runner" --root "$tree" --linux scripts/ci/b_test.py) > "$work/out" 2>&1
check "--linux: its container (and image build) under one ticket of the run's" "1 1" \
  "$(grep -c -- '--label run-script-tests .*--job script-tests --cores 1$' "$GOV_CALLS") $(grep -c '^run ' "$work/docker-calls")"
# THE GOVERNED FAN-OUT (2026-11-25: load 103, `gov status` 0 admitted): under a ticket charged its cores already
# (AGENTCTL_GOV_WIDTH: a check's hook) the run takes none and runs at most that wide, whatever --jobs asks
: > "$GOV_CALLS"
(cd "$work" && SEEN="$work/seen" AGENTCTL_GOV_WIDTH=2 KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$work/agentctl" \
  KINOWO_JVM_GATE_OS=Darwin bash "$runner" --root "$tree" --jobs 8 scripts/sees-test.sh scripts/a-test.sh scripts/ci/b_test.py) \
  > "$work/out" 2>&1
check "...under a ticket of width 2: no ticket asked, --jobs 8 runs 2 at a time, each test one core of it" \
  "0 1 fd7 closed held= slot= width=1" \
  "$(grep -c '^gov admit' "$GOV_CALLS") $(grep -c '3 script test(s), 2 at a time' "$work/out") $(cat "$work/seen")"
# a test fanning out itself declares its width (`# script-test-width: N`): the run's own ticket is charged it and the
# test runs that wide; under a parent's ticket it still gets one core
# -- while the sum fits the machine's cores, so the machine is pinned (getconf on PATH): 2026-11-25, Tool self-tests
# #555, GitHub's 3-core macOS runner read 2 + 2 > 3 and dropped the extra, the case passing only where 4+ cores ran it
put scripts/wide-test.sh "$(printf '%s\n' '# script-test-width: 3' 'echo "width=${AGENTCTL_GOV_WIDTH:-}" > "$SEEN"')"
mkdir -p "$work/cores-bin"
printf '#!/bin/sh\n[ "$1" = _NPROCESSORS_ONLN ] && { echo "$CORES"; exit 0; }\nexec /usr/bin/getconf "$@"\n' > "$work/cores-bin/getconf"
chmod +x "$work/cores-bin/getconf"
: > "$GOV_CALLS"
(cd "$work" && SEEN="$work/seen" CORES=8 PATH="$work/cores-bin:$PATH" spec_governed "$work/agentctl" bash "$runner" --root "$tree" \
  scripts/wide-test.sh scripts/a-test.sh) > "$work/out" 2>&1
check "...a test declaring width 3: the ticket charged its extra 2 beside min(--jobs, tests), the test 3 wide" "1 width=3" \
  "$(grep -c -- '--job script-tests --cores 4$' "$GOV_CALLS") $(cat "$work/seen")"
: > "$GOV_CALLS"
(cd "$work" && SEEN="$work/seen" CORES=3 PATH="$work/cores-bin:$PATH" spec_governed "$work/agentctl" bash "$runner" --root "$tree" \
  scripts/wide-test.sh scripts/a-test.sh) > "$work/out" 2>&1
check "...on 3 cores the extra 2 does not fit: the ticket min(--jobs, tests) wide, the wide test one core of it" "1 width=1" \
  "$(grep -c -- '--job script-tests --cores 2$' "$GOV_CALLS") $(cat "$work/seen")"
(cd "$work" && SEEN="$work/seen" spec_governed "$work/agentctl" env AGENTCTL_GOV_WIDTH=2 bash "$runner" --root "$tree" scripts/wide-test.sh) \
  > "$work/out" 2>&1
check "...under a parent's ticket the wide test gets one core" "width=1" "$(cat "$work/seen")"

# The watchdog the pre-push hook runs reaches its self-test only through the hook (2026-11-26: 14eb62591d changed
# stall-watch.sh, --changed never selected pre-push-tools-test.sh, and its stalled-check case failed on main): this
# checkout's own tools test declares what it reads, so an edit to the watchdog selects it.
printf 'scripts/stall-watch.sh\nscripts/stall_watch.py\n' > "$work/stall-paths"
check "--paths: an edit to the stall watchdog selects the hook's tools test, which runs it through the hook" "2" \
  "$("$BASH" "$REPO_ROOT/scripts/run-script-tests" --list --paths "$work/stall-paths" 2>/dev/null | cut -f1 | grep -c 'scripts/hooks/pre-push-tools-test.sh\|scripts/stall_watch_test.py')"

spec_summary
