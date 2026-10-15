#!/usr/bin/env bash
# run-script-tests over a throwaway tree of fake self-tests: what it discovers (and what a
# `# script-test: <tag>` header keeps out of the default run), its exit status and table, a path
# filter, --changed against a base commit, the per-test timeout (through both the timeout command
# and the perl fallback), and the bound on how many tests run at once.
# Run: bash scripts/run-script-tests-test.sh
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
put scripts/c-test.sh "$(printf '%s\n' '. scripts/ci/lib.sh' 'echo boom; exit 1')"
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

run() { (cd "$work" && "${SCRIPT_TESTS_BASH:-bash}" "$runner" --root "$tree" "$@") > "$work/out" 2>&1; echo $?; }
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

# The timeout: a hung test is killed at its bound and FAILS as a timeout, through `timeout` and the perl fallback.
put scripts/slow-test.sh 'sleep 30'
for tool in auto perl; do
  start=$SECONDS
  code="$(SCRIPT_TESTS_TIMEOUT_TOOL=$tool run --timeout 1 slow-test)"
  check "a hung test is cut off at its timeout and fails ($tool)" "1 yes" "$code $([ $((SECONDS - start)) -lt 10 ] && echo yes || echo no)"
  check "...reported as a timeout ($tool)" "1" "$(grep -c '^TIMEOUT .*slow-test' "$work/out")"
done
rm "$tree/scripts/slow-test.sh"

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

check "an unknown option is a usage error" "64" "$(run --bogus)"
spec_summary
