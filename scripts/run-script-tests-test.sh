#!/usr/bin/env bash
# run-script-tests over a throwaway tree of fake self-tests: what it discovers (and what a
# `# script-test: <tag>` header keeps out of the default run), its exit status and table, a path
# filter, --changed against a base commit, the per-test timeout, and the bound on how many tests run at once.
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
put scripts/c-test.sh "$(printf '%s\n' '. scripts/ci/lib.sh' 'for n in $(seq 1 100); do echo "line $n"; done' 'echo boom; exit 1')"
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

# The timeout: a hung test is killed at its bound and FAILS as a timeout. The test never ends on its own (it records
# its pid and blocks until killed), so the runner gets past it only by its bound: that, reported, and no test left
# running prove it, never elapsed wall time (shell-spec.sh's HUNG STUBS).
put scripts/slow-test.sh "echo \$\$ >> '$work/hung'; exec sleep 86400"
: > "$work/hung"
code="$(run --timeout 1 slow-test)"
check "a hung test is cut off at its timeout and fails" "1" "$code"
check "...reported as a timeout, and not left running" "1 0" "$(grep -c '^TIMEOUT .*slow-test' "$work/out") $(spec_still_running "$work/hung")"
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
check "...the checkout mounted read-only at its own path, as this user, not root" "1 1" \
  "$(grep -c -- "^run .*-v $tree:$tree:ro" "$work/docker-calls") $(grep -c -- "--user $(id -u):$(id -g)" "$work/docker-calls")"
check "...naming exactly the tests selected here" "1" "$(grep -c 'run-script-tests --jobs 8 --timeout 600 scripts/ci/b_test.py$' "$work/docker-calls")"
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
scratch_git "$tree" worktree add -q "$work/wt" -b wt
LINUX_ROOT="$work/wt" linux scripts/ci/b_test.py > /dev/null
check "...from a worktree, the clone's metadata and the worktree's own" "config worktrees/wt/HEAD worktrees/wt/commondir worktrees/wt/gitdir" \
  "$(tar -tf "$work/docker-stdin" | grep -x -e config -e 'worktrees/wt/[a-zA-Z]*' | grep -v -e index -e ORIG_HEAD | paste -sd' ' -)"
scratch_git "$tree" worktree remove --force "$work/wt"
# a checkout borrowing objects (ci-runner's git mirror, by objects/info/alternates) reads them in the container too
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
check "...a blob the mirror lost: built again, from Docker Hub" "2 FROM docker.io/library/ubuntu:26.04" \
  "$(grep -c '^build ' "$work/docker-calls") $(grep '^FROM' "$work/docker-calls" | tail -1)"
check "...any other build failure fails, built once" "2 1" \
  "$(NO_IMAGE=1 MIRROR_LOST="no space left on device" linux scripts/ci) $(grep -c '^build ' "$work/docker-calls")"
scratch_git "$tree" update-ref refs/remotes/origin/main "$base"
want="$(run --list --changed origin/main > /dev/null; grep -c . "$work/out")"
check "with no names, the tests --changed origin/main selects" "0 $want" \
  "$(linux) $(sed -n 's/^run-script-tests: \([0-9]*\) test(s) on Linux.*/\1/p' "$work/out")"
check "Docker not running: a clear failure, exit 2" "2" "$(DOCKER_DOWN=1 linux scripts/ci)"
check "...naming what to start" "1" "$(grep -c -- '--linux needs a running Docker' "$work/out")"

spec_summary
