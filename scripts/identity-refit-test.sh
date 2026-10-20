#!/usr/bin/env bash
# scripts/identity-refit: the refit's steps in order — the spec, the measure on the classpath sbt exported, and the
# checks only when the weights changed — locally through sbt-warm and it-local.sh, in CI through plain sbt and the
# workflow's Mongo; a dirty tree and the root checkout refused. sbt, sbt-warm, it-local.sh and java are stubs.
# Run: bash scripts/identity-refit-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-refit\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
unset GITHUB_STEP_SUMMARY MONGODB_URI TMDB_API_KEY

# a repository holding the script, its stubbed neighbours and the files it reads, and a worktree of it to run in
repo="$work/repo"
mkdir -p "$repo/scripts" "$repo/identity/src/main/resources" "$repo/test/resources/fixtures/08-06-2026" "$work/bin"
cp "$REPO_ROOT/scripts/identity-refit" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/forge-gh" "$repo/scripts/"
echo '{"version":"v1"}' > "$repo/identity/src/main/resources/identity-weights.json"
echo 'film A' > "$repo/test/resources/fixtures/08-06-2026/expected-schedules.txt"
printf 'target/\n' > "$repo/.gitignore"

# sbt-warm and sbt log each call; an `export` prints a classpath; SPEC_FAILS fails the refit's spec; the snapshot
# spec writes a moved film
cat > "$repo/scripts/sbt-warm" <<'STUB'
#!/usr/bin/env bash
echo "$(basename "$0") $*" >> "$STUB/calls"
for a in "$@"; do
  case "$a" in
    *IdentityRefitSpec*) [ -z "${SPEC_FAILS:-}" ] || { echo "[error] IdentityRefitSpec failed"; exit 1; }; echo "[info] Tests: succeeded 16" ;;
    export*) printf "\033[0m/cp/a.jar:/cp/test-classes\033[0m\n" ;;
    *FilmScheduleEndToEndSpec*) echo "film B" > test/resources/fixtures/08-06-2026/expected-schedules.txt ;;
  esac
done
STUB
cp "$repo/scripts/sbt-warm" "$work/bin/sbt"
cat > "$repo/scripts/it-local.sh" <<'STUB'
#!/usr/bin/env bash
echo "it-local.sh $*" >> "$STUB/calls"
STUB
# java: records its arguments, writes the report, and with KEEP changes the weights
cat > "$work/bin/java" <<'STUB'
#!/usr/bin/env bash
echo "java $*" >> "$STUB/calls"
while [ $# -gt 0 ]; do [ "$1" = "--report" ] && { mkdir -p "$(dirname "$2")"; echo "## Identity rule refit" > "$2"; }; shift; done
[ -z "${KEEP:-}" ] || echo '{"version":"v1-refit-1"}' > identity/src/main/resources/identity-weights.json
echo "kept 0: "
STUB
# the processor as a Linux runner names it: lscpu answers, sysctl knows no machdep key (and fails)
printf '#!/usr/bin/env bash\necho "Model name:            Test CPU 9000"\n' > "$work/bin/lscpu"
printf '#!/usr/bin/env bash\nexit 255\n' > "$work/bin/sysctl"
chmod +x "$repo/scripts/"* "$work/bin/"*
scratch_git "$repo" init -q -b main && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm init
tree="$work/tree"
scratch_git "$repo" worktree add -q -b refit "$tree"

# refit <dir> [args...] -> its exit status; its output in $work/out, the calls in $work/calls
refit() {
    local dir=$1; shift
    rm -f "$work/calls"
    (cd "$dir" && STUB="$work" PATH="$work/bin:$PATH" scripts/identity-refit "$@") > "$work/out" 2>&1
    echo $?
}
reset_tree() { scratch_git "$tree" checkout -q -- . && scratch_git "$tree" clean -qfdx; }
calls() { cat "$work/calls" 2>/dev/null; }

check "--help prints the usage" "0" "$(refit "$tree" --help)"
check "...naming every flag" "1" "$(grep -c -- '--skip-checks.*--relearn.*--threads.*--report' "$work/out")"
check "an unknown flag is refused" "2" "$(refit "$tree" --bogus)"
check "a flag given last without its value is a usage error, exit 2" "2" "$(refit "$tree" --report)"
check "...named, with the pointer to --help" "2" "$(grep -c -e '--report needs a value' -e 'see identity-refit --help' "$work/out")"

check "the root checkout is refused" "1" "$(CI='' refit "$repo")"
check "...before any sbt" "" "$(calls)"

echo stray > "$tree/stray.txt"
check "a dirty tree is refused" "1" "$(CI='' refit "$tree")"
check "...saying so" "1" "$(grep -c 'the tree is dirty' "$work/out")"
reset_tree

check "nothing kept: done after the measure" "0" "$(CI='' TMDB_API_KEY='' refit "$tree" --threads 3)"
check "...the spec and the classpath in ONE sbt-warm call" "1" "$(calls | grep -c '^sbt-warm worker/testOnly scripts.IdentityRefitSpec export worker/Test/fullClasspath$')"
check "...the measure on that classpath, applied, on the threads asked" "1" \
    "$(calls | grep -c -- '^java -Xmx12g .* -cp /cp/a.jar:/cp/test-classes scripts.IdentityRefit --apply --report target/identity-refit/report.md --threads 3$')"
check "...naming the processor, though sysctl fails (a Linux runner)" "1" "$(grep -c 'processors (Test CPU 9000)' "$work/out")"
check "...no check run" "2" "$(calls | wc -l | tr -d ' ')"
check "...without a TMDB key, proposals asking TMDB said unmeasured" "1" "$(grep -c 'no TMDB_API_KEY.*unmeasured' "$work/out")"
check "...the measure's log beside the report" "1" "$(grep -c 'kept 0' "$tree/target/identity-refit/refit.log")"

check "a shard: round 1's share measured, nothing else" "0" "$(CI='' TMDB_API_KEY=k refit "$tree" --shard 1/4 --shard-out shards/s1.json --threads 3)"
check "...the classpath alone, no spec (the merging run tests the refit)" "1 0" \
    "$(calls | grep -c '^sbt-warm export worker/Test/fullClasspath$') $(calls | grep -c IdentityRefitSpec)"
check "...the shard measured into its file: no report, no weights written" "1" \
    "$(calls | grep -c -- '^java -Xmx12g .* -cp /cp/a.jar:/cp/test-classes scripts.IdentityRefit --shard 1/4 --shard-out shards/s1.json --threads 3$')"
check "...no check run" "2" "$(calls | wc -l | tr -d ' ')"
check "a shard needs a file to write" "2" "$(refit "$tree" --shard 1/4)"
check "merged: round 1 from the shards' files, the rest as ever" "0" "$(CI='' TMDB_API_KEY=k refit "$tree" --threads 3 --merge shards)"
check "...the measure applied, its round 1 merged" "1" \
    "$(calls | grep -c -- '^java .* scripts.IdentityRefit --apply --report target/identity-refit/report.md --threads 3 --merge shards$')"
check "...the spec still run first" "1" "$(calls | grep -c '^sbt-warm worker/testOnly scripts.IdentityRefitSpec export worker/Test/fullClasspath$')"

check "a red refit spec stops it before the measure" "1" "$(CI='' SPEC_FAILS=1 refit "$tree")"
check "...no measure run" "0" "$(calls | grep -c '^java')"

check "kept, locally: the checks run" "0" "$(CI='' KEEP=1 TMDB_API_KEY=k refit "$tree" --relearn year.delta --report target/r.md)"
check "...relearn passed through" "1" "$(calls | grep -c -- '--report target/r.md --threads [0-9]* --relearn year.delta$')"
check "...in order: identity specs, hard clusters on a mongod of its own (the warm server stopped first), the snapshots twice each" \
    "sbt-warm identity/testOnly services.identity.* worker/testOnly services.identity.* scripts.IdentityCalibrateSpec scripts.IdentityRefitSpec|sbt-warm --stop --root|it-local.sh worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec|sbt-warm e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec|sbt-warm e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec|sbt-warm web/PageTest/testOnly views.PageSnapshotSpec|sbt-warm web/PageTest/testOnly views.PageSnapshotSpec" \
    "$(calls | tail -n +3 | paste -sd '|' -)"
check "...the moved films appended to the report" "1" "$(grep -c '^+film B' "$tree/target/r.md")"
check "...the weights left changed in the worktree" "1" "$(scratch_git "$tree" diff --name-only | grep -c identity-weights.json)"
reset_tree

check "kept, --skip-checks: measure only" "0" "$(CI='' KEEP=1 refit "$tree" --skip-checks)"
check "...no check run" "2" "$(calls | wc -l | tr -d ' ')"
reset_tree

summary="$work/summary.md"
check "kept, in CI: the checks run" "0" "$(CI=true KEEP=1 MONGODB_URI=mongodb://ci GITHUB_STEP_SUMMARY="$summary" refit "$repo" --report identity-refit-report.md)"
check "...every sbt call plain sbt -batch, the hard clusters on the workflow's Mongo" "0 7 1" \
    "$(calls | grep -c '^sbt-warm') $(calls | grep -c '^sbt -batch') $(calls | grep -c '^sbt -batch worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec$')"
check "...the report in the step summary" "1" "$(grep -c 'Identity rule refit' "$summary")"
scratch_git "$repo" checkout -q -- . && scratch_git "$repo" clean -qfdx

spec_summary
