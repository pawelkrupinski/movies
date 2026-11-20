#!/usr/bin/env bash
# scripts/identity-boot-cost against a stub sbt and a stub java: it compiles worker's tests and reads their classpath
# from sbt, runs the measure in a JVM of its own without escape analysis (the runs asked passed on), and holds the rows
# to the baseline -- within it exit 0, a rise past the tolerance exit 1, --update rewrites the baseline, a failed
# measure exit 2. Run: bash scripts/identity-boot-cost-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m identity-boot-cost\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cat > "$work/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*" >> "$STUB_CALLS"
echo "[info] compiling"
echo "/cp/worker-test-classes:/cp/scala-library.jar"
STUB
cat > "$work/java" <<'STUB'
#!/usr/bin/env bash
echo "java $*" >> "$STUB_CALLS"
[ -n "${STUB_JAVA_FAIL:-}" ] && { echo "Exception in thread main"; exit 1; }
for a in "$@"; do case "$a" in *rows.tsv) out=$a ;; esac; done
boot=${STUB_BOOT_BYTES:-20000000}
printf 'resolve\t*\t10\t10000000\t5000000\nboot\t*\t10\t%s\t6000000\nboot-steady\t*\t10\t1000000\t100000\n' "$boot" > "$out"
echo "IdentityBootCost: 90 slices, 3 runs a phase"
STUB
chmod +x "$work/sbt" "$work/java"
printf 'tolerance\t2\nphase\tresolve\t10\t1000000\t500000\nphase\tboot\t10\t2000000\t600000\nphase\tboot-steady\t10\t100000\t10000\n' > "$work/baseline.tsv"
export STUB_CALLS="$work/calls" KINOWO_BOOT_COST_SBT="$work/sbt" KINOWO_BOOT_COST_JAVA="$work/java" KINOWO_BOOT_COST_BASELINE="$work/baseline.tsv"

: > "$STUB_CALLS"
out=$("$REPO_ROOT/scripts/identity-boot-cost" --runs 5 2>&1); status=$?
check "the measure as recorded: exit 0" "0" "$status"
check "...worker's tests compiled and their classpath exported" "sbt worker/Test/compile export worker/Test/fullClasspath" "$(grep '^sbt' "$STUB_CALLS")"
check "...the measure in its own JVM, without escape analysis, on that classpath, the runs passed on" "1" \
  "$(grep -c -- '^java -XX:-DoEscapeAnalysis -XX:AutoBoxCacheMax=16384 .* -cp /cp/worker-test-classes:/cp/scala-library.jar scripts.IdentityBootCost .*/rows.tsv 5$' "$STUB_CALLS")"
check "...every phase ok" "3" "$(echo "$out" | grep -c '| ok |$')"

out=$(STUB_BOOT_BYTES=20500000 "$REPO_ROOT/scripts/identity-boot-cost" 2>&1); status=$?
check "boot's bytes a family 2.5% up: exit 1, said" "1 1" "$status $(echo "$out" | grep -c '| boot | .*ROSE past the tolerance')"

out=$(STUB_BOOT_BYTES=20500000 "$REPO_ROOT/scripts/identity-boot-cost" --update 2>&1); status=$?
check "--update: exit 0, the baseline rewritten, its tolerance kept" "0 1 1" \
  "$status $(grep -c '^phase	boot	10	2050000	600000$' "$work/baseline.tsv") $(grep -c '^tolerance	2$' "$work/baseline.tsv")"

out=$(STUB_JAVA_FAIL=1 "$REPO_ROOT/scripts/identity-boot-cost" 2>&1); status=$?
check "a measure that failed: exit 2, with its log's tail" "2 1" "$status $(echo "$out" | grep -c 'Exception in thread main')"

out=$("$REPO_ROOT/scripts/identity-boot-cost" --runs 0 2>&1); status=$?
check "--runs 0: a usage error, exit 2" "2" "$status"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/identity-boot-cost" --help | grep -c '^THE IDENTITY BOOT-COST CHECK')"
spec_summary
