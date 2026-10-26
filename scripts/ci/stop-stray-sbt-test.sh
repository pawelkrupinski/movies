#!/usr/bin/env bash
# stop-stray-sbt.sh against stand-in JVMs: a `java` (a bash named java on its command line) left running in the
# build root and in a module under it, one in another build, and a non-JVM in the build root.
# Run: bash scripts/ci/stop-stray-sbt-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m stop-stray-sbt.sh\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'pkill -f "$work" 2>/dev/null; rm -rf "$work"' EXIT
mkdir -p "$work/build/worker" "$work/other" "$work/build-two"
alive() { kill -0 "$1" 2>/dev/null && echo alive || echo gone; }
# A process whose command line's program is $2 (a `java` as the JVM's own), holding $work in its arguments for the trap.
start() { (cd "$1" && exec -a "$2" bash -c 'while :; do sleep 1; done' "$work") > /dev/null 2>&1 & echo $!; }

check "nothing running: exits 0 and says nothing" "0:" "$(bash "$REPO_ROOT/scripts/ci/stop-stray-sbt.sh" "$work/build" 2>&1; echo "$?:")"

sbt=$(start "$work/build" java)
forked=$(start "$work/build/worker" java)
elsewhere=$(start "$work/other" java)
sibling=$(start "$work/build-two" java)
helper=$(start "$work/build" sleeper)
sleep 0.3

out=$(bash "$REPO_ROOT/scripts/ci/stop-stray-sbt.sh" "$work/build" 2>&1)
check "ends the strays and succeeds" "0" "$?"
check "...the JVM in the build root is gone" "gone" "$(alive "$sbt")"
check "...the JVM in a module under it is gone" "gone" "$(alive "$forked")"
check "...each named in a warning" "2" "$(grep -c '^::warning::ending a JVM' <<< "$out")"
check "...another build's JVM is left alone" "alive" "$(alive "$elsewhere")"
check "...a sibling directory sharing the root's prefix is another build" "alive" "$(alive "$sibling")"
check "...and a process that is no JVM is left alone" "alive" "$(alive "$helper")"

# From the build root itself, as the workflow runs it.
again=$(start "$work/build" java)
sleep 0.3
(cd "$work/build" && bash "$REPO_ROOT/scripts/ci/stop-stray-sbt.sh" > /dev/null 2>&1)
check "defaults to the current directory as the build root" "gone" "$(alive "$again")"

spec_summary
