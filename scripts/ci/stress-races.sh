#!/usr/bin/env bash
# Run the race / concurrency harnesses many times over, to surface the interleavings a single CI
# run almost never meets. The nightly stress workflow runs this with 50; a push runs each once.
#
# A harness that fails 1 time in 50 is a real race that will redden some push, some day, and be
# "fixed" by a rerun. Here it fails the night it can, with a count. Every iteration runs even
# after a failure, so the count is honest; the run fails if any iteration did.
#
# Usage: stress-races.sh <iterations> [unit|it|all]
#   unit  the in-JVM harnesses (no Mongo)
#   it    the Mongo-backed ones (MONGODB_URI / MONGODB_DB must point at a replica set)
#
# The list is HERE, once. A listed harness whose class no longer exists fails the run rather than
# being silently dropped; a new harness is added by adding its line.
set -uo pipefail

iterations="${1:?iterations}"
scope="${2:-all}"
[[ "$iterations" =~ ^[1-9][0-9]{0,3}$ ]] || { echo "iterations must be a number, 1-9999" >&2; exit 2; }

cd "$(git rev-parse --show-toplevel)" || exit 1
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# "<module>/<Config> <fully qualified class>"
Unit=(
    "common/Test tools.ConcurrentCandidateProbeSpec"
    "worker/Test services.sharecards.ShareCardConcurrencySpec"
    "worker/Test tools.BoundedParallelSpec"
    "worker/Test tools.AdaptiveParallelSpec"
    "worker/Test tools.ParallelDetailFetchSpec"
    "worker/Test tools.FilmwebDiffParallelFetchSpec"
)
It=(
    "web/IntegrationTest integration.HiddenFilmsConcurrentWritesIntegrationSpec"
    "worker/IntegrationTest integration.StagingFoldConcurrentTmdbRaceIntegrationSpec"
)

case "$scope" in
    unit) harnesses=("${Unit[@]}") ;;
    it)   harnesses=("${It[@]}") ;;
    all)  harnesses=("${Unit[@]}" "${It[@]}") ;;
    *)    echo "scope must be unit, it or all" >&2; exit 2 ;;
esac

# Every listed class must still exist where its line says.
missing=0
for h in "${harnesses[@]}"; do
    read -r target cls <<< "$h"
    module="${target%%/*}"
    case "${target#*/}" in IntegrationTest) dir="$module/src/it" ;; *) dir="$module/src/test" ;; esac
    if ! git grep -qw -e "class ${cls##*.}" -- "$dir"; then
        echo "::error::stress-races.sh lists ${cls} under $target, but no such class is in $dir"
        missing=1
    fi
done
[ "$missing" -eq 0 ] || exit 1

# One testOnly per module/config, each iteration: "<target> <class> <class>...".
commands=()
while IFS= read -r target; do
    classes=$(printf '%s\n' "${harnesses[@]}" | awk -v t="$target" '$1 == t { printf "%s ", $2 }')
    commands+=("$target/testOnly ${classes% }")
done < <(printf '%s\n' "${harnesses[@]}" | awk '{ print $1 }' | sort -u)

out=target/stress-races
rm -rf "$out" target/test-reports/unit target/test-reports/it
mkdir -p "$out"
: > "$out/failures.tsv"
failed_iterations=0
for i in $(seq 1 "$iterations"); do
    iteration_failed=0
    for c in "${commands[@]}"; do
        if ! sbt --client "$c" > "$out/iteration.log" 2>&1; then
            iteration_failed=1
            tail -n 60 "$out/iteration.log"
        fi
    done
    if [ "$iteration_failed" -eq 1 ]; then
        failed_iterations=$((failed_iterations + 1))
        python3 "$here/flake_verdict.py" failed target/test-reports/unit target/test-reports/it \
            | tr '\037' '\t' | cut -f2- | awk -v i="$i" '{ print i "\t" $0 }' >> "$out/failures.tsv"
        mkdir -p "$out/iteration-$i" && cp -R target/test-reports/. "$out/iteration-$i/" 2>/dev/null || true
    fi
    echo "iteration $i/$iterations: $([ "$iteration_failed" -eq 1 ] && echo FAILED || echo ok)"
done
sbt --client shutdown >/dev/null 2>&1 || true

{
    echo "### Race harnesses × $iterations ($scope)"
    echo
    echo "$((iterations - failed_iterations))/$iterations iterations passed."
    if [ -s "$out/failures.tsv" ]; then
        echo
        echo "| Failed in | Class | Test |"
        echo "|---|---|---|"
        cut -f2- "$out/failures.tsv" | sort | uniq -c | sort -rn \
            | awk -v n="$iterations" 'BEGIN { FS = "\t" } { split($1, a, " "); count = a[1]; sub(/^ *[0-9]+ /, "", $1); printf "| %d/%d | `%s` | %s |\n", count, n, $1, $2 }'
    fi
} | tee -a "${GITHUB_STEP_SUMMARY:-/dev/null}"

[ "$failed_iterations" -eq 0 ]
