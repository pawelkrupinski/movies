#!/usr/bin/env bash
#
# Grow and re-record the hard-cluster fixture that
# worker/src/it/scala/HardClusterConvergenceIntegrationSpec.scala replays on every push
# (the recording: integration.HardClusterRecord).
#
#   scripts/hard-clusters.sh ratchet <code> <convergence.log>...
#       Add the clusters of every film the log names as divergent, churning or split
#       (scripts/convergence-findings.py) to the fixture, then re-record that
#       country's responses. The ratchet: a cluster a 1.5-5 hour convergence leg caught
#       becomes a cluster the itAll layer checks in two minutes, for good.
#
#   scripts/hard-clusters.sh record [<code>...]
#       Re-record the responses file(s) only — after a change that makes the pipeline
#       ask new questions (the spec reports them as "unrecorded request(s)"). What the
#       passes find (a divergence, churn) is printed, never stops the recording. Answers the
#       file already holds are kept (the tree has moved on since they were recorded) and
#       only new requests reach the tree; delete the file to re-record all of it. To make
#       a fix's regression visible, record once WITH the bug and once with the fix.
#
# Inputs, each defaulting to what CI's convergence leg has already restored:
#   KINOWO_HARD_CLUSTERS_CORPUS_DIR  directory holding cinema-scrapes-<code>.json.gz
#                                    (default test/resources/fixtures/corpus)
#   KINOWO_FIXTURE_ROOT              directory holding enrichment-<code>/ (default
#                                    test/resources/fixtures)
#   MONGODB_URI                      a THROWAWAY Mongo (default the local 28017 one)
# When the corpus or a tree is absent, both come from ONE recording: the newest successful
# "Record scrape fixtures" run holding the first country's corpus (or KINOWO_HARD_CLUSTERS_RECORDING),
# its corpus and enrichment trees downloaded once into the shared cache (scripts/identity-tools.sh).
# A recording that cannot be found or downloaded stops the script, naming it.
#
# Commit what it changes under test/resources/fixtures/corpus/ (hard-clusters-*) with
# the fix the finding led to.
set -euo pipefail
cd "$(dirname "$0")/.."

# shellcheck disable=SC2034 # read by identity-tools.sh's identity_log
IDENTITY_TOOL=hard-clusters
# shellcheck source=scripts/identity-tools.sh
. scripts/identity-tools.sh

mode=${1:-}; shift || true
MONGODB_URI=${MONGODB_URI:-$(local_mongo_uri)}
export MONGODB_URI
corpus_dir=${KINOWO_HARD_CLUSTERS_CORPUS_DIR:-test/resources/fixtures/corpus}
fixture_root=${KINOWO_FIXTURE_ROOT:-test/resources/fixtures}
run=${KINOWO_HARD_CLUSTERS_RECORDING:-}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/hard-clusters.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

# recorded <code> -> sets `recorded` to the shared cache's fixtures directory of the ONE recording every absent input
# comes from (the first asked for picks it)
recorded() {
    local code=$1
    if [ -z "$run" ]; then
        run=$(identity_newest_recording "$code") || { echo "[hard-clusters] no recording to fetch the $code inputs from (above)" >&2; exit 1; }
    fi
    recorded=$(identity_recording "$run" "$code") || { echo "[hard-clusters] could not fetch recording $run's $code corpus and tree (above)" >&2; exit 1; }
}

ensure_corpus() {
    local code=$1
    [ -f "$corpus_dir/cinema-scrapes-$code.json.gz" ] && return 0
    recorded "$code"
    corpus_dir=$recorded/corpus
    echo "[hard-clusters] the $code corpus: recording run $run ($corpus_dir)"
}

ensure_tree() {
    local code=$1
    [ -d "$fixture_root/enrichment-$code" ] && return 0
    # A real directory, not a symlink: FakeHttpFetch refuses a path whose real form differs.
    recorded "$code"
    fixture_root=$recorded
    echo "[hard-clusters] the $code enrichment tree: recording run $run ($fixture_root)"
}

# sbt_step <log> <sbt args...> -> sbt, its lines worth reading shown; a failed sbt fails the script
sbt_step() {
    local log=$1; shift
    if ! sbt -batch "$@" > "$log" 2>&1; then
        tail -40 "$log" >&2
        echo "[hard-clusters] sbt $* failed (log above)" >&2
        exit 1
    fi
    grep -E '\[hard-clusters\]|recorded [0-9]+ responses|FAILED|error' "$log" || true
}

record() {
    local codes=("$@")
    [ ${#codes[@]} -gt 0 ] || codes=(pl uk de us es)
    for code in "${codes[@]}"; do ensure_tree "$code"; done
    local root
    root=$(cd "$fixture_root" && pwd)
    sbt_step "$scratch/record.log" "worker/IntegrationTest/runMain integration.HardClusterRecord ${codes[*]} --fixtures $root"
}

case "$mode" in
    ratchet)
        code=${1:?usage: hard-clusters.sh ratchet <code> <convergence.log>...}; shift
        [ $# -gt 0 ] || { echo "no logs given" >&2; exit 2; }
        scripts/convergence-findings.py "$code" "$@" > "$scratch/findings.tsv"
        if [ ! -s "$scratch/findings.tsv" ]; then
            echo "[hard-clusters] the log names no film — nothing to add"; exit 0
        fi
        echo "[hard-clusters] findings:"; sed 's/^/  /' "$scratch/findings.tsv"
        ensure_corpus "$code"
        sbt_step "$scratch/extend.log" "worker/Test/runMain scripts.BuildHardClusters extend $corpus_dir $scratch/findings.tsv"
        record "$code"
        ;;
    record)
        record "$@"
        ;;
    *)
        sed -n '3,31p' "$0"; exit 2
        ;;
esac
