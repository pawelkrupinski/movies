#!/usr/bin/env bash
#
# Grow and re-record the hard-cluster fixture that
# worker/src/it/scala/HardClusterConvergenceIntegrationSpec.scala replays on every push.
#
#   scripts/hard-clusters.sh ratchet <code> <convergence.log>...
#       Add the clusters of every film the log names as divergent, churning or split
#       (scripts/convergence-findings.py) to the fixture, then re-record that
#       country's responses. The ratchet: a cluster a 1.5-5 hour convergence leg caught
#       becomes a cluster the itAll layer checks in two minutes, for good.
#
#   scripts/hard-clusters.sh record [<code>...]
#       Re-record the responses file(s) only — after a change that makes the pipeline
#       ask new questions (the spec reports them as "unrecorded request(s)"). Answers the
#       file already holds are kept (the tree has moved on since they were recorded) and
#       only new requests reach the tree; delete the file to re-record all of it. To make
#       a fix's regression visible, record once WITH the bug and once with the fix.
#
# Inputs, each defaulting to what CI's convergence leg has already restored:
#   KINOWO_HARD_CLUSTERS_CORPUS_DIR  directory holding cinema-scrapes-<code>.json.gz
#                                    (default test/resources/fixtures/corpus; fetched
#                                    from the newest "Record scrape fixtures" run when
#                                    absent)
#   KINOWO_FIXTURE_ROOT              directory holding enrichment-<code>/ (default
#                                    test/resources/fixtures; fetched from the
#                                    convergence-fixtures release when absent)
#   MONGODB_URI                      a THROWAWAY Mongo (default the local 28017 one)
#
# Commit what it changes under test/resources/fixtures/corpus/ (hard-clusters-*) with
# the fix the finding led to.
set -euo pipefail
cd "$(dirname "$0")/.."

mode=${1:-}; shift || true
export MONGODB_URI=${MONGODB_URI:-mongodb://127.0.0.1:28017/?directConnection=true}
corpus_dir=${KINOWO_HARD_CLUSTERS_CORPUS_DIR:-test/resources/fixtures/corpus}
fixture_root=${KINOWO_FIXTURE_ROOT:-test/resources/fixtures}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/hard-clusters.XXXXXX")
trap 'rm -rf "$scratch"' EXIT

ensure_corpus() {
    local code=$1
    [ -f "$corpus_dir/cinema-scrapes-$code.json.gz" ] && return 0
    local run
    run=$(gh run list --workflow "Record scrape fixtures" --status success --limit 1 --json databaseId --jq '.[0].databaseId')
    echo "[hard-clusters] fetching the $code corpus from recording run $run"
    gh run download "$run" --name "scrape-fixtures-$code" --dir "$scratch/scrapes-$code"
    tar -xzf "$scratch/scrapes-$code/scrapes-$code.tar.gz" -C "$scratch/scrapes-$code" 2>/dev/null || true
    corpus_dir="$scratch/scrapes-$code/test/resources/fixtures/corpus"
}

ensure_tree() {
    local code=$1
    [ -d "$fixture_root/enrichment-$code" ] && return 0
    echo "[hard-clusters] fetching the $code enrichment tree from the convergence-fixtures release"
    gh release download convergence-fixtures --pattern "enrichment-$code.tar.gz" --dir "$scratch" --clobber
    tar -xzf "$scratch/enrichment-$code.tar.gz" -C "$scratch"
    # A real directory, not a symlink: FakeHttpFetch refuses a path whose real form differs.
    fixture_root="$scratch/test/resources/fixtures"
}

record() {
    local codes=("$@")
    [ ${#codes[@]} -gt 0 ] || codes=(pl uk de us es)
    for code in "${codes[@]}"; do ensure_tree "$code"; done
    KINOWO_HARD_CLUSTERS_RECORD=1 KINOWO_FIXTURE_ROOT="$(cd "$fixture_root" && pwd)" \
    KINOWO_HARD_CLUSTERS_COUNTRIES=$(IFS=,; echo "${codes[*]}") \
        sbt -batch "worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec" \
        | grep -E '\[hard-clusters\]|recorded [0-9]+ responses|Tests:|FAILED' || true
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
        sbt -batch "worker/Test/runMain scripts.BuildHardClusters extend $corpus_dir $scratch/findings.tsv" \
            | grep -E '\[hard-clusters\]|error' || true
        record "$code"
        ;;
    record)
        record "$@"
        ;;
    *)
        sed -n '3,27p' "$0"; exit 2
        ;;
esac
