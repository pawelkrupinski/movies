#!/usr/bin/env bash
#
# One country's identity measurement run, in the job (.github/workflows/identity-measure.yml) or on a laptop with the
# same corpus staged: IdentityShadowIntegrationSpec over the whole corpus, either BOOTING the base's production into the
# pipeline cache or MEASURING the checked-out code against it.
#
#   identity-measure-run.sh boot|measure <cc> <out-dir> <pipeline-cache-dir>
#
# Environment (each optional): MONGODB_URI (default the local mongod on 27017), KINOWO_IDENTITY_CORPUS_DIR (default this
# checkout's test/resources/fixtures/corpus), and for `measure` KINOWO_IDENTITY_ROBUSTNESS (default off) and
# KINOWO_IDENTITY_FOCUS; KINOWO_CONVERGENCE_CORPUS_RUN / KINOWO_CONVERGENCE_TREE_ASSET name the recording replayed;
# IDENTITY_MEASURE_REPO, the checkout to run in (default this script's own) — the workflow runs its own copy of this
# script against a base checked out from before the script existed.
# `measure` writes <out>/variant-<cc>.txt (the commit and recording) and <out>/run-<cc>.log (the suite's output).
# Exits with the suite's status; 64 on a bad argument.
# Tested by scripts/ci/identity-measure-run-test.sh.
set -uo pipefail

usage() { echo "usage: $0 boot|measure <cc> <out-dir> <pipeline-cache-dir>" >&2; exit 64; }
[ "$#" -eq 4 ] || usage
mode="$1" cc="$2" out="$3" cache="$4"
case "$mode" in boot|measure) ;; *) usage ;; esac
[[ "$cc" =~ ^[a-z]{2}$ ]] || { echo "$0: '$cc' is not a country code" >&2; exit 64; }
repo="${IDENTITY_MEASURE_REPO:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"

export MONGODB_URI="${MONGODB_URI:-mongodb://127.0.0.1:27017/?directConnection=true}"
export KINOWO_IDENTITY_CORPUS_DIR="${KINOWO_IDENTITY_CORPUS_DIR:-$repo/test/resources/fixtures/corpus}"
export KINOWO_IDENTITY_FULL="$cc" KINOWO_IDENTITY_OUT="$out" KINOWO_IDENTITY_PIPELINE_CACHE="$cache"
sbt_args=(-J-Xmx10g -J-Duser.language=en -J-Duser.country=US "worker/IntegrationTest/testOnly integration.IdentityShadowIntegrationSpec")

cd "$repo" || exit 1
if [ "$mode" = boot ]; then
    export KINOWO_IDENTITY_BOOT_ONLY=true
    exec sbt "${sbt_args[@]}"
fi
export KINOWO_IDENTITY_ROBUSTNESS="${KINOWO_IDENTITY_ROBUSTNESS:-off}"
[ -n "${KINOWO_IDENTITY_FOCUS:-}" ] && export KINOWO_IDENTITY_FOCUS
mkdir -p "$out"
{ git -C "$repo" log --oneline -1 2>/dev/null || echo "(no commit)"
  echo "recording ${KINOWO_CONVERGENCE_CORPUS_RUN:-?} tree ${KINOWO_CONVERGENCE_TREE_ASSET:-?}"; } > "$out/variant-$cc.txt"
sbt "${sbt_args[@]}" 2>&1 | tee "$out/run-$cc.log"
