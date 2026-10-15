#!/usr/bin/env bash
#
# Re-capture the unmatched-cluster fixture (test/resources/fixtures/identity-unmatched/<cc>.json.gz)
# in one command — docs/design/identity-resolver.md §20.14 item 5.
#
#   scripts/identity-capture.sh [--dry-run] [--capture|--fill] [--parallel 2] [--heap 12g] [cc...]
#
#   cc...       countries to capture (default: us de uk pl es, run largest first)
#   --capture   capture every country named, whatever its fixture's inputs
#   --fill      fill every country named (its <cc>.json.gz must exist)
#   --parallel  countries run side by side, one JVM each (default 2; fewer when memory holds fewer
#               12g heaps beside 4 GB for the rest), sharing 4 live reads per host between them
#   --heap      each country's JVM heap (default 12g)
#   --dry-run   print the plan — what it would download, export and run, with every variable — and
#               touch neither prod nor the network
#
# CAPTURE OR FILL, per country, said and logged: a FILL (UnmatchedClustersFillIntegrationSpec, minutes)
# answers only the questions a change newly asks of the fixture's clusters; a CAPTURE resolves the
# whole corpus again. It fills when <cc>.json.gz exists and its decisions are unchanged under the
# current code — <cc>.inputs, stamped by each capture, names the same recording and the same hash of
# the code deciding them (scripts.IdentityCapture.DecisionInputs) — and captures otherwise.
#
# What a capture does (scripts.IdentityCapture, worker/src/test/scala/scripts, does the work):
#   1. fetches the newest successful "Record scrape fixtures" run's corpora (artifact
#      scrape-fixtures-<cc>) and the enrichment trees recorded with them (release convergence-fixtures,
#      enrichment-<cc>-<run>.tar.*) into $KINOWO_IDENTITY_CAPTURE_WORK (default target/identity-capture),
#      skipping a country whose download is already that run's;
#   2. exports prod's identity_family_answers (kinowo, kinowo_<cc>) READ-ONLY over the prod tunnel
#      (scripts/local-mirror/prod-tunnel.sh; the URI is the Touch ID vault's MONGODB_URI, unless
#      exported, rewritten to the tunnel by prod_tunnel_uri and never printed) — or over
#      $KINOWO_IDENTITY_FAMILY_URI where the caller (CI) already holds a read-only route;
#   3. runs each country's UnmatchedClustersCaptureIntegrationSpec in a JVM of its own, against the
#      it/ Mongo (127.0.0.1:28017) in a database of its own, every variable defaulted (a variable you
#      set is kept), the TMDB key read from the Touch ID vault (TMDB_API_KEY) unless
#      KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY is set — both secrets in ONE `secrets run` (one Touch ID):
#      the script re-runs itself under it before compiling, naming only the keys still missing; a country whose capture read venue pages live
#      ("capture again") is captured again to answer them, while each capture reads more (at most 4 captures);
#   4. prints each phase's time and throughput, and exits non-zero, naming them, when any country's JVM
#      failed, was killed or printed no capture.
#
# Then judge every new take into identity-judgements.tsv and re-baseline expected-matches.tsv (the ratchet,
# worker/src/test/scala/services/identity/UnmatchedClustersRatchetSpec.scala).
set -euo pipefail
self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
# shellcheck source=scripts/identity-tools.sh
. "$(dirname "$self")/identity-tools.sh" # identity_classpath_line; tool-conventions.sh's usage_from_header
cd "$(dirname "$0")/.."

dry=false
fill_only=false
for arg in "$@"; do
    case "$arg" in
        --dry-run) dry=true ;;
        --fill) fill_only=true ;;
        -h|--help) usage_from_header "$self"; exit 0 ;;
    esac
done

# The secrets, up front and together, so a missing one costs one Touch ID before the compile rather
# than a failure after it. A fill reads no family answers from prod, and a caller-supplied route or
# seed replaces the prod read.
# shellcheck source=scripts/local-mirror/prod-tunnel.sh
. scripts/local-mirror/prod-tunnel.sh
reads_prod=false
if ! $dry && ! $fill_only && [ -z "${KINOWO_IDENTITY_FAMILY_URI:-}" ] && [ -z "${KINOWO_IDENTITY_FAMILY_SEED:-}" ]; then
    reads_prod=true
fi
if ! $dry; then
    needed=(KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY)
    # Under its own name, NOT MONGODB_URI: the capture JVMs inherit this environment, and a
    # MONGODB_URI there replaces each one's local throwaway database (IdentityCapture.environment).
    $reads_prod && needed+=(KINOWO_PROD_MONGODB_URI=MONGODB_URI)
    ensure_vault_env "${needed[@]}" -- "$BASH" "$self" "$@" || exit 2
fi

work=${KINOWO_IDENTITY_CAPTURE_WORK:-target/identity-capture}
mkdir -p "$work"

# The integration classpath, compiled once: every country's JVM runs on it.
echo "[identity-capture] compiling the integration classpath"
# sbt's whole output goes to a log, and only a line that IS a classpath is kept: on 2026-10-12 the
# first scheduled run handed `java -cp` whatever sbt printed last and died "Could not find or load
# main class scripts.IdentityCapture" with the reason swallowed.
if ! sbt -batch "worker/IntegrationTest/compile" "export worker/IntegrationTest/fullClasspath" > "$work/sbt.log" 2>&1; then
    cat "$work/sbt.log" >&2
    echo "[identity-capture] sbt failed to compile the integration classpath (log above)" >&2
    exit 1
fi
identity_classpath_line "$work/sbt.log" > "$work/classpath" || true
classes=$(tr ':' '\n' < "$work/classpath" | grep -E '/worker/target/[^/]+/test-classes$' || true)
if [ -z "$classes" ] || [ ! -f "$classes/scripts/IdentityCapture.class" ]; then
    cat "$work/sbt.log" >&2
    echo "[identity-capture] no classpath holding scripts.IdentityCapture in sbt's output (log above)" >&2
    exit 1
fi

if ! $dry; then
    export KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY
fi
# The tunnel is for a capture's export only.
if $reads_prod; then
    init_prod_tunnel identity-capture "$(prod_tunnel_uri "$KINOWO_PROD_MONGODB_URI")"
    unset KINOWO_PROD_MONGODB_URI
    trap close_prod_tunnel EXIT INT TERM
    ensure_prod_tunnel || { echo "[identity-capture] prod Mongo unreachable — can you 'ssh' to the mongo host? (see prod-tunnel.sh)" >&2; exit 1; }
    KINOWO_IDENTITY_FAMILY_URI=$TUNNEL_PROBE_URI
    export KINOWO_IDENTITY_FAMILY_URI
fi

java -Xmx2g -cp "$(cat "$work/classpath")" scripts.IdentityCapture "$@"
