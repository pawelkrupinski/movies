#!/usr/bin/env bash
#
# Re-capture the unmatched-cluster fixture (test/resources/fixtures/identity-unmatched/<cc>.json.gz)
# in one command — docs/design/identity-resolver.md §20.14 item 5.
#
#   scripts/identity-capture.sh [--dry-run] [--capture|--fill] [--heap 12g] [cc...]
#
#   cc...       countries to capture (default: us de uk pl es, run largest first)
#   --capture   capture every country named, whatever its fixture's inputs
#   --fill      fill every country named (its <cc>.json.gz must exist)
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
#      (scripts/local-mirror/prod-tunnel.sh; the URI is .env.local's MONGODB_URI, read by envval's
#      single-line grep and never printed) — or over $KINOWO_IDENTITY_FAMILY_URI where the caller (CI)
#      already holds a read-only route;
#   3. runs each country's UnmatchedClustersCaptureIntegrationSpec in a JVM of its own, against the
#      it/ Mongo (127.0.0.1:28017) in a database of its own, every variable defaulted (a variable you
#      set is kept), the TMDB key read from the Touch ID vault (`secrets get movies TMDB_API_KEY`)
#      unless KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY is set;
#   4. prints each phase's time and throughput.
#
# Then judge every new take into labels.tsv and re-baseline expected-matches.tsv (the ratchet,
# worker/src/test/scala/services/identity/UnmatchedClustersRatchetSpec.scala).
set -euo pipefail
cd "$(dirname "$0")/.."

dry=false
fill_only=false
for arg in "$@"; do
    case "$arg" in
        --dry-run) dry=true ;;
        --fill) fill_only=true ;;
        -h|--help) sed -n '3,37p' "$0"; exit 0 ;;
    esac
done

work=${KINOWO_IDENTITY_CAPTURE_WORK:-target/identity-capture}
mkdir -p "$work"

# The integration classpath, compiled once: every country's JVM runs on it.
echo "[identity-capture] compiling the integration classpath"
sbt -batch -error "export worker/IntegrationTest/fullClasspath" | tail -n 1 > "$work/classpath"
[ -s "$work/classpath" ] || { echo "[identity-capture] sbt printed no classpath" >&2; exit 1; }

if ! $dry; then
    if [ -z "${KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY:-}" ]; then
        if ! KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=$(secrets get movies TMDB_API_KEY 2>/dev/null) || [ -z "$KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" ]; then
            echo "[identity-capture] no TMDB key: run 'secrets unlock movies' (Touch ID) or set KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" >&2
            exit 2
        fi
        export KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY
    fi
    # A fill reads no family answers from prod; the tunnel is for a capture's export only.
    if ! $fill_only && [ -z "${KINOWO_IDENTITY_FAMILY_URI:-}" ] && [ -z "${KINOWO_IDENTITY_FAMILY_SEED:-}" ]; then
        # shellcheck source=scripts/local-mirror/prod-tunnel.sh
        . scripts/local-mirror/prod-tunnel.sh
        # .env.local lives in the main checkout; a worktree has none of its own
        env_file=$PWD/.env.local
        [ -f "$env_file" ] || env_file="$(cd "$(git rev-parse --git-common-dir)/.." && pwd)/.env.local"
        init_prod_tunnel identity-capture "$(envval MONGODB_URI "$env_file")" "$env_file"
        trap close_prod_tunnel EXIT INT TERM
        ensure_prod_tunnel || { echo "[identity-capture] prod Mongo unreachable — can you 'ssh' to the mongo host? (see prod-tunnel.sh)" >&2; exit 1; }
        KINOWO_IDENTITY_FAMILY_URI=$TUNNEL_PROBE_URI
        export KINOWO_IDENTITY_FAMILY_URI
    fi
fi

java -Xmx2g -cp "$(cat "$work/classpath")" scripts.IdentityCapture "$@"
