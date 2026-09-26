#!/usr/bin/env bash
#
# Regenerates the identity resolver's calibrated evidence model from data
# (docs/design/identity-resolver.md §calibration):
#
#   common/src/main/resources/identity-weights.json          weights, calibration map, thresholds, cannot-link rules
#   test/resources/fixtures/identity/identity-labels.json.gz  the labelled set, with its train/calibration/test split
#   $REPORT/calibration-report.{md,json}, contradicted-prod-resolutions.tsv, prod-cross-check-mismatches.tsv
#
# Inputs:
#   CORPORA   dir holding cinema-scrapes-<cc>.json.gz (the recorder's full corpora, run 36153174348)
#   FIXTURES  dir holding enrichment-<cc>/ (the matching recorded enrichment trees, real directories)
#   PROD      optional dir for the production snapshot. With --extract-prod the snapshot is (re)read
#             from production first: READ-ONLY find() over movies + movie_slots of every country
#             database (scripts/identity-calibrate/extract-prod.js), through the local prod tunnel
#             (scripts/local-mirror/prod-tunnel.sh; 127.0.0.1:27017) and .env.local's MONGODB_URI.
#             Without PROD no listing has a proposal to corroborate, so nothing is labelled.
#
# The report formats numbers in the JVM locale: the command pins it to en-US ("0.97", not "0,97").
#
# Usage:
#   CORPORA=… FIXTURES=… PROD=… [REPORT=…] [EPSILON=certified|<rate>] scripts/identity-calibrate.sh [--extract-prod]
set -euo pipefail
cd "$(dirname "$0")/.."

: "${CORPORA:?set CORPORA to the dir of cinema-scrapes-<cc>.json.gz}"
: "${FIXTURES:?set FIXTURES to the dir of enrichment-<cc>/ trees}"
: "${PROD:?set PROD to the dir of the production snapshot (prod-<db>.jsonl)}"
REPORT="${REPORT:-target/identity-calibration}"
EPSILON="${EPSILON:-certified}"
VERSION="${VERSION:-calibration-$(date -u +%Y-%m-%d)}"

if [[ "${1:-}" == "--extract-prod" ]]; then
  env_file="${ENV_FILE:-.env.local}"
  uri=$(grep '^MONGODB_URI=' "$env_file" | head -1 | cut -d= -f2- | sed 's/^"//;s/"$//')
  [[ -n "$uri" ]] || { echo "MONGODB_URI not in $env_file" >&2; exit 1; }
  nc -z 127.0.0.1 27017 || { echo "no prod tunnel on 127.0.0.1:27017 — start one with scripts/local-mirror/prod-tunnel.sh" >&2; exit 1; }
  mkdir -p "$PROD"
  for db in kinowo kinowo_uk kinowo_de kinowo_es kinowo_us; do
    KINOWO_CALIBRATE_DB=$db mongosh "$uri" --quiet scripts/identity-calibrate/extract-prod.js > "$PROD/prod-$db.jsonl"
    echo "prod snapshot $db: $(wc -l < "$PROD/prod-$db.jsonl") rows"
  done
fi

sbt -J-Xmx12g -J-Duser.language=en -J-Duser.country=US -batch "worker/Test/runMain scripts.IdentityCalibrate --corpora $CORPORA --fixtures $FIXTURES --prod $PROD --report $REPORT --epsilon $EPSILON --version $VERSION ${COUNTRIES:+--countries $COUNTRIES}"
