#!/usr/bin/env bash
# Boots the real Grafana image with our real provisioning AND a metrics backend
# that actually contains data — so template-variable interpolation can be
# verified instead of assumed.
#
# The gap this fills: `smoke-test.sh` proves the provisioning YAML/JSON parses,
# but its Grafana has no metrics backend at all. `label_values(...)` returns
# nothing there, so `$country` / `$worker_app` never populate and no panel
# query is ever interpolated with a real value. Every scoping bug that lives in
# the interpolation (the "All" → `.*` cross-country leak, a query missing the
# `$worker_app` matcher) is invisible to that test.
#
# Here a single-node VictoriaMetrics is seeded (see seed-metrics.py) with three
# worker apps across pl/uk/de, and the `app-metrics-live` datasource is pointed
# at it. The checked-in datasource YAML is NOT edited: provisioning is copied to
# a temp dir and the URL rewritten there.
#
# Usage:
#   fly/grafana/local-harness.sh            # boot, verify, print URL, exit (containers stay up)
#   fly/grafana/local-harness.sh --keep     # same, then block in the foreground until Ctrl-C
#   fly/grafana/local-harness.sh --down     # tear everything down
#
# Env overrides: HARNESS_GRAFANA_PORT (3998), HARNESS_VM_PORT (8429),
#                HARNESS_SEED_HOURS (2), HARNESS_SEED_STEP_SECONDS (60)
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Multi-stage Dockerfile (VictoriaMetrics builder, then the Grafana runtime we
# ship) — take the LAST FROM, same as smoke-test.sh.
GRAFANA_IMG="$(grep '^FROM ' "$DIR/Dockerfile" | tail -1 | awk '{print $2}')"
VM_IMG="${HARNESS_VM_IMAGE:-victoriametrics/victoria-metrics:v1.111.0}"

NET="kinowo-grafana-harness-net"
GRAFANA_NAME="kinowo-grafana-harness"
VM_NAME="kinowo-grafana-harness-vm"
GRAFANA_PORT="${HARNESS_GRAFANA_PORT:-3998}"
VM_PORT="${HARNESS_VM_PORT:-8429}"
SEED_HOURS="${HARNESS_SEED_HOURS:-2}"
SEED_STEP="${HARNESS_SEED_STEP_SECONDS:-60}"

down() {
  docker rm -f "$GRAFANA_NAME" "$VM_NAME" >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
}

if [ "${1:-}" = "--down" ]; then
  echo "==> tearing down the harness"
  down
  echo "==> down"
  exit 0
fi

KEEP=0
[ "${1:-}" = "--keep" ] && KEEP=1

WORK="$(mktemp -d)"
# Only the temp dir is unconditionally cleaned; the containers are the point of
# the harness and outlive a plain run (use --down to stop them). In --keep mode
# Ctrl-C tears the whole thing down.
cleanup() { rm -rf "$WORK"; }
trap cleanup EXIT

# Idempotent: a previous run's containers are replaced, never collided with.
down

echo "==> creating network $NET"
docker network create "$NET" >/dev/null

echo "==> booting $VM_IMG on :$VM_PORT"
docker run -d --name "$VM_NAME" --network "$NET" -p "$VM_PORT:8428" \
  "$VM_IMG" \
  -retentionPeriod=1 -search.latencyOffset=1s >/dev/null

echo "==> waiting for VictoriaMetrics"
for _ in $(seq 1 60); do
  if curl -fsS "http://localhost:$VM_PORT/health" >/dev/null 2>&1; then vm_ok=1; break; fi
  sleep 1
done
[ "${vm_ok:-0}" = 1 ] || { echo "FAIL: VictoriaMetrics never became healthy"; docker logs "$VM_NAME" | tail -40; exit 1; }

echo "==> seeding ${SEED_HOURS}h of samples at ${SEED_STEP}s steps (3 worker apps: pl/uk/de)"
python3 "$DIR/seed-metrics.py" --hours "$SEED_HOURS" --step-seconds "$SEED_STEP" \
  > "$WORK/seed.prom"
curl -fsS --data-binary "@$WORK/seed.prom" \
  "http://localhost:$VM_PORT/api/v1/import/prometheus" >/dev/null
# VictoriaMetrics ingests asynchronously; force the pending rows to disk so the
# label-values assertions below don't race the flush.
curl -fsS "http://localhost:$VM_PORT/internal/force_flush" >/dev/null
echo "    seeded $(wc -l < "$WORK/seed.prom" | tr -d ' ') samples"

# Copy provisioning rather than bind-mounting the source tree: keeps the
# checked-in datasource YAML untouched while we repoint it, and sidesteps the
# Docker Desktop bind-mount staleness smoke-test.sh already works around.
cp -R "$DIR/provisioning/." "$WORK/provisioning/"
python3 - "$WORK/provisioning/datasources/app-metrics-live.yaml" "http://$VM_NAME:8428" <<'PY'
import sys
path, url = sys.argv[1], sys.argv[2]
text = open(path).read()
patched = text.replace("url: http://127.0.0.1:8428", "url: %s" % url)
assert patched != text, "datasource URL line not found — did app-metrics-live.yaml change?"
open(path, "w").write(patched)
PY

echo "==> booting $GRAFANA_IMG on :$GRAFANA_PORT"
docker run -d --name "$GRAFANA_NAME" --network "$NET" -p "$GRAFANA_PORT:3000" \
  -e GF_SECURITY_ADMIN_USER=admin \
  -e GF_SECURITY_ADMIN_PASSWORD=admin \
  -e GF_AUTH_ANONYMOUS_ENABLED=false \
  -e FLY_PROMETHEUS_TOKEN=dummy-token \
  -e TELEGRAM_BOT_TOKEN=dummy:bottoken \
  -v "$WORK/provisioning:/etc/grafana/provisioning:ro" \
  "$GRAFANA_IMG" >/dev/null

echo "==> waiting for Grafana /api/health"
for _ in $(seq 1 60); do
  if curl -fsS "http://localhost:$GRAFANA_PORT/api/health" >/dev/null 2>&1; then gf_ok=1; break; fi
  sleep 2
done
[ "${gf_ok:-0}" = 1 ] || { echo "FAIL: Grafana never became healthy"; docker logs "$GRAFANA_NAME" | tail -40; exit 1; }

base="http://admin:admin@localhost:$GRAFANA_PORT"
proxy="$base/api/datasources/proxy/uid/app-metrics-live/api/v1"
fail=0

# The actual point of the harness: Grafana must see the seeded series THROUGH
# the provisioned datasource, which is what makes `label_values(...)` resolve.
assert_labels() { # label  expected-csv
  local got
  got=$(curl -fsS "$proxy/label/$1/values" \
    | python3 -c "import sys,json;print(','.join(sorted(json.load(sys.stdin)['data'])))") \
    || { echo "FAIL: could not read $1 label values through the datasource proxy"; fail=1; return; }
  if [ "$got" = "$2" ]; then
    echo "PASS: \$$1 resolves to [$got]"
  else
    echo "FAIL: \$$1 resolved to [$got], expected [$2]"
    fail=1
  fi
}

assert_labels country "de,pl,uk"
assert_labels app "kinowo-worker,kinowo-worker-de,kinowo-worker-uk"

# `$worker_app` is resolved from kinowo_worker_throttled scoped by $country —
# reproduce that interpolation to prove country scoping actually narrows it.
for c in pl uk de; do
  expected="kinowo-worker"; [ "$c" = pl ] || expected="kinowo-worker-$c"
  got=$(curl -fsS --get --data-urlencode "match[]=kinowo_worker_throttled{country=~\"$c\"}" \
    "$proxy/label/app/values" \
    | python3 -c "import sys,json;print(','.join(sorted(json.load(sys.stdin)['data'])))")
  if [ "$got" = "$expected" ]; then
    echo "PASS: \$worker_app scoped to country=$c resolves to [$got]"
  else
    echo "FAIL: \$worker_app scoped to country=$c resolved to [$got], expected [$expected]"
    fail=1
  fi
done

# The asymmetry the dashboards depend on: jvm_* has no country label, so
# $worker_app is the only thing scoping those panels. If this ever returns a
# country the seed (and the assumption) is wrong.
got=$(curl -fsS --get --data-urlencode 'match[]=jvm_memory_max_bytes' \
  "$proxy/label/country/values" \
  | python3 -c "import sys,json;print(','.join(sorted(json.load(sys.stdin).get('data') or [])))")
if [ -z "$got" ]; then
  echo "PASS: jvm_memory_max_bytes carries no country label (so \$worker_app must scope it)"
else
  echo "FAIL: jvm_memory_max_bytes unexpectedly carries country=[$got]"
  fail=1
fi

echo
if [ "$fail" = 0 ]; then
  echo "== HARNESS UP =="
else
  echo "== HARNESS UP, BUT ASSERTIONS FAILED =="
fi
echo "  Grafana:          http://localhost:$GRAFANA_PORT  (admin / admin)"
echo "  Overview:         http://localhost:$GRAFANA_PORT/d/fly-overview"
echo "  Worker diag:      http://localhost:$GRAFANA_PORT/d/kinowo-worker-diag"
echo "  VictoriaMetrics:  http://localhost:$VM_PORT/vmui"
echo "  Teardown:         $0 --down"
[ "$fail" = 0 ] || exit 1

if [ "$KEEP" = 1 ]; then
  trap 'echo; echo "==> Ctrl-C: tearing down"; down; cleanup' INT TERM
  echo
  echo "==> --keep: holding in the foreground, Ctrl-C to tear down"
  while docker inspect -f '{{.State.Running}}' "$GRAFANA_NAME" 2>/dev/null | grep -q true; do
    sleep 5
  done
fi
