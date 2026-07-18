#!/usr/bin/env bash
# Boots the real Grafana image with our real provisioning AND a metrics backend
# that actually contains data — so template-variable interpolation can be
# verified instead of assumed.
#
# The gap this fills: `smoke-test.sh` proves the provisioning YAML/JSON parses,
# but its Grafana has no metrics backend at all. `label_values(...)` returns
# nothing there, so `$country` never populates and no panel query is ever
# interpolated with a real value. Every scoping bug that lives in the
# interpolation (the "All" → `.*` cross-country leak, an `and on(app)` join
# against the wrong country-labelled metric) is invisible to that test.
#
# Here a single-node VictoriaMetrics is seeded (see seed-metrics.py) with three
# worker apps and three web apps across pl/uk/de, and the `app-metrics-live`
# datasource is pointed at it. The checked-in datasource YAML is NOT edited:
# provisioning is copied to a temp dir and the URL rewritten there.
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

echo "==> seeding ${SEED_HOURS}h of samples at ${SEED_STEP}s steps (3 worker + 3 web apps: pl/uk/de)"
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
assert_labels app "kinowo,kinowo-worker,kinowo-worker-de,kinowo-worker-uk,showtimes-de,showtimes-uk"

# Run a panel's real PromQL and report which apps came back. `$country` is
# interpolated by hand here — that IS the thing under test.
apps_returned() { # promql
  curl -fsS --get --data-urlencode "query=$1" "$proxy/query" \
    | python3 -c "
import sys, json
r = json.load(sys.stdin)['data']['result']
print(','.join(sorted({s['metric'].get('app', '?') for s in r})) or 'NO DATA')"
}

assert_scoped_to() { # what  promql  expected-csv
  local got; got=$(apps_returned "$2")
  if [ "$got" = "$3" ]; then
    echo "PASS: $1 → [$got]"
  else
    echo "FAIL: $1 → [$got], expected [$3]"
    fail=1
  fi
}

# The side-by-side memory panels (43 worker / 44 web). Neither app's jvm_*
# carries a country label, so BOTH scope by intersecting with a metric that does
# — `and on(app) kinowo_worker_throttled` for the worker, the mirrored
# `and on(app) kinowo_web_movies_served` for web. That join is the whole scoping
# mechanism, and no template variable is involved (a second variable has twice
# caused silent cross-country leakage). Run each panel's real expr per country
# and assert it comes back with that country's app and nothing else — including
# that the two panels never land on the same app.
for c in pl uk de; do
  worker_expected="kinowo-worker"; [ "$c" = pl ] || worker_expected="kinowo-worker-$c"
  web_expected="kinowo";           [ "$c" = pl ] || web_expected="showtimes-$c"
  assert_scoped_to "worker heap, country=$c" \
    "jvm_memory_used_bytes{area=\"heap\"} and on(app) kinowo_worker_throttled{country=~\"$c\"}" \
    "$worker_expected"
  assert_scoped_to "web heap, country=$c" \
    "jvm_memory_used_bytes{area=\"heap\"} and on(app) kinowo_web_movies_served{country=~\"$c\"}" \
    "$web_expected"
  if [ "$worker_expected" = "$web_expected" ]; then
    echo "FAIL: the worker and web memory panels both resolve to [$worker_expected] for country=$c"
    fail=1
  fi
done

# The join is only as good as its right-hand side: if the web app ever stops
# exporting the country-labelled gauge, the web panel silently empties rather
# than showing the wrong country. Assert the per-country pools the panel plots.
for c in pl uk de; do
  web_expected="kinowo"; [ "$c" = pl ] || web_expected="showtimes-$c"
  assert_scoped_to "web G1 old gen, country=$c" \
    "jvm_memory_pool_used_bytes{pool=\"G1 Old Gen\"} and on(app) kinowo_web_movies_served{country=~\"$c\"}" \
    "$web_expected"
done

# The memory panels' HOST line (fly_instance_memory_mem_available) can't be
# scoped by a join: it lives on fly-prometheus, a datasource with no country
# label and no shared series to intersect with. It is narrowed by the derived
# $worker_host_app / $web_host_app variables instead, whose options come from
# THIS datasource. Grafana resolves those with label_values, whose wire form is
# a /label/app/values call filtered by the variable's own matcher — so running
# that call per country is running the variable definition itself. If a derived
# variable ever returned another country's app, the host line would contradict
# every other series on its panel, which is precisely the old $worker_app bug.
label_values_app() { # match-selector
  curl -fsS --get --data-urlencode "match[]=$1" "$proxy/label/app/values" \
    | python3 -c "import sys,json;print(','.join(sorted(json.load(sys.stdin).get('data') or [])) or 'NO DATA')"
}

assert_variable_resolves_to() { # what  match-selector  expected-csv
  local got; got=$(label_values_app "$2")
  if [ "$got" = "$3" ]; then
    echo "PASS: $1 → [$got]"
  else
    echo "FAIL: $1 → [$got], expected [$3]"
    fail=1
  fi
}

for c in pl uk de; do
  worker_expected="kinowo-worker"; [ "$c" = pl ] || worker_expected="kinowo-worker-$c"
  web_expected="kinowo";           [ "$c" = pl ] || web_expected="showtimes-$c"
  assert_variable_resolves_to "\$worker_host_app, country=$c" \
    "kinowo_worker_throttled{country=~\"$c\"}" "$worker_expected"
  assert_variable_resolves_to "\$web_host_app, country=$c" \
    "kinowo_web_movies_served{country=~\"$c\"}" "$web_expected"
done

# Multi-select is the case a per-country regex gets wrong most quietly: $country
# interpolates to `pl|uk`, and a variable that mishandled it would silently drop
# one country's host line rather than error.
assert_variable_resolves_to "\$worker_host_app, country=pl|uk (multi-select)" \
  "kinowo_worker_throttled{country=~\"pl|uk\"}" "kinowo-worker,kinowo-worker-uk"
assert_variable_resolves_to "\$web_host_app, country=pl|uk (multi-select)" \
  "kinowo_web_movies_served{country=~\"pl|uk\"}" "kinowo,showtimes-uk"

# The asymmetry the dashboards depend on: jvm_* has no country label — on
# EITHER app — so the `and on(app)` join is the only thing scoping those panels.
# If this ever returns a country the seed (and the assumption) is wrong.
got=$(curl -fsS --get --data-urlencode 'match[]=jvm_memory_max_bytes' \
  "$proxy/label/country/values" \
  | python3 -c "import sys,json;print(','.join(sorted(json.load(sys.stdin).get('data') or [])))")
if [ -z "$got" ]; then
  echo "PASS: jvm_memory_max_bytes carries no country label (so the on(app) join must scope it)"
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
