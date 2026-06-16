#!/usr/bin/env bash
# Boots the real Grafana image with our provisioning mounted and asserts that
# the datasource, all alert rules, the Telegram contact point and the overview
# dashboard load cleanly — i.e. the provisioning YAML/JSON is valid and Grafana
# accepts it. This is the test layer for this change: break any provisioning
# file and this fails; valid config passes. No network to Fly is needed (dummy
# token), because provisioning loads the objects without testing connectivity.
#
# Usage: fly/grafana/smoke-test.sh        (needs Docker)
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMG="$(grep -m1 '^FROM ' "$DIR/Dockerfile" | awk '{print $2}')"
NAME="kinowo-grafana-smoke"
PORT="${SMOKE_PORT:-3999}"
EXPECTED_RULES=13

# Mount a FRESH copy, not the source tree directly: Docker Desktop's macOS
# bind-mount cache can keep serving a file's pre-edit bytes after you edit it,
# which silently tests stale provisioning. Copying to a new path sidesteps that
# and is harmless on Linux/CI.
WORK="$(mktemp -d)"
cp -R "$DIR/provisioning/." "$WORK/"

cleanup() { docker rm -f "$NAME" >/dev/null 2>&1 || true; rm -rf "$WORK"; }
trap cleanup EXIT
docker rm -f "$NAME" >/dev/null 2>&1 || true

echo "==> booting $IMG with provisioning on :$PORT"
docker run -d --name "$NAME" -p "$PORT:3000" \
  -e GF_SECURITY_ADMIN_USER=admin \
  -e GF_SECURITY_ADMIN_PASSWORD=admin \
  -e GF_AUTH_ANONYMOUS_ENABLED=false \
  -e FLY_PROMETHEUS_TOKEN=dummy-token \
  -e TELEGRAM_BOT_TOKEN=dummy:bottoken \
  -v "$WORK:/etc/grafana/provisioning:ro" \
  "$IMG" >/dev/null

echo "==> waiting for /api/health"
for _ in $(seq 1 60); do
  if curl -fsS "http://localhost:$PORT/api/health" >/dev/null 2>&1; then ok=1; break; fi
  sleep 2
done
[ "${ok:-0}" = 1 ] || { echo "FAIL: Grafana never became healthy"; docker logs "$NAME" | tail -40; exit 1; }

base="http://admin:admin@localhost:$PORT"
fail=0
assert() { # description  url  python-bool-expr (data bound as `d`)
  local got
  got=$(curl -fsS "$base/$2" | python3 -c "import sys,json;d=json.load(sys.stdin);print('PASS' if ($3) else 'FAIL')") \
    || { echo "FAIL: $1 (request error on /$2)"; fail=1; return; }
  echo "$got: $1"
  [ "$got" = PASS ] || fail=1
}

assert "Fly Prometheus datasource provisioned" \
  "api/datasources" \
  "any(x.get('uid')=='fly-prometheus' for x in d)"

assert "all $EXPECTED_RULES alert rules provisioned" \
  "api/v1/provisioning/alert-rules" \
  "len(d)==$EXPECTED_RULES"

assert "serving-down rule alarms on NoData" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-serving-down' and r.get('noDataState')=='Alerting' for r in d)"

assert "disk-space-low rule provisioned with 0.8 threshold" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-disk-space-low' and any(c['model']['conditions'][0]['evaluator']['params']==[0.8] for c in r['data'] if c['refId']=='C') for r in d)"

assert "mongo-down rule provisioned and alarms on NoData" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-mongo-down' and r.get('noDataState')=='Alerting' for r in d)"

assert "disk-fill-projected rule uses predict_linear" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-disk-fill-projected' and any('predict_linear' in c['model'].get('expr','') for c in r['data']) for r in d)"

assert "memory-pressure-psi rule provisioned" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-memory-pressure-psi' and any('memory_pressure_some' in c['model'].get('expr','') for c in r['data']) for r in d)"

assert "residential-proxy-failing rule provisioned with 0.5 ratio threshold" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-residential-proxy-failing' and any(c['model']['conditions'][0]['evaluator']['params']==[0.5] for c in r['data'] if c['refId']=='C') for r in d)"

assert "Telegram contact point provisioned" \
  "api/v1/provisioning/contact-points" \
  "any(c.get('name')=='Telegram' for c in d)"

assert "overview dashboard provisioned" \
  "api/search?query=kinowo" \
  "any(x.get('uid')=='fly-overview' for x in d)"

echo "==> scanning Grafana logs for provisioning errors"
# Ignore the benign "no plugins provisioning dir" line — we ship none.
if docker logs "$NAME" 2>&1 | grep -Ei 'failed to provision|failure to map|failure parsing|provisioning\.datasources.*level=error'; then
  echo "FAIL: provisioning errors in log"; fail=1
else
  echo "PASS: no provisioning errors in log"
fi

[ "$fail" = 0 ] && echo "== SMOKE TEST PASSED ==" || { echo "== SMOKE TEST FAILED =="; exit 1; }
