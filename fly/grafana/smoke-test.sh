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
EXPECTED_RULES=17

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

# The worker-throttle control gate must read a SMOOTHED balance, not the raw
# instantaneous gauge, and debounce with `for: 1m` — otherwise a single spurious
# low/zero scrape sample trips the backoff while the real balance sits well above
# 6k. Guards against a regression back to `instant` + `for: 0s`.
assert "worker-credit-low rule averages the balance over 2m and debounces with for:1m" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-worker-credit-low' and r.get('for')=='1m' and any('avg_over_time' in c['model'].get('expr','') for c in r['data']) for r in d)"

# The org-wide (un-app-scoped) rules must exclude the split-out `schowek` app, or
# its series would page these alerts. Each must carry an app!="schowek" matcher.
for uid in kinowo-cpu-steal-high kinowo-memory-pressure kinowo-http-5xx-high kinowo-http-p95-high; do
  assert "$uid excludes the schowek app" \
    "api/v1/provisioning/alert-rules" \
    "any(r['uid']=='$uid' and any('schowek' in c['model'].get('expr','') for c in r['data']) for r in d)"
done

assert "Telegram contact point provisioned" \
  "api/v1/provisioning/contact-points" \
  "any(c.get('name')=='Telegram' for c in d)"

# Pin the target chat to the "Kinowo Monitoring" group (-1003950886618) — the
# single channel for all alerts. Guards against a regression back to a personal
# DM or the old "Kinowo" channel.
assert "Telegram chatid is the Kinowo Monitoring group" \
  "api/v1/provisioning/contact-points" \
  "any(c.get('name')=='Telegram' and c.get('settings',{}).get('chatid')=='-1003950886618' for c in d)"

assert "overview dashboard provisioned" \
  "api/search?query=kinowo" \
  "any(x.get('uid')=='fly-overview' for x in d)"

# The per-type task-flow panels: enqueued (non-duplicate) by type queries the
# enqueued counter scoped to result="added" (dedup collapses excluded), and
# started by type splits the started counter. Guards the panels and their exprs
# against being dropped or pointed at the wrong metric/label.
assert "enqueued-by-type panel queries enqueued_total{result=added} by task_type" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='sum by (task_type) (rate(kinowo_worker_tasks_enqueued_total{result=\"added\"}[\$__rate_interval])) * 60' for p in d['dashboard']['panels'] for t in p.get('targets',[]))"

assert "started-by-type panel queries started_total by task_type" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='sum by (task_type) (rate(kinowo_worker_tasks_started_total[\$__rate_interval])) * 60' for p in d['dashboard']['panels'] for t in p.get('targets',[]))"

# The three by-type task-flow panels (enqueued → started → fully-worked) must sit
# next to each other in one row — same gridPos.y, three distinct x's — so a type's
# enqueue → claim → done funnel reads left-to-right at a glance. Guards against a
# panel drifting back to its own row.
assert "enqueued/started/fully-worked by-type panels share one row" \
  "api/dashboards/uid/fly-overview" \
  "(lambda ps: len(ps)==3 and len({p['gridPos']['y'] for p in ps})==1 and len({p['gridPos']['x'] for p in ps})==3)([p for p in d['dashboard']['panels'] if any(s in p.get('title','') for s in ['enqueued (non-duplicate) by type','started (claimed) by type','fully-worked throughput by type'])])"

echo "==> scanning Grafana logs for provisioning errors"
# Ignore the benign "no plugins provisioning dir" line — we ship none.
if docker logs "$NAME" 2>&1 | grep -Ei 'failed to provision|failure to map|failure parsing|provisioning\.datasources.*level=error'; then
  echo "FAIL: provisioning errors in log"; fail=1
else
  echo "PASS: no provisioning errors in log"
fi

[ "$fail" = 0 ] && echo "== SMOKE TEST PASSED ==" || { echo "== SMOKE TEST FAILED =="; exit 1; }
