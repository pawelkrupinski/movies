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
# The Dockerfile is multi-stage (a VictoriaMetrics builder stage, then the
# Grafana runtime stage). We want the FINAL stage — the Grafana image we
# actually ship — so take the last FROM, not the first.
IMG="$(grep '^FROM ' "$DIR/Dockerfile" | tail -1 | awk '{print $2}')"
NAME="kinowo-grafana-smoke"
PORT="${SMOKE_PORT:-3999}"
EXPECTED_RULES=21

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

# The direct-scrape datasource backed by the VictoriaMetrics sidecar (real-time
# kinowo_* / kinowo_worker_* series). Points at the in-machine VM on :8428.
assert "App Metrics (live) datasource provisioned at 127.0.0.1:8428" \
  "api/datasources" \
  "any(x.get('uid')=='app-metrics-live' and x.get('url')=='http://127.0.0.1:8428' for x in d)"

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

# The task-queue starvation alerts: the head-of-line age rule is the earliest
# signal of the sustained-throttle spiral (it would have paged ~2h before the
# 2026-06-23 credit floor). Guards both new rules' thresholds against drift.
assert "worker-task-age-high rule fires above 600s oldest waiting age" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-worker-task-age-high' and any(c['model']['conditions'][0]['evaluator']['params']==[600] for c in r['data'] if c['refId']=='C') for r in d)"

assert "worker-queue-backlog rule fires above 150 waiting tasks" \
  "api/v1/provisioning/alert-rules" \
  "any(r['uid']=='kinowo-worker-queue-backlog' and any(c['model']['conditions'][0]['evaluator']['params']==[150] for c in r['data'] if c['refId']=='C') for r in d)"

assert "Telegram contact point provisioned" \
  "api/v1/provisioning/contact-points" \
  "any(c.get('name')=='Telegram' for c in d)"

# Pin the target chat to the "Kinowo Monitoring" group (-1003950886618) — the
# single channel for all alerts. Guards against a regression back to a personal
# DM or the old "Kinowo" channel.
assert "Telegram chatid is the Kinowo Monitoring group" \
  "api/v1/provisioning/contact-points" \
  "any(c.get('name')=='Telegram' and c.get('settings',{}).get('chatid')=='-1003950886618' for c in d)"

# The WorkerThrottle route must re-POST a still-firing alert on a SHORT interval
# (5m), not the default-long one. The webhook flips an in-memory flag the worker
# loses on reboot; a long repeat_interval leaves a rebooted worker blind to a
# still-low credit balance for that whole interval. Guards against a regression
# back to the 12h value.
assert "WorkerThrottle route re-seeds the gate on a short (<=5m) repeat_interval" \
  "api/v1/provisioning/policies" \
  "any(r.get('receiver')=='WorkerThrottle' and r.get('repeat_interval')=='5m' for r in d.get('routes',[]))"

# Each worker app must route to the contact point whose webhook URL targets THAT
# app. ExternalThrottleGate reads only the top-level `status` and ignores labels,
# so a crossed pairing would throttle the wrong fleet silently. Checked through
# Grafana's own parsed config (two endpoints, hence not the `assert` helper) —
# the YAML-text equivalent lives in GrafanaWorkerThrottleCoverageSpec, which is
# what actually gates CI.
if curl -fsS "$base/api/v1/provisioning/policies" -o /tmp/kw-policies.json \
   && curl -fsS "$base/api/v1/provisioning/contact-points" -o /tmp/kw-contacts.json; then
  got=$(python3 - <<'PY'
import json
policies = json.load(open('/tmp/kw-policies.json'))
contacts = json.load(open('/tmp/kw-contacts.json'))
url_by_name = {c['name']: c.get('settings', {}).get('url', '') for c in contacts}
routes = [r for r in policies.get('routes', []) if r.get('receiver', '').startswith('WorkerThrottle')]
paired = [
    app in url_by_name.get(r['receiver'], '')
    for r in routes
    for m in r.get('object_matchers', [])
    if m[0] == 'app'
    for app in [m[2]]
]
print('PASS' if routes and paired and all(paired) else 'FAIL')
PY
) || got=FAIL
  echo "$got: every worker-throttle route pairs its app with that app's own webhook"
  [ "$got" = PASS ] || fail=1
else
  echo "FAIL: could not read policies/contact-points"; fail=1
fi
rm -f /tmp/kw-policies.json /tmp/kw-contacts.json

assert "overview dashboard provisioned" \
  "api/search?query=kinowo" \
  "any(x.get('uid')=='fly-overview' for x in d)"

# The per-type task-flow panels: enqueued (non-duplicate) by type queries the
# enqueued counter scoped to result="added" (dedup collapses excluded), and
# started by type splits the started counter. Guards the panels and their exprs
# against being dropped or pointed at the wrong metric/label.
assert "enqueued-by-type panel queries enqueued_total{result=added} by task_type, scoped to \$country" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='sum by (task_type) (rate(kinowo_worker_tasks_enqueued_total{country=~\"\$country\",result=\"added\"}[\$__rate_interval])) * 60' for p in d['dashboard']['panels'] for t in p.get('targets',[]))"

assert "started-by-type panel queries started_total by task_type, scoped to \$country" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='sum by (task_type) (rate(kinowo_worker_tasks_started_total{country=~\"\$country\"}[\$__rate_interval])) * 60' for p in d['dashboard']['panels'] for t in p.get('targets',[]))"

# The three by-type task-flow panels (enqueued → started → fully-worked) must sit
# next to each other in one row — same gridPos.y, three distinct x's — so a type's
# enqueue → claim → done funnel reads left-to-right at a glance. Guards against a
# panel drifting back to its own row.
assert "enqueued/started/fully-worked by-type panels share one row" \
  "api/dashboards/uid/fly-overview" \
  "(lambda ps: len(ps)==3 and len({p['gridPos']['y'] for p in ps})==1 and len({p['gridPos']['x'] for p in ps})==3)([p for p in d['dashboard']['panels'] if any(s in p.get('title','') for s in ['enqueued (non-duplicate) by type','started (claimed) by type','fully-worked throughput by type'])])"

# The worker JVM/host memory panel must carry the host-free-memory series (the
# whole point of the panel — how much RAM the machine has left) and be a Mixed
# panel so it can join the worker's own jvm_* exports with Fly host metrics.
# Fly host metrics carry no `country` label AND sit on a different datasource, so
# no on(app) data join can reach them — they are narrowed to the selected country
# by the derived $worker_host_app variable instead (see the templating assertions
# below). A regression here shows every country's worker on a one-country panel.
assert "worker memory panel scopes host free memory to \$country" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='fly_instance_memory_mem_available{app=~\"\$worker_host_app\"}' for p in d['dashboard']['panels'] if p.get('id')==43 for t in p.get('targets',[]))"

# The WEB app has the same panel, side by side with the worker's. It exists
# because the web JVM exports jvm_*/process_* too (services.metrics.WebJvmMetrics)
# — before that, web memory was answerable only from host RAM, which can't see
# the heap inside it. Its host line is scoped by the web mirror of that variable,
# which must be a DIFFERENT one: sharing it would plot the worker's host RAM here.
assert "web memory panel scopes host free memory to \$country" \
  "api/dashboards/uid/fly-overview" \
  "any(t.get('expr')=='fly_instance_memory_mem_available{app=~\"\$web_host_app\"}' for p in d['dashboard']['panels'] if p.get('id')==44 for t in p.get('targets',[]))"

# The web JVM series carry no country label (one web process per country), so
# they are country-scoped by intersecting with the country-labelled business
# gauge — the mirror of the worker panel's kinowo_worker_throttled join. Without
# this join the panel would plot every country's web heap at once.
assert "web memory panel scopes its jvm_* targets by an on(app) join to \$country" \
  "api/dashboards/uid/fly-overview" \
  "all('and on(app) kinowo_web_movies_served{country=~\"\$country\"}' in t['expr'] for p in d['dashboard']['panels'] if p.get('id')==44 for t in p.get('targets',[]) if t['expr'].startswith(('sum(jvm_','max(jvm_')))"

# The two memory panels must sit side by side in one row — worker left, web
# right — so one country's two JVMs are compared at a glance rather than by
# scrolling. Guards against a later edit reflowing one of them onto its own row.
assert "worker + web memory panels share one row, half width each" \
  "api/dashboards/uid/fly-overview" \
  "(lambda ps: len(ps)==2 and len({p['gridPos']['y'] for p in ps})==1 and {p['gridPos']['x'] for p in ps}=={0,12} and all(p['gridPos']['w']==12 for p in ps))([p for p in d['dashboard']['panels'] if p.get('id') in (43,44)])"

# \$country is the only variable anyone can PICK. The hand-selectable \$worker_app
# it replaced twice caused silent cross-country leakage — its value lived in the
# URL independently of the country, so a bookmarked link happily showed country=uk
# next to worker_app=kinowo-worker-de. Every target whose metric carries a country
# is therefore scoped by data (the `and on(app) kinowo_worker_throttled{country=~
# "$country"}` intersections), never by a variable.
#
# The two exceptions are DERIVED, not selectable: the memory panels' fly_instance_*
# host series live on fly-prometheus, which no join can reach, so $worker_host_app /
# $web_host_app resolve the country's app names from app-metrics-live. What makes
# them safer than the old variable is that their query is a function OF $country,
# so the value they DERIVE cannot disagree with it, and hide: 2 removes the
# dropdown that made pinning routine (Grafana writes $__all back to the URL, so
# bookmarks re-derive). Not a guarantee: a hand-written `&var-worker_host_app=...`
# still overrides — verified, and asserted as a known residual by
# fly/grafana/interpolation-probe.mjs, whose {{app}} legend is the visible tell.
# Both properties below are what keep it derived; a variable that stops
# referencing $country, or gains a dropdown, is the old leak coming back.
for uid in fly-overview kinowo-worker-diag; do
  assert "$uid: country is the only user-facing template variable" \
    "api/dashboards/uid/$uid" \
    "[v['name'] for v in d['dashboard']['templating']['list'] if v.get('hide')!=2]==['country']"
done

assert "fly-overview: the derived host-app variables are hidden and defined from \$country" \
  "api/dashboards/uid/fly-overview" \
  "(lambda vs: sorted(vs)==['web_host_app','worker_host_app'] and all(v.get('hide')==2 and 'country=~\"\$country\"' in v.get('definition','') and v['datasource']['uid']=='app-metrics-live' for v in d['dashboard']['templating']['list'] if v['name'] in vs))([v['name'] for v in d['dashboard']['templating']['list'] if v['name'].endswith('_host_app')])"

# Host series MUST carry {{app}} in the legend — the panels that stay fleet-wide
# (CPU, credit, throttle, steal, memory %) and the two country-scoped memory ones
# alike. Without it several apps render under one fixed display name and you get
# identical duplicate legend entries — indistinguishable from the cross-country
# leak this replaced, and on the memory panels the only way to SEE a bad scope.
assert "fleet-wide fly_instance_* series are labelled per app" \
  "api/dashboards/uid/fly-overview" \
  "all('{{app}}' in t.get('legendFormat','') for p in d['dashboard']['panels'] for t in p.get('targets',[]) if 'fly_instance_' in t.get('expr',''))"

assert "worker-diag credit-balance series is labelled per app" \
  "api/dashboards/uid/kinowo-worker-diag" \
  "all('{{app}}' in t.get('legendFormat','') for p in d['dashboard']['panels'] for t in p.get('targets',[]) if 'fly_instance_' in t.get('expr',''))"

assert "worker diagnostics dashboard provisioned" \
  "api/search?query=kinowo" \
  "any(x.get('uid')=='kinowo-worker-diag' for x in d)"

echo "==> scanning Grafana logs for provisioning errors"
# Ignore the benign "no plugins provisioning dir" line — we ship none.
if docker logs "$NAME" 2>&1 | grep -Ei 'failed to provision|failure to map|failure parsing|provisioning\.datasources.*level=error'; then
  echo "FAIL: provisioning errors in log"; fail=1
else
  echo "PASS: no provisioning errors in log"
fi

[ "$fail" = 0 ] && echo "== SMOKE TEST PASSED ==" || { echo "== SMOKE TEST FAILED =="; exit 1; }
