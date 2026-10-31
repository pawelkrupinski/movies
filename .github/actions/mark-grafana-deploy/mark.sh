#!/usr/bin/env bash
# The deploy marker the action posts: inputs in GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN, TIER, SHA, MSG (see action.yml).
# Runs as the runner runs a `shell: bash` step: -e and pipefail.
set -eo pipefail
# SHADOW (scripts/ci/ci-writes.sh): a deploy marker only from the run that deploys.
. "$(dirname "${BASH_SOURCE[0]}")/../../../scripts/ci/ci-writes.sh" || { echo "::error::$0: scripts/ci/ci-writes.sh is missing beside it -- its shadow gate cannot be read" >&2; exit 2; }
ci_writes || { ci_writes_skipped "the Grafana deploy marker"; exit 0; }
if [ -z "${GRAFANA_URL:-}" ] || [ -z "${GRAFANA_ANNOTATION_TOKEN:-}" ]; then
    echo "Grafana annotation secrets unset — skipping deploy marker."
    exit 0
fi
short="${SHA:0:7}"
# The first line by expansion, not `printf | head -1`: head leaves after one line, and a message bigger than the pipe's
# buffer killed printf with SIGPIPE, which pipefail made this step's exit status (141, Main #2767).
subject="${MSG%%$'\n'*}"
now="$(( $(date +%s) * 1000 ))"
body="$(jq -n --argjson time "$now" \
    --arg text "deploy ${TIER} ${short} — ${subject} (${SHA})" \
    --arg tier "$TIER" \
    '{time: $time, tags: ["deploy", "main", $tier], text: $text}')"
# NEVER FAIL THE CALLING JOB OVER A MARKER. Also: check the status code
# explicitly rather than `curl -f`, which only trips on HTTP >= 400 — a
# 302 (Grafana's oauth2-proxy front door redirecting to a login page,
# or a stale host) reads as success under `-f` and silently posts
# nothing. See the 2026-08-29 kinowo/Grafana-host incident this guarded
# against, and the 2026-09-15 discovery that it was masking the SSO
# redirect too.
code="$(curl -sS --max-time 20 -o /tmp/grafana-annotation-response.json -w '%{http_code}' \
    -X POST "${GRAFANA_URL%/}/api/annotations" \
    -H "Authorization: Bearer ${GRAFANA_ANNOTATION_TOKEN}" \
    -H "Content-Type: application/json" \
    -d "$body" || echo "000")"
if [ "$code" = "200" ]; then
    echo "Posted deploy marker for ${TIER} ${short}."
else
    echo "::warning title=Deploy marker not posted::Grafana at ${GRAFANA_URL%/} returned HTTP ${code} for the ${TIER} ${short} annotation (expected 200)."
    cat /tmp/grafana-annotation-response.json 2>/dev/null || true
fi
