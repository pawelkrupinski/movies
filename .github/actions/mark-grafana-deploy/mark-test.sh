#!/usr/bin/env bash
# mark.sh against a stub `curl` that answers $STUB_CODE and records what it was sent.
# Run: bash .github/actions/mark-grafana-deploy/mark-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m mark-grafana-deploy/mark.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cat > "$work/curl" <<'STUB'
#!/usr/bin/env bash
while [ $# -gt 0 ]; do [ "$1" = -d ] && printf '%s' "$2" > "$(dirname "$0")/sent"; shift; done
printf '%s' "$STUB_CODE"
STUB
chmod +x "$work/curl"

mark() {  # mark <http code> <commit message>
  rm -f "$work/sent"
  STUB_CODE="$1" MSG="$2" GRAFANA_URL=https://grafana.test GRAFANA_ANNOTATION_TOKEN=t TIER=worker SHA=0123456789abcdef \
    PATH="$work:$PATH" bash "$REPO_ROOT/.github/actions/mark-grafana-deploy/mark.sh" > /dev/null 2>&1
  echo $?
}
subject() { jq -r .text "$work/sent" 2>/dev/null; }

check "a marker is posted" "0" "$(mark 200 $'Fix the thing\n\nwhy it broke')"
check "...with the message's first line" "deploy worker 0123456 — Fix the thing (0123456789abcdef)" "$(subject)"
check "a rejected marker never fails the job" "0" "$(mark 302 'Fix the thing')"
# Main #2767: a 95 KB commit message. `printf "$MSG" | head -1` under pipefail died of SIGPIPE (141) once the
# message outgrew the pipe's buffer and head had already gone, and failed publish-worker over a marker.
long="$(printf 'Long subject\n'; head -c 300000 /dev/zero | tr '\0' x)"
check "a commit message far past a pipe's buffer never fails the job" "0" "$(mark 200 "$long")"
check "...and still marks its first line" "deploy worker 0123456 — Long subject (0123456789abcdef)" "$(subject)"
check "unset secrets skip the marker" "0" "$(STUB_CODE=200 MSG=x GRAFANA_URL='' GRAFANA_ANNOTATION_TOKEN='' TIER=w SHA=s PATH="$work:$PATH" bash "$REPO_ROOT/.github/actions/mark-grafana-deploy/mark.sh" >/dev/null 2>&1; echo $?)"

spec_summary
