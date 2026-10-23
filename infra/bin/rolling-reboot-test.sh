#!/usr/bin/env bash
# bin/rolling-reboot reaches fleetkit's tool with profile movies over this checkout, and without the
# dashboard's node_modules refuses with exit 2 rather than doing anything.
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="$(mktemp -d)"; trap 'rm -rf "${WORK}"' EXIT
fails=0
mkdir -p "${WORK}/infra/bin" "${WORK}/infra/version-dashboard/node_modules/fleetkit/rolling-reboot/bin"
cp "${HERE}/rolling-reboot" "${WORK}/infra/bin/"
# Without the package: exit 2, said.
out="$("${WORK}/infra/bin/rolling-reboot" --dry-run 2>&1)"; status=$?
if [ "${status}" -eq 2 ] && printf '%s' "${out}" | grep -q "npm ci"; then echo "ok    missing package: exit 2"; else echo "FAIL  missing package: exit ${status}: ${out}"; fails=$((fails + 1)); fi
# With a stand-in for the package: what it is called with.
cat > "${WORK}/infra/version-dashboard/node_modules/fleetkit/rolling-reboot/bin/rolling-reboot" <<'PY'
import sys
print("ARGS " + " ".join(sys.argv[1:]))
PY
out="$("${WORK}/infra/bin/rolling-reboot" --dry-run --only mongo-1 2>&1)"; status=$?
want="ARGS --profile movies --repo $(cd "${WORK}/infra" && pwd) --dry-run --only mongo-1"
if [ "${status}" -eq 0 ] && [ "${out}" = "${want}" ]; then echo "ok    runs profile movies over this checkout"; else echo "FAIL  got: ${out} (exit ${status}), want: ${want}"; fails=$((fails + 1)); fi
# The dashboard starts a run only if the tool says it stops between hosts (fleetkit jobs.ts).
for marker in "stop requested" "signal.SIGTERM"; do
    if grep -qF "${marker}" "${HERE}/rolling-reboot"; then echo "ok    says '${marker}'"; else echo "FAIL  no '${marker}'"; fails=$((fails + 1)); fi
done
[ "${fails}" -eq 0 ]
