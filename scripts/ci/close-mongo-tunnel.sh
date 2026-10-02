#!/usr/bin/env bash
#
# Close what wait-for-mongo-tunnel.sh opened, and PROVE it is closed.
#
# WHY IT EXISTS. The corpus used to be recorded in a job of its own, which ended a few
# seconds after the read — the runner died, and the tunnel, the ssh key on disk and the
# read credential in $GITHUB_ENV died with it. Now the read is the first thing a country's
# enrichment leg does, and that job then runs project code for up to six hours with a
# write-scoped token and the TMDB/OMDb keys. Left open, the route to prod Mongo would be
# reachable from every one of those steps. Closing it right after the read keeps the
# window exactly as wide as it was: one step.
#
# What it does, in the order that keeps anything from coming back:
#   1. kills the supervisor FIRST — it restarts a listener that exits, so killing the
#      listener first would only race a respawn;
#   2. kills the local listener on the port, and every ssh relay still using the key;
#   3. removes the key, the known_hosts line and the generated scripts;
#   4. blanks KINOWO_CONVERGENCE_SCRAPES_URI in $GITHUB_ENV — the configuration reads an
#      empty value as absent, so no later step can even try the dead address;
#   5. fails unless nothing is listening on the port any more.
#
# Usage:  scripts/ci/close-mongo-tunnel.sh [local-port]   (default 27018, as the opener)
set -uo pipefail

PORT="${1:-27018}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/mongo-tunnel-paths.sh"

pkill -f "$TUNNEL_SUPERVISOR" 2>/dev/null || true
pkill -f "TCP4-LISTEN:$PORT" 2>/dev/null || true
pkill -f "$TUNNEL_KEY" 2>/dev/null || true
rm -f "$TUNNEL_KEY" "$TUNNEL_KNOWN_HOSTS" "$TUNNEL_RELAY" "$TUNNEL_SUPERVISOR"

if [ -n "${GITHUB_ENV:-}" ]; then
  echo "KINOWO_CONVERGENCE_SCRAPES_URI=" >> "$GITHUB_ENV"
fi

listening() {
  python3 -c 'import socket, sys
try:
    socket.create_connection(("127.0.0.1", int(sys.argv[1])), timeout=1).close()
except OSError:
    sys.exit(1)' "$PORT"
}

# A killed process can hold its socket for a moment; give it a few seconds, not forever.
for _ in 1 2 3 4 5; do
  listening || { echo "[tunnel] closed: nothing listens on :$PORT, key and credential gone"; exit 0; }
  sleep 1
done
echo "[tunnel] something still listens on :$PORT after closing the tunnel" >&2
exit 1
