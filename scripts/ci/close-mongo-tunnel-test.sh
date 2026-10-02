#!/usr/bin/env bash
# close-mongo-tunnel.sh against stand-ins for what wait-for-mongo-tunnel.sh leaves running:
# a supervisor, a listener on the port, an ssh relay holding the key, and the files.
# Run: bash scripts/ci/close-mongo-tunnel-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m close-mongo-tunnel.sh\n'

work="$(mktemp -d)"
export RUNNER_TEMP="$work"
. "$REPO_ROOT/scripts/ci/mongo-tunnel-paths.sh"
export GITHUB_ENV="$work/github-env"
trap 'pkill -f "$work" 2>/dev/null; rm -rf "$work"' EXIT

port=$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')

# A listener whose command line carries socat's `TCP4-LISTEN:<port>`, as the real one does.
listen_code='import socket, sys, time
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", int(sys.argv[1]))); s.listen()
time.sleep(300)'

open_tunnel() {
  printf 'key\n' > "$TUNNEL_KEY"; printf 'host\n' > "$TUNNEL_KNOWN_HOSTS"
  printf '#!/usr/bin/env bash\nexec sleep 300\n' > "$TUNNEL_RELAY"
  printf '#!/usr/bin/env bash\nwhile true; do sleep 1; done\n' > "$TUNNEL_SUPERVISOR"
  chmod +x "$TUNNEL_RELAY" "$TUNNEL_SUPERVISOR"
  "$TUNNEL_SUPERVISOR" & supervisor=$!
  python3 -c "$listen_code" "$port" "TCP4-LISTEN:$port,bind=127.0.0.1" & listener=$!
  python3 -c 'import time; time.sleep(300)' ssh -i "$TUNNEL_KEY" & relay=$!
  for _ in $(seq 1 50); do python3 -c 'import socket,sys; socket.create_connection(("127.0.0.1", int(sys.argv[1])), 1)' "$port" 2>/dev/null && break; sleep 0.1; done
  echo "KINOWO_CONVERGENCE_SCRAPES_URI=mongodb://user:secret@127.0.0.1:$port/" >> "$GITHUB_ENV"
}
alive() { kill -0 "$1" 2>/dev/null && echo alive || echo gone; }

open_tunnel
bash "$REPO_ROOT/scripts/ci/close-mongo-tunnel.sh" "$port" > "$work/out" 2>&1
check "closes an open tunnel and succeeds" "0" "$?"
sleep 0.5
check "...the supervisor is gone, so nothing respawns the listener" "gone" "$(alive "$supervisor")"
check "...the listener is gone" "gone" "$(alive "$listener")"
check "...the ssh relay holding the key is gone" "gone" "$(alive "$relay")"
check "...the key, host key and scripts are deleted" "" "$(ls "$TUNNEL_KEY" "$TUNNEL_KNOWN_HOSTS" "$TUNNEL_RELAY" "$TUNNEL_SUPERVISOR" 2>/dev/null)"
check "...and later steps see the credential blanked" "KINOWO_CONVERGENCE_SCRAPES_URI=" "$(tail -n 1 "$GITHUB_ENV")"

# Something it does not recognise still holding the port: the proof must fail, not shrug.
python3 -c "$listen_code" "$port" & stranger=$!
for _ in $(seq 1 50); do python3 -c 'import socket,sys; socket.create_connection(("127.0.0.1", int(sys.argv[1])), 1)' "$port" 2>/dev/null && break; sleep 0.1; done
bash "$REPO_ROOT/scripts/ci/close-mongo-tunnel.sh" "$port" > "$work/out" 2>&1
check "fails when the port still answers after closing" "1" "$?"
kill "$stranger" 2>/dev/null

bash "$REPO_ROOT/scripts/ci/close-mongo-tunnel.sh" "$port" > "$work/out" 2>&1
check "closing a tunnel that was never opened is a no-op success" "0" "$?"

spec_summary
