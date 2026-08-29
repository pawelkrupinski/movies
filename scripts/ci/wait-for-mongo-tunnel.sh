#!/usr/bin/env bash
#
# Open the CI end of the route to prod Mongo and DO NOT RETURN until bytes actually
# flow through it — then hold it open for the rest of the job, and export the URI
# the recorder should dial.
#
# WHAT CHANGED AND WHY. This used to run `flyctl proxy 27017 --app kinowo-mongo`.
# Prod Mongo moved off Fly onto the Hetzner host mongo-1 and that app is STOPPED,
# so every line below that mentioned it was aimed at a machine that no longer
# answers — the same trap scripts/local-mirror/prod-tunnel.sh documents for the
# laptop side of the move.
#
# WHY NOT `ssh -L`, WHICH IS WHAT THE LAPTOP USES. A forward is the client naming a
# host and port for the server to allow; the key that does it can be pointed at
# anything the server tolerates. This runner is GitHub's, not ours, so it gets less
# than that: its key is pinned to a FORCED COMMAND on mongo-1
# (infra/nix/modules/roles/mongo-ci-read.nix) that relays one connection to an
# address baked into the host's own closure. The runner cannot ask for a different
# destination, cannot get a shell, and cannot forward anything.
#
# The shape that falls out of it: one ssh session PER TCP CONNECTION, so the local
# listener is a `socat ... fork` that spawns a fresh relay per connect rather than a
# single long-lived tunnel process.
#
# WHAT SURVIVED THE MOVE, because it was never about flyctl:
#
#   * THE READINESS PROBE SPEAKS MONGO. `nc -z` succeeds the instant something binds
#     the local port, which happens well before there is a usable upstream — three
#     convergence legs once spent eight minutes failing against a tunnel that had
#     reported itself healthy at second three:
#
#       MongoTimeoutException: … state=CONNECTING,
#       exception={MongoSocketReadTimeoutException: Timeout while receiving message}
#
#     mongo-ping.py sends the smallest legal handshake and requires a reply, which
#     is the one check that distinguishes "port is bound" from "the database is
#     reachable". Now it proves rather more: a reply has crossed local socat → ssh →
#     the forced command → mongod.
#   * IT IS SUPERVISED, NOT STARTED-AND-TRUSTED. The old proxy died mid-job and left
#     everything after it reading `Connection refused`. A listener can die the same
#     way, and "it was alive when the step finished" says nothing about the sbt run
#     several steps later.
#
# Usage:  scripts/ci/wait-for-mongo-tunnel.sh [local-port]
#           local-port  default 27018 (27017 is a leg's own throwaway Mongo)
#
# Environment:
#   MONGO_CI_SSH_KEY       required — the PRIVATE half of the key pinned to the
#                          forced command on mongo-1
#   MONGO_CI_SSH_HOST_KEY  required — mongo-1's host key, one known_hosts line
#   MONGO_CI_SSH_TARGET    default mongociread@2.28.56.140 (hosts/mongo-1's
#                          `fleet.publicAddress`, pinned by terraform)
#
# On success it appends KINOWO_CONVERGENCE_SCRAPES_URI to $GITHUB_ENV, so no step
# has to spell out a production connection string.
set -euo pipefail

PORT="${1:-27018}"
TARGET="${MONGO_CI_SSH_TARGET:-mongociread@2.28.56.140}"
ATTEMPTS="${TUNNEL_ATTEMPTS:-3}"
PROBE_TRIES="${TUNNEL_PROBE_TRIES:-30}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROBE="$HERE/mongo-ping.py"
WORK="${RUNNER_TEMP:-${TMPDIR:-/tmp}}"

[ -f "$PROBE" ] || { echo "[tunnel] missing probe script at $PROBE" >&2; exit 1; }
[ -n "${MONGO_CI_SSH_KEY:-}" ] || { echo "[tunnel] MONGO_CI_SSH_KEY is not set — see mongo-ci-read.nix" >&2; exit 1; }

# THE HOST KEY IS REQUIRED, AND `accept-new` IS NOT AN ACCEPTABLE FALLBACK HERE. A
# runner is a fresh machine every night with an empty known_hosts, so trust-on-first-
# use is trust-on-every-use — no verification at all — and what a wrong host would be
# handed is the production read credential this script asks for two steps below.
# Obtain the line with `ssh-keyscan -t ed25519 2.28.56.140`, from somewhere you trust.
if [ -z "${MONGO_CI_SSH_HOST_KEY:-}" ]; then
  echo "[tunnel] MONGO_CI_SSH_HOST_KEY is not set. Refusing to connect unverified:" >&2
  echo "[tunnel]   ssh-keyscan -t ed25519 2.28.56.140   → the MONGO_CI_SSH_HOST_KEY secret" >&2
  exit 1
fi

KEY="$WORK/mongo-ci-read.key"
KNOWN="$WORK/mongo-ci-read.known_hosts"
umask 077
printf '%s\n' "$MONGO_CI_SSH_KEY" > "$KEY"
printf '%s\n' "$MONGO_CI_SSH_HOST_KEY" > "$KNOWN"
chmod 600 "$KEY" "$KNOWN"

# BatchMode: nothing on a runner can answer a passphrase prompt, so hang-forever must
# become fail-now. IdentitiesOnly: the runner's agent may hold other keys and offering
# them first is how a key-count limit turns into "Too many authentication failures".
SSH_OPTIONS=(
  -T
  -i "$KEY"
  -o IdentitiesOnly=yes
  -o BatchMode=yes
  -o StrictHostKeyChecking=yes
  -o UserKnownHostsFile="$KNOWN"
  -o ConnectTimeout=10
  -o ServerAliveInterval=15
  -o ServerAliveCountMax=3
  -o LogLevel=ERROR
)

# THE CREDENTIAL COMES FROM THE HOST, not from a repository secret — see the endpoint's
# own comment for that argument. Fetched once, here, so a failure to authorise shows up
# in the step that is about the connection rather than as an sbt stack trace.
echo "[tunnel] asking $TARGET for the read-only credential"
USERINFO="$(ssh "${SSH_OPTIONS[@]}" "$TARGET" credential)"
[ -n "$USERINFO" ] || { echo "[tunnel] the endpoint returned no credential" >&2; exit 1; }

# Mask BOTH halves before the URI exists. GitHub only redacts what it has been told
# about, and a Mongo driver that logs its connection string on a retry would otherwise
# print a production password into a public log.
echo "::add-mask::${USERINFO#*:}"
echo "::add-mask::$USERINFO"

# directConnection=true IS LOAD-BEARING, and its absence does not look like a
# configuration problem. mongod on mongo-1 is a single-node replica set whose one
# member advertises itself as 10.20.0.10:27017 (a private Hetzner subnet); without
# this the driver takes the seed only as a discovery hint, replaces it with the
# address the set advertises, and then spends every query in server selection against
# a host the runner cannot route to.
#
# maxPoolSize bounds how many ssh sessions can be in flight at once, because each
# pooled connection is one. The driver's default of 100 would open a hundred
# concurrent handshakes against sshd's MaxStartups and get most of them dropped.
#
# The timeouts are NOT set here: tools.TunnelTunedUri adds them and explicitly leaves
# anything already present alone, so stating them twice would only create a second
# place to change them.
URI="mongodb://${USERINFO}@127.0.0.1:${PORT}/?authSource=admin&directConnection=true&maxPoolSize=8"

# socat is what turns "one ssh session per connection" into a local port. It is on the
# GitHub runner images, but installed rather than assumed — a missing tool should cost
# twenty seconds, not a red nightly.
if ! command -v socat >/dev/null 2>&1; then
  echo "[tunnel] socat missing — installing"
  sudo apt-get update -qq && sudo apt-get install -y -qq socat
fi

LOG="${TUNNEL_LOG:-/tmp/mongo-tunnel.log}"
RELAY="$WORK/mongo-relay.sh"
SUPERVISOR="$WORK/mongo-tunnel-supervisor.sh"

# ONE RELAY PER CONNECTION. `connect` is the verb; the endpoint decides what it means,
# and this side deliberately cannot say more than that.
{
  echo '#!/usr/bin/env bash'
  echo 'exec ssh \'
  printf '  %q \\\n' "${SSH_OPTIONS[@]}"
  printf '  %q connect\n' "$TARGET"
} > "$RELAY"
chmod +x "$RELAY"

# Restarts the listener when it EXITS *and* when it merely stops answering. The second
# kind is the one that cost a run before the move: the process stayed alive, held its
# port, and served nothing, so an exit-only supervisor sat happily beside a dead tunnel
# while every query timed out. The liveness check is the same handshake the readiness
# probe uses, so "supervised" and "ready" mean exactly the same thing.
cat > "$SUPERVISOR" <<SUPERVISE
#!/usr/bin/env bash
while true; do
  socat "TCP4-LISTEN:$PORT,bind=127.0.0.1,reuseaddr,fork" "EXEC:$RELAY" >>"$LOG" 2>&1 &
  listener=\$!

  # Give it a moment to bind, then watch. A first failure is tolerated — the first
  # connection pays for an ssh handshake — but two consecutive silent checks mean it is
  # wedged rather than starting.
  sleep 5
  misses=0
  while kill -0 "\$listener" 2>/dev/null; do
    if python3 "$PROBE" "$PORT" 2>/dev/null; then
      misses=0
    else
      misses=\$((misses + 1))
      if [ "\$misses" -ge 2 ]; then
        echo "[tunnel] listener alive but not answering — killing it" >>"$LOG"
        kill "\$listener" 2>/dev/null || true
        break
      fi
    fi
    sleep 10
  done

  wait "\$listener" 2>/dev/null || true
  echo "[tunnel] listener gone — restarting" >>"$LOG"
  sleep 2
done
SUPERVISE
chmod +x "$SUPERVISOR"

# `setsid` + `nohup`, not a plain `&`: the supervisor has to outlive BOTH this script
# and the step's shell, because the reads it exists for happen several steps later. A
# backgrounded child stays in the shell's process group and dies with it — which is
# exactly what happened when this was tested, leaving the probe passing and the tunnel
# gone moments afterwards.
setsid nohup "$SUPERVISOR" >/dev/null 2>&1 < /dev/null &
echo "[tunnel] supervisor detached, log $LOG"

for _ in $(seq 1 $((ATTEMPTS * PROBE_TRIES))); do
  if python3 "$PROBE" "$PORT" 2>/dev/null; then
    echo "[tunnel] ready on :$PORT — Mongo answered a handshake through $TARGET"
    if [ -n "${GITHUB_ENV:-}" ]; then
      echo "KINOWO_CONVERGENCE_SCRAPES_URI=$URI" >> "$GITHUB_ENV"
    else
      echo "[tunnel] no \$GITHUB_ENV — export KINOWO_CONVERGENCE_SCRAPES_URI yourself" >&2
    fi
    exit 0
  fi
  sleep 2
done

echo "[tunnel] prod Mongo never answered a handshake on :$PORT (via $TARGET)" >&2
tail -20 "$LOG" >&2 || true
exit 1
