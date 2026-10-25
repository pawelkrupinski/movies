#!/usr/bin/env bash
# Where the prod-Mongo tunnel keeps its pieces on a runner — sourced by the script that
# opens it (wait-for-mongo-tunnel.sh) and the one that closes it (close-mongo-tunnel.sh),
# so "close" can never miss a file or a process "open" left behind under a renamed path.
#
# shellcheck disable=SC2034  # read by the scripts that source this file
# Sets: TUNNEL_WORK, TUNNEL_KEY, TUNNEL_KNOWN_HOSTS, TUNNEL_RELAY, TUNNEL_SUPERVISOR; defines mongo_ci_read_uri.
TUNNEL_WORK="${RUNNER_TEMP:-${TMPDIR:-/tmp}}"
TUNNEL_KEY="$TUNNEL_WORK/mongo-ci-read.key"
TUNNEL_KNOWN_HOSTS="$TUNNEL_WORK/mongo-ci-read.known_hosts"
TUNNEL_RELAY="$TUNNEL_WORK/mongo-relay.sh"
TUNNEL_SUPERVISOR="$TUNNEL_WORK/mongo-tunnel-supervisor.sh"

# The CI reader's connection string, spelled once: wait-for-mongo-tunnel.sh builds the runner's from it, and
# scripts/mongo-grants reads the reader's auth database off it (the user record the grants go to).
#   mongo_ci_read_uri <user:password> <port>
mongo_ci_read_uri() {
  printf 'mongodb://%s@127.0.0.1:%s/?authSource=admin&directConnection=true&maxPoolSize=8\n' "$1" "$2"
}
