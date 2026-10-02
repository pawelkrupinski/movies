#!/usr/bin/env bash
# Where the prod-Mongo tunnel keeps its pieces on a runner — sourced by the script that
# opens it (wait-for-mongo-tunnel.sh) and the one that closes it (close-mongo-tunnel.sh),
# so "close" can never miss a file or a process "open" left behind under a renamed path.
#
# shellcheck disable=SC2034  # read by the scripts that source this file
# Sets: TUNNEL_WORK, TUNNEL_KEY, TUNNEL_KNOWN_HOSTS, TUNNEL_RELAY, TUNNEL_SUPERVISOR.
TUNNEL_WORK="${RUNNER_TEMP:-${TMPDIR:-/tmp}}"
TUNNEL_KEY="$TUNNEL_WORK/mongo-ci-read.key"
TUNNEL_KNOWN_HOSTS="$TUNNEL_WORK/mongo-ci-read.known_hosts"
TUNNEL_RELAY="$TUNNEL_WORK/mongo-relay.sh"
TUNNEL_SUPERVISOR="$TUNNEL_WORK/mongo-tunnel-supervisor.sh"
