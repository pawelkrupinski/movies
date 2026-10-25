#!/usr/bin/env bash
# Run one command against PROD Mongo from a laptop: the credential from the Touch ID vault, the
# connection through the ssh tunnel.
#
#   scripts/with-prod-mongo.sh [--root | --both] <command...>
#   e.g. scripts/with-prod-mongo.sh sbt 'worker/Test/runMain scripts.ReapOrphanedFilmRows'
#
# What the command gets, in its environment:
#   MONGODB_URI     the vault's MONGODB_URI (with --root: MONGO_ROOT_URI) rewritten to the tunnel —
#                   127.0.0.1:<port>, directConnection=true, credentials / db path / other params kept
#                   (prod-tunnel.sh's prod_tunnel_uri). The vault holds the in-cluster address, which a
#                   laptop cannot reach.
#   MONGO_ROOT_URI  with --both only: MONGODB_URI stays the APP's, and the root credential comes too,
#                   rewritten to the SAME tunnel. For a job that needs both users (scripts/mongo-grants,
#                   scripts/fleet-db-finish): one `secrets run`, one Touch ID and one tunnel instead of
#                   one of each per step.
#   MONGODB_DB      as already set, else `kinowo` (Poland's database).
#
# The secret comes from the environment when it is already set (a caller that exported it) and from
# the vault otherwise: this script re-runs itself under `secrets run movies MONGODB_URI` (one Touch
# ID; --both asks for the two keys in that same one run). There is no .env.local any more (2026-10-09). The tunnel is prod-tunnel.sh's: an ssh forward
# to mongo-1 (KINOWO_MONGO_SSH overrides the target), reused when one already answers.
#
# The command runs as a CHILD, not via exec: a tunnel this script opened is closed when the command
# ends instead of being left behind as a stray ssh. The command's exit status is this script's.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/local-mirror/prod-tunnel.sh
. "$HERE/local-mirror/prod-tunnel.sh"

key=MONGODB_URI
keys=(MONGODB_URI)
flags=()
case "${1:-}" in
  --root) key=MONGO_ROOT_URI; keys=(MONGO_ROOT_URI); flags=(--root); shift ;;
  --both) keys=(MONGODB_URI MONGO_ROOT_URI); flags=(--both); shift ;;
esac
[ $# -gt 0 ] || { echo "usage: $0 [--root | --both] <command...>" >&2; exit 2; }

# `${flags[@]+…}`: bash 3.2 (macOS's /bin/bash) calls an empty array unbound under `set -u`.
ensure_vault_env "${keys[@]}" -- "$BASH" "$0" ${flags[@]+"${flags[@]}"} "$@" || exit 1

uri="$(prod_tunnel_uri "${!key}")"
if [ "${keys[*]}" = "MONGODB_URI MONGO_ROOT_URI" ]; then
  # The root URI goes through the app URI's port, so both name the one tunnel opened below.
  port="$(printf '%s' "$uri" | sed -nE 's#^mongodb://([^@/]*@)?127\.0\.0\.1:([0-9]+).*#\2#p')"
  MONGO_ROOT_URI="$(PROD_TUNNEL_PORT="$port" prod_tunnel_uri "$MONGO_ROOT_URI")"
  export MONGO_ROOT_URI
fi
init_prod_tunnel with-prod-mongo "$uri"
trap close_prod_tunnel EXIT
ensure_prod_tunnel >&2 || { echo "[with-prod-mongo] prod Mongo unreachable — can you 'ssh' to the mongo host? (see local-mirror/prod-tunnel.sh)" >&2; exit 1; }

export MONGODB_URI="$uri"
export MONGODB_DB="${MONGODB_DB:-kinowo}"
# The tunnel's own progress lines went to stderr above, so the command's stdout stays its own.
status=0
"$@" || status=$?
exit "$status"
