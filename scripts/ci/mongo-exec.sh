#!/usr/bin/env bash
# Run a command where the job's throwaway MongoDB runs (start-mongo-replset.sh): beside the NATIVE mongod the Forgejo job
# image bakes in (the marker that script leaves names its home, whose bin/ -- mongosh -- comes first on PATH), else inside
# the `mongo` container. HOME=/tmp either way, keeping mongosh's state out of the data directory.
#
#   mongo-exec.sh [-d] <command...>    e.g. mongo-exec.sh mongosh --quiet --eval 'db.runCommand({ping:1})'
#                                       -d: detached, as `docker exec -d` (the command runs on in the background)
# Tested by scripts/ci/start-mongo-replset-test.sh.
set -uo pipefail
marker=${MONGO_NATIVE_MARKER:-/tmp/kinowo-mongo-native}
detach=""
[ "${1:-}" = -d ] && { detach=1; shift; }
[ "$#" -gt 0 ] || { echo "usage: $0 [-d] <command...>" >&2; exit 64; }
if [ -s "$marker" ]; then
  home=$(cat "$marker")
  if [ -n "$detach" ]; then
    HOME=/tmp PATH="$home/bin:$PATH" nohup "$@" > /dev/null 2>&1 &
    exit 0
  fi
  HOME=/tmp PATH="$home/bin:$PATH" exec "$@"
fi
exec docker exec ${detach:+-d} -e HOME=/tmp mongo "$@"
