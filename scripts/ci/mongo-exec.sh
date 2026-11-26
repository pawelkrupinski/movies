#!/usr/bin/env bash
# Purpose: Run a command where the job's throwaway MongoDB runs (start-mongo-replset.sh)
# Run a command where the job's throwaway MongoDB runs (start-mongo-replset.sh): beside the NATIVE mongod a job
# image bakes in (the marker that script leaves names its home, whose bin/ -- mongosh -- comes first on PATH), else inside
# the `mongo` container. HOME=/tmp either way, keeping mongosh's state out of the data directory.
#
#   mongo-exec.sh [-d] <command...>    e.g. mongo-exec.sh mongosh --quiet --eval 'db.runCommand({ping:1})'
#   mongo-exec.sh --scratch-dir        a directory a command run through this script can keep files in, across calls:
#                                       the runner's own temp directory beside a native mongod, else the container's /tmp
#                                       -d: detached, as `docker exec -d` (the command runs on in the background)
# Tested by scripts/ci/start_mongo_replset_test.py.
set -uo pipefail
marker=${MONGO_NATIVE_MARKER:-${RUNNER_TEMP:-/tmp}/kinowo-mongo-native}
detach=""
if [ "${1:-}" = --scratch-dir ]; then
  if [ -s "$marker" ]; then echo "${RUNNER_TEMP:-/tmp}"; else echo /tmp; fi
  exit 0
fi
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
