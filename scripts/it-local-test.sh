#!/usr/bin/env bash
# Assertions for scripts/it-local.sh: the it/ layer runs against a mongod of its OWN, a primary, and nothing is left
# behind. mongod, mongosh and sbt are fakes on PATH; no real server starts.
#
#   scripts/it-local-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-it-local.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export STUB_LOG="$SCRATCH/log"
# mongod: records its --dbpath and pid, then runs until killed
cat > "$FAKE/mongod" <<'STUB'
#!/usr/bin/env bash
[ "${1:-}" = --version ] && { echo "db version v8.0.0"; exit 0; }
while [ $# -gt 0 ]; do [ "$1" = --dbpath ] && echo "$2" > "$STUB_LOG.dbpath"; shift; done
echo $$ > "$STUB_LOG.mongod-pid"
exec sleep 300
STUB
# mongosh: the server answering is ours (it reports our dbpath) unless STUB_FOREIGN; primary unless STUB_NEVER_PRIMARY
cat > "$FAKE/mongosh" <<'STUB'
#!/usr/bin/env bash
case "$*" in
  *serverCmdLineOpts*) if [ -n "${STUB_FOREIGN:-}" ]; then echo /elsewhere/another-runs-data; else cat "$STUB_LOG.dbpath" 2>/dev/null; fi ;;
  *myState*) if [ -n "${STUB_NEVER_PRIMARY:-}" ]; then echo 2; else echo 1; fi ;;
  *rs.initiate*) echo "$*" > "$STUB_LOG.initiate"; echo ok ;;
  *) echo ok ;;
esac
STUB
cat > "$FAKE/sbt" <<'STUB'
#!/usr/bin/env bash
echo "URI=$MONGODB_URI DB=$MONGODB_DB ARGS=$*" > "$STUB_LOG.sbt"
STUB
chmod +x "$FAKE"/*

run() { rm -f "$STUB_LOG".*; ( cd "$REPO_ROOT" && env PATH="$FAKE:$PATH" IT_LOCAL_START_SECONDS=2 "$@" scripts/it-local.sh 'worker/It/testOnly X' ) > "$SCRATCH/out" 2>&1; }
mongod_state() { kill -0 "$(cat "$STUB_LOG.mongod-pid" 2>/dev/null)" 2>/dev/null && echo running || echo stopped; }
data_state() { [ -d "$(cat "$STUB_LOG.dbpath" 2>/dev/null)" ] && echo kept || echo removed; }

echo "[spec] it-local.sh"
run; code=$?
check "runs the suites against its own server's port, on kinowo_it" "0 yes" \
  "$code $(grep -qE '^URI=mongodb://127\.0\.0\.1:[0-9]+/\?directConnection=true DB=kinowo_it ARGS=worker/It/testOnly X$' "$STUB_LOG.sbt" && echo yes || echo no)"
check "...and stops that server and removes its data afterwards" "stopped removed" "$(mongod_state) $(data_state)"
# A dropDatabase always waits for majority (server-side, 10 min); with the journal default on, a single node's majority
# is its journal fsync, which a loaded Mac starved past that wait (MovieRepositoryIntegrationSpec, 2026-10-23).
check "...a replica set acknowledging majority on apply, not on a journal sync" "1" \
  "$(grep -c 'writeConcernMajorityJournalDefault: false' "$STUB_LOG.initiate" 2>/dev/null || echo 0)"

run STUB_FOREIGN=1; code=$?
check "another server answering on the picked port: refused, no suite run against it" "1 no" \
  "$code $([ -f "$STUB_LOG.sbt" ] && echo yes || echo no)"
check "...said" "1" "$(grep -c 'another server holds the port' "$SCRATCH/out")"

run STUB_NEVER_PRIMARY=1; code=$?
check "a server that never becomes PRIMARY: refused, no suite run" "1 no" "$code $([ -f "$STUB_LOG.sbt" ] && echo yes || echo no)"
check "...said, and its server stopped" "1 stopped" "$(grep -c 'did not become PRIMARY' "$SCRATCH/out") $(mongod_state)"

spec_summary
