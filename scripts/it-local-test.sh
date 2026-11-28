#!/usr/bin/env bash
# Assertions for scripts/it-local.sh: the it/ layer runs against a mongod of its OWN, a primary, and nothing is left
# behind -- and its sbt never builds while the checkout's kept sbt-warm server does. mongod, mongosh and sbt are fakes
# on PATH; no real server starts.
#
#   scripts/it-local-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-it-local.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export STUB_LOG="$SCRATCH/log"
# sbt-warm's state (it-local.sh takes the checkout's build lock through it): the spec's own, never this Mac's servers'
export KINOWO_SBT_WARM_STATE="$SCRATCH/warm"
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

# ONE BUILDER PER target/ (2026-11-20): it-local.sh's own sbt and the checkout's kept sbt-warm server compiled the same
# classes at once, and an itAll failed only because worker/Test was compiling in the other. In a scratch checkout of the
# scripts, the stub sbt "builds" for a while -- as the server's client (`--client`) or as it-local's own sbt -- and
# says so: a build starting while another is under way is an OVERLAP.
. "$REPO_ROOT/scripts/scratch-git.sh"
tree="$SCRATCH/tree"; scratch_repo "$tree"
mkdir -p "$tree/scripts/lib" "$tree/worker/target/scala-3.x" "$tree/project/target"
for f in it-local.sh own-mongod.sh sbt-warm jvm-gate.sh tool-conventions.sh stall-watch.sh \
  stall_watch.py zinc_orphans.py; do
  cp "$REPO_ROOT/scripts/$f" "$tree/scripts/$f"
done
cp -R "$REPO_ROOT"/scripts/lib/. "$tree/scripts/lib/"   # sbt-warm's Python modules (scripts/lib/sbt_warm/ and theirs)
export KINOWO_IDENTITY_CACHE="$SCRATCH/identity" KINOWO_SBT_CACHE="$SCRATCH/cache" \
  KINOWO_SBT_CACHE_PUBLISH=0 KINOWO_SBT_WARM_SEED="$SCRATCH/no-seed" KINOWO_SBT_WARM_GITOPS=0 \
  KINOWO_SBT_WARM_KEEPER_POLL=0.2 KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS=1
unset KINOWO_STALL_FILE KINOWO_PROGRESS_FILE
BUILDS="$SCRATCH/builds"
cat > "$FAKE/sbt" <<STUB
#!/usr/bin/env bash
who=it-local; [ "\${1:-}" = --client ] && who=server
mkdir "$BUILDS.building" 2>/dev/null || echo "OVERLAP \$who" >> "$BUILDS"
echo "start \$who" >> "$BUILDS"
touch "$BUILDS.\$who-started"
sleep "\${STUB_BUILD_SECONDS:-3}"
echo "end \$who" >> "$BUILDS"
rmdir "$BUILDS.building" 2>/dev/null
exit 0
STUB
chmod +x "$FAKE/sbt"
warm_compile() { ( cd "$tree" && env PATH="$FAKE:$PATH" scripts/sbt-warm "worker/Test/compile" ) > "$SCRATCH/warm.out" 2>&1; }
it_local() { ( cd "$tree" && env PATH="$FAKE:$PATH" IT_LOCAL_START_SECONDS=2 scripts/it-local.sh 'worker/IntegrationTest/test' ) > "$SCRATCH/it.out" 2>&1; }
builds() { tr '\n' ' ' < "$BUILDS" | sed 's/ $//'; }

rm -f "$BUILDS"*
warm_compile & warm=$!
wait_until "the server's build started" '[ -f "$BUILDS.server-started" ]'
it_local; it_code=$?; wait "$warm"; warm_code=$?
check "it-local.sh while the kept server compiles: its own sbt waits for that build to end, then builds" \
  "0 0 start server end server start it-local end it-local" "$it_code $warm_code $(builds)"
check "...said" "1" "$(grep -c 'waits for it to end' "$SCRATCH/it.out")"

rm -f "$BUILDS"*
it_local & itl=$!
wait_until "it-local's build started" '[ -f "$BUILDS.it-local-started" ]'
warm_compile; warm_code=$?; wait "$itl"; it_code=$?
check "a server call while it-local.sh's sbt builds: it waits for that build to end, then builds" \
  "0 0 start it-local end it-local start server end server" "$it_code $warm_code $(builds)"
check "...said, naming it" "1" "$(grep -c 'exclusively (scripts/it-local.sh' "$SCRATCH/warm.out")"

spec_summary
