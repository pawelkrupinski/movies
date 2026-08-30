#!/usr/bin/env bash
#
# Assertion for the seed → tail handover: a write that lands AFTER a collection
# has been copied but BEFORE the tailer opens its stream must still reach the
# mirror. Unlike the other specs here this one needs a real replica set, because
# the thing under test is a change stream's start point — so it runs against the
# local mirror Mongo (:28017) that mirror.sh already manages, in throwaway
# databases it drops afterwards. It never touches prod.
#
#   scripts/local-mirror/seed-tail-race-spec.sh
#
# Exits 0 when the case passes, 1 on failure, and 0 with a loud SKIP line when
# no local Mongo is running.
#
# Why this file exists: seed.js used to clear the resume token on the way out,
# so tail.js started "from now" — from the moment the LAST collection finished
# copying. Every write to an earlier collection in between was lost with nothing
# to repair it: staleness.js measures lag from `updatedAt` (a delete has none)
# and count-drift from `movies` only. On 2026-08-29 a re-seed copied DE's
# `pending_movies` while three films were still incubating, the staging fold
# deleted those rows during the copy's remaining collections, and /debug showed
# three films stuck in staging for an hour after prod had folded them — while
# every health signal read green.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

LOCAL_URI="mongodb://127.0.0.1:28017/?directConnection=true"
SRC_DB="kinowo_seedrace_spec"
MIRROR_DB="${SRC_DB}_prod_mirror"
# One of the real mirrored collections, so mirror-targets.js's list covers it
# without the spec having to fake the target list.
COLL="pending_movies"

m() { mongosh "$LOCAL_URI" --quiet --eval "$1" 2>/dev/null; }

if ! m 'db.adminCommand({ping:1}).ok' | grep -q 1; then
  echo "[spec] seed→tail race: SKIPPED — no Mongo on 127.0.0.1:28017"
  echo "       start one with scripts/local-mirror/start-local-mongo.sh to run this case"
  exit 0
fi

cleanup() {
  m "db.getSiblingDB('$SRC_DB').dropDatabase(); db.getSiblingDB('$MIRROR_DB').dropDatabase()" >/dev/null
}
trap cleanup EXIT

echo "[spec] seed→tail race"
cleanup

# ── Arrange: two rows in the source, both copied by the seed ─────────────────
m "db.getSiblingDB('$SRC_DB').getCollection('$COLL').insertMany([{_id:'keep'},{_id:'doomed'}])" >/dev/null

mongosh "$LOCAL_URI" --quiet \
  --eval "var DST='$LOCAL_URI'; var SRC_DB='$SRC_DB'" \
  --file "$HERE/mirror-targets.js" --file "$HERE/seed.js" >/dev/null 2>&1

seeded="$(m "db.getSiblingDB('$MIRROR_DB').getCollection('$COLL').countDocuments({})" | tr -d '[:space:]')"
if [ "$seeded" != "2" ]; then
  echo "  FAIL the seed did not copy both rows (mirror has ${seeded:-none})"
  exit 1
fi

# ── The race: a write landing between the copy and the tailer's first read ───
m "db.getSiblingDB('$SRC_DB').getCollection('$COLL').deleteOne({_id:'doomed'})" >/dev/null

# ── Act: hand over to the tailer, exactly as supervise_db does ───────────────
mongosh "$LOCAL_URI" --quiet \
  --eval "var DST='$LOCAL_URI'; var SRC_DB='$SRC_DB'" \
  --file "$HERE/mirror-targets.js" --file "$HERE/stream-start.js" --file "$HERE/tail.js" \
  >/dev/null 2>&1 &
tailpid=$!
# The tailer blocks on an idle stream by design, so it is stopped on the clock
# rather than trusted to end. Generous enough that a slow apply cannot be read
# as a lost event — a false PASS is the only outcome worse than a false FAIL.
sleep 5
kill "$tailpid" 2>/dev/null
wait "$tailpid" 2>/dev/null

# ── Assert: the mirror caught up with the delete it raced past ───────────────
ghost="$(m "db.getSiblingDB('$MIRROR_DB').getCollection('$COLL').countDocuments({_id:'doomed'})" | tr -d '[:space:]')"
kept="$(m  "db.getSiblingDB('$MIRROR_DB').getCollection('$COLL').countDocuments({_id:'keep'})"   | tr -d '[:space:]')"

if [ "$kept" != "1" ]; then
  echo "  FAIL the tailer lost a row the seed had copied ('keep' missing)"
  exit 1
fi
if [ "$ghost" != "0" ]; then
  echo "  FAIL a delete during the seed→tail gap never reached the mirror — 'doomed' is a ghost row"
  echo "       (this is the bug: tail.js started from now, past the delete)"
  exit 1
fi

echo "  ok   a delete during the seed→tail gap still reaches the mirror"
echo "[spec] seed→tail race: all cases pass"
