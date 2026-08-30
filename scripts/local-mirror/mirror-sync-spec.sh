#!/usr/bin/env bash
#
# Assertions for the parts of the sync that only a real replica set can show:
# where a tailer opens its stream, and whether the staleness gate can see a
# mirror holding documents prod deleted. Unlike the other specs here this one
# needs a server — change streams and the gate's own queries are the subject —
# so it runs against the local mirror Mongo (:28017) that mirror.sh already
# manages, in throwaway databases it drops afterwards. It never touches prod.
#
#   scripts/local-mirror/mirror-sync-spec.sh
#
# Exits 0 when every case passes, 1 on the first failure, and 0 with a loud SKIP
# line when no local Mongo is running.
#
# Why this file exists: on 2026-08-29 a re-seed copied DE's `pending_movies`
# while three films were still incubating; the staging fold deleted those rows
# during the copy's remaining collections; the tailer opened AFTER the copy and
# never saw the deletes; and the staleness gate — which measures lag from
# `updatedAt` (a delete carries none) and drift from `movies` alone — called the
# result healthy. /debug showed three films stuck in staging that prod had
# folded an hour earlier. Both halves of that get a case here.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

LOCAL_URI="mongodb://127.0.0.1:28017/?directConnection=true"
SRC_DB="kinowo_sync_spec"
MIRROR_DB="${SRC_DB}_prod_mirror"
# Real mirrored collections, so mirror-targets.js's list covers them without the
# spec having to fake the target list. `pending_movies` is where the incident
# happened and is small enough to reason about; `movies` is what the gate's
# other signals read, and has to look healthy so a case can only ever trip on
# the signal it is about.
GHOST_COLL="pending_movies"

m() { mongosh "$LOCAL_URI" --quiet --eval "$1" 2>/dev/null; }
src() { m "db.getSiblingDB('$SRC_DB').getCollection('$1')${2}"; }
mir() { m "db.getSiblingDB('$MIRROR_DB').getCollection('$1')${2}"; }

# Runs one of the sync's own mongosh entry points exactly as mirror.sh does,
# with the local server standing in for both prod and the mirror.
run_script() {
  mongosh "$LOCAL_URI" --quiet --eval "var DST='$LOCAL_URI'; var SRC_DB='$SRC_DB'" \
    "${@/#/--file=}" >/dev/null 2>&1
}

if ! m 'db.adminCommand({ping:1}).ok' | grep -q 1; then
  echo "[spec] mirror sync: SKIPPED — no Mongo on 127.0.0.1:28017"
  echo "       start one with scripts/local-mirror/start-local-mongo.sh to run these cases"
  exit 0
fi

reset_dbs() {
  m "db.getSiblingDB('$SRC_DB').dropDatabase(); db.getSiblingDB('$MIRROR_DB').dropDatabase()" >/dev/null
}
trap reset_dbs EXIT

failures=0
pass() { echo "  ok   $1"; }
fail() { echo "  FAIL $1"; failures=$((failures + 1)); }

echo "[spec] mirror sync"

# ── A write during the seed → tail gap still reaches the mirror ──────────────
# seed.js used to clear the resume token on its way out, so tail.js opened "from
# now" — from the moment the LAST collection finished copying. Anything written
# to an earlier collection in between was lost with nothing to repair it.
seed_tail_gap() {
  local what="a delete during the seed→tail gap still reaches the mirror"
  reset_dbs
  src "$GHOST_COLL" ".insertMany([{_id:'keep'},{_id:'doomed'}])" >/dev/null

  run_script "$HERE/mirror-targets.js" "$HERE/seed.js"
  if [ "$(mir "$GHOST_COLL" '.countDocuments({})' | tr -d '[:space:]')" != "2" ]; then
    fail "$what (the seed did not copy both rows)"; return
  fi

  # The race: a write landing between the copy and the tailer's first read.
  src "$GHOST_COLL" ".deleteOne({_id:'doomed'})" >/dev/null

  run_script "$HERE/mirror-targets.js" "$HERE/stream-start.js" "$HERE/tail.js" &
  local tailpid=$!
  # The tailer blocks on an idle stream by design, so it is stopped on the clock
  # rather than trusted to end. Generous enough that a slow apply cannot be read
  # as a lost event — a false PASS is the only outcome worse than a false FAIL.
  sleep 5
  kill "$tailpid" 2>/dev/null; wait "$tailpid" 2>/dev/null

  local ghost kept
  ghost="$(mir "$GHOST_COLL" ".countDocuments({_id:'doomed'})" | tr -d '[:space:]')"
  kept="$(mir  "$GHOST_COLL" ".countDocuments({_id:'keep'})"   | tr -d '[:space:]')"
  if [ "$kept" != "1" ];  then fail "$what (the tailer lost a row the seed had copied)"; return; fi
  if [ "$ghost" != "0" ]; then fail "$what (tail.js started past the delete)";           return; fi
  pass "$what"
}

# ── The gate sees a mirror holding documents prod deleted ────────────────────
# Everything else here is deliberately healthy — same `movies` on both sides, no
# missing collection, no torn-seed mark — so these cases can only trip on the
# count comparison the ghost rule added. staleness.js exits 3 for stale.
staleness_verdict() {
  local code
  set +e
  mongosh "$LOCAL_URI" --quiet --eval "var DST='$LOCAL_URI'; var SRC_DB='$SRC_DB'" \
    --file "$HERE/mirror-targets.js" --file "$HERE/staleness-rule.js" --file "$HERE/staleness.js" \
    >/dev/null 2>&1
  code=$?
  set -e
  echo "$code"
}

healthy_pair() {
  reset_dbs
  src movies ".insertOne({_id:'a-film'})" >/dev/null
  mir movies ".insertOne({_id:'a-film'})" >/dev/null
}

# An excess seen for the FIRST time is not yet evidence: it is what a mirror
# still catching up looks like, and what two counts taken a round-trip apart
# look like while the read-model projector rewrites a collection. Measured on
# prod — UK showed a `movies` and `screenings` excess with lag reading 0ms that
# was gone on the next run. A re-seed for one of those costs a full copy.
tolerates_a_first_sighting() {
  local what="an excess seen for the first time does not re-seed"
  healthy_pair
  mir "$GHOST_COLL" ".insertOne({_id:'maybe-catching-up'})" >/dev/null
  if [ "$(staleness_verdict)" = "0" ]; then pass "$what"
  else fail "$what (one sample re-seeded — this is the thrash guard)"; fi
}

# …but one that has STOOD is a delete the mirror is never going to receive. The
# watch's own note is what carries that across runs, so back-dating it is the
# same state a mirror reaches by sitting ahead for a quarter of an hour.
detects_a_standing_ghost() {
  local what="a row the mirror has held for longer than catch-up reads as stale"
  healthy_pair
  mir "$GHOST_COLL" ".insertOne({_id:'ghost'})" >/dev/null
  staleness_verdict >/dev/null                   # first sighting, recorded
  m "db.getSiblingDB('$MIRROR_DB').getCollection('__mirror_state').updateOne(
       {_id:'$SRC_DB:ahead'},
       {\$set:{'collections.$GHOST_COLL.since': Date.now() - 20*60*1000}})" >/dev/null
  if [ "$(staleness_verdict)" = "3" ]; then pass "$what"
  else fail "$what (the gate called a mirror with a standing ghost row fresh)"; fi
}

leaves_a_clean_mirror_alone() {
  local what="the same mirror without the ghost row reads as fresh"
  healthy_pair
  if [ "$(staleness_verdict)" = "0" ]; then pass "$what"
  else fail "$what (the gate would re-seed a healthy mirror)"; fi
}

# An excess that resolves must leave NO trace, or the next one inherits its age
# and re-seeds immediately.
forgets_an_excess_that_clears() {
  local what="an excess that clears is forgotten, not carried forward"
  healthy_pair
  mir "$GHOST_COLL" ".insertOne({_id:'transient'})" >/dev/null
  staleness_verdict >/dev/null                   # first sighting, recorded
  mir "$GHOST_COLL" ".deleteOne({_id:'transient'})" >/dev/null
  staleness_verdict >/dev/null                   # back in line — the note must drop it
  local remembered
  remembered="$(m "db.getSiblingDB('$MIRROR_DB').getCollection('__mirror_state')
                     .countDocuments({_id:'$SRC_DB:ahead', 'collections.$GHOST_COLL': {\$exists: true}})" | tr -d '[:space:]')"
  if [ "$remembered" = "0" ]; then pass "$what"
  else fail "$what (a resolved excess stayed on the watch list)"; fi
}

# A seed cannot leave documents prod deleted behind, so it must clear the watch
# too — a note left over is already old enough to re-seed on the next audit,
# which is a copy that triggers the next copy.
a_seed_clears_the_watch() {
  local what="a fresh seed clears the ahead-watch it inherited"
  healthy_pair
  mir "$GHOST_COLL" ".insertOne({_id:'ghost'})" >/dev/null
  staleness_verdict >/dev/null                   # records the sighting
  run_script "$HERE/mirror-targets.js" "$HERE/seed.js"
  local remembered
  remembered="$(m "db.getSiblingDB('$MIRROR_DB').getCollection('__mirror_state')
                     .countDocuments({_id:'$SRC_DB:ahead'})" | tr -d '[:space:]')"
  if [ "$remembered" = "0" ]; then pass "$what"
  else fail "$what (the seed left a watch entry that will re-seed again)"; fi
}

seed_tail_gap
leaves_a_clean_mirror_alone
tolerates_a_first_sighting
detects_a_standing_ghost
forgets_an_excess_that_clears
a_seed_clears_the_watch

if [ "$failures" -gt 0 ]; then echo "[spec] mirror sync: $failures failure(s)"; exit 1; fi
echo "[spec] mirror sync: all cases pass"
