// Full copy of ONE prod database's mirrored collections → the local mirror
// (drop + refill). Used by mirror.sh for the initial seed and for re-seeds when
// a change stream can't resume. Invoked once per database (do not run directly):
//   mongosh "<PROD_URI?compressors=zlib>" \
//     --eval "var DST='<LOCAL_URI>'; var SRC_DB='kinowo_uk'" \
//     --file mirror-targets.js --file seed.js
//
// Uses a cursor copy over the zlib-compressed prod connection (same path the
// app's findAll uses, ~6x less wire data) rather than mongodump/mongorestore,
// whose uncompressed cursor drops mid-transfer over the flyctl tunnel.
const srcDb = db.getSiblingDB(SRC_DB);
const dstDb = new Mongo(DST).getDB(mirrorDbFor(SRC_DB));

print(`[seed] ${SRC_DB} → ${mirrorDbFor(SRC_DB)}…`);

// Drop-then-refill is not atomic and not resumable: a seed that dies partway
// leaves the collections it had reached fresh, the one it was mid-copy on
// TRUNCATED, the one it had just dropped EMPTY, and the rest stale — and every
// one of those reads to a caller as ordinary data. That is how /debug came to
// list 934 films with zero cinemas apiece: `movie_slots` was dropped and never
// refilled, and staleness.js's existence check ("prod has documents, the mirror
// has none") is blind to a collection that was merely truncated. So say out
// loud that a seed is in flight, and only clear it on the last collection —
// staleness-rule.js reads an uncleared mark as "re-seed", which is the only
// thing that repairs a torn snapshot.
const state = dstDb.getCollection("__mirror_state");
state.updateOne({ _id: SRC_DB + ":seed" }, { $set: { incomplete: true } }, { upsert: true });

// The instant this snapshot begins, captured BEFORE the first collection is
// read. The tailer starts here rather than "from now" (see stream-start.js):
// the copy below walks the collections one at a time and only then hands over,
// so a write landing on an already-copied collection would otherwise fall into
// a gap nothing repairs. Replaying the copy's own window costs one pass of
// idempotent applies; not replaying it cost three DE films that showed as stuck
// in staging on /debug for an hour after prod had folded them.
const startAtOperationTime = srcDb.getMongo().getDB("admin").runCommand({ ping: 1 }).operationTime;

MIRRORED_COLLECTIONS.forEach(name => {
  const src = srcDb.getCollection(name);
  const dst = dstDb.getCollection(name);
  dst.drop();
  let batch = [], n = 0;
  src.find().forEach(d => {
    batch.push(d);
    if (batch.length >= 200) { dst.insertMany(batch, { ordered: false }); n += batch.length; batch = []; }
  });
  if (batch.length) { dst.insertMany(batch, { ordered: false }); n += batch.length; }
  print(`[seed]   ${name}: ${n} docs`);
});

// Every collection copied → the snapshot is whole again.
state.deleteOne({ _id: SRC_DB + ":seed" });
// Fresh snapshot → replace this database's stream-start state: the old resume
// token belongs to a corpus that no longer exists, and the tailer must pick up
// from where this snapshot BEGAN, not from where it ended. `replaceOne` rather
// than `$set`, so the stale token cannot survive alongside the new time. Keyed
// by source database, since each database gets its own stream. A source that
// reports no operationTime (a standalone, which cannot serve change streams
// anyway) leaves the field unset and the tailer falls back to starting now.
state.replaceOne({ _id: SRC_DB }, { _id: SRC_DB, startAtOperationTime }, { upsert: true });
print(`[seed] ${SRC_DB} done`);
