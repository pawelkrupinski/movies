// Ask whether ONE database's mirror has drifted far enough from prod to need a
// re-seed, and answer with an exit code mirror.sh can branch on:
//   0 = fresh, keep tailing        3 = stale, re-seed first
//
// Invoked by mirror.sh once per supervision cycle (do not run directly):
//   mongosh "<PROD_URI?compressors=zlib>" \
//     --eval "var DST='<LOCAL_URI>'; var SRC_DB='kinowo_uk'" \
//     --file mirror-targets.js --file staleness-rule.js --file staleness.js
//
// This file does the OBSERVING only — the threshold logic, and its assertions,
// live in staleness-rule.js. Roughly two dozen queries per cycle (~110ms each
// against prod over the tunnel, sub-ms locally — most of them the per-collection
// counts that catch a mirror holding deleted documents), which is why mirror.sh
// backs off between failed cycles rather than re-checking every 2s.
const srcDb = db.getSiblingDB(SRC_DB);
const dstDb = new Mongo(DST).getDB(mirrorDbFor(SRC_DB));

// `movies` and `screenings` are the mirrored collections carrying `updatedAt`
// (enrichment_attempts / rating_cadence stamp their own differently-named
// fields, and both are written alongside a corpus write anyway). Newest of the
// two = how current this side is.
const TIMESTAMPED = ["movies", "screenings"];

function maxUpdatedAtMs(database) {
  let newest = null;
  TIMESTAMPED.forEach(name => {
    const doc = database.getCollection(name)
      .find({ updatedAt: { $exists: true } }, { updatedAt: 1 })
      .sort({ updatedAt: -1 }).limit(1).toArray()[0];
    if (doc && doc.updatedAt) {
      const ms = doc.updatedAt.getTime ? doc.updatedAt.getTime() : Number(doc.updatedAt);
      if (newest === null || ms > newest) newest = ms;
    }
  });
  return newest;
}

// Existence, not size: one `_id` is enough to tell "prod has this collection
// and the mirror has none of it" — the shape a collection newly added to
// MIRRORED_COLLECTIONS leaves behind, which tailing alone can never fix.
function hasDocuments(database, name) {
  return database.getCollection(name).find({}, { _id: 1 }).limit(1).toArray().length > 0;
}

// How long a collection has to have been ahead before the excess is believed.
// Minutes, not seconds: a single sample cannot tell a missed delete from a
// mirror that has not caught up, and neither can a re-sample moments later.
// Measured on prod — with lag reading 0ms, UK's `movies` and `screenings` both
// showed an excess that was gone on the next run, because the read-model
// projector rewrites a collection by deleting and re-inserting and the two
// counts are taken a round-trip apart. Only an excess that OUTLIVES every burst
// is a delete the mirror is never going to receive; a re-seed for one that
// would have cleared on its own costs a full copy of every collection.
const AHEAD_MIN_AGE_MS = 15 * 60 * 1000;

const state = dstDb.getCollection("__mirror_state");
const AHEAD_KEY = SRC_DB + ":ahead";

// Mirrored collections where the MIRROR holds more documents than prod — which
// can only mean deletes that never arrived — and has done since long enough ago
// that catch-up cannot explain it. The watch itself is the state: each run
// carries forward the moment a collection was FIRST seen ahead and drops the
// ones that have come back into line, so nothing accumulates and an excess that
// resolves leaves no trace.
function mirrorAhead(nowMs) {
  const previous = (state.findOne({ _id: AHEAD_KEY }) || {}).collections || {};
  const current = {};
  const confirmed = [];

  MIRRORED_COLLECTIONS.forEach(name => {
    const prod = srcDb.getCollection(name).countDocuments();
    const mirror = dstDb.getCollection(name).countDocuments();
    if (mirror <= prod) return;

    const since = previous[name] ? previous[name].since : nowMs;
    current[name] = { since };
    if (nowMs - since >= AHEAD_MIN_AGE_MS) confirmed.push({ name, prod, mirror, forMs: nowMs - since });
  });

  state.replaceOne({ _id: AHEAD_KEY }, { _id: AHEAD_KEY, collections: current }, { upsert: true });
  return confirmed;
}

const verdict = stalenessVerdict({
  prodCount: srcDb.movies.countDocuments(),
  mirrorCount: dstDb.movies.countDocuments(),
  prodMaxUpdatedAtMs: maxUpdatedAtMs(srcDb),
  mirrorMaxUpdatedAtMs: maxUpdatedAtMs(dstDb),
  missingCollections: MIRRORED_COLLECTIONS.filter(
    name => hasDocuments(srcDb, name) && !hasDocuments(dstDb, name)),
  mirrorAhead: mirrorAhead(Date.now()),
  // Left behind by a seed that never reached its last collection (seed.js).
  seedIncomplete: state.find({ _id: SRC_DB + ":seed" }, { _id: 1 }).limit(1).toArray().length > 0,
});

print(`[stale] ${SRC_DB}: ${verdict.stale ? "STALE" : "ok"} — ${verdict.reason}`);
if (verdict.stale) quit(3);
