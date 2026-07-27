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
// live in staleness-rule.js. Six queries per cycle (~110ms each against prod
// over the tunnel, sub-ms locally), which is why mirror.sh backs off between
// failed cycles rather than re-checking every 2s.
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

const verdict = stalenessVerdict({
  prodCount: srcDb.movies.countDocuments(),
  mirrorCount: dstDb.movies.countDocuments(),
  prodMaxUpdatedAtMs: maxUpdatedAtMs(srcDb),
  mirrorMaxUpdatedAtMs: maxUpdatedAtMs(dstDb),
  missingCollections: MIRRORED_COLLECTIONS.filter(
    name => hasDocuments(srcDb, name) && !hasDocuments(dstDb, name)),
});

print(`[stale] ${SRC_DB}: ${verdict.stale ? "STALE" : "ok"} — ${verdict.reason}`);
if (verdict.stale) quit(3);
