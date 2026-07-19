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

// Fresh snapshot → drop this database's stale resume token so its tailer starts
// from now. Keyed by source database, since each database gets its own stream.
dstDb.getCollection("__mirror_state").deleteOne({ _id: SRC_DB });
print(`[seed] ${SRC_DB} done`);
