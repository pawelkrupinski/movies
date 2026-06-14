// Full copy of prod `movies` → the local mirror (drop + refill). Used by
// mirror.sh for the initial seed and for re-seeds when the change stream can't
// resume. Invoked (do not run directly):
//   mongosh "<PROD_URI?compressors=zlib>" --eval "var DST='<LOCAL_URI>'" --file seed.js
//
// Uses a cursor copy over the zlib-compressed prod connection (same path the
// app's findAll uses, ~6x less wire data) rather than mongodump/mongorestore,
// whose uncompressed cursor drops mid-transfer over the flyctl tunnel.
const SRC_DB = "kinowo";              // prod's database (the sync source)
const DST_DB = "kinowo_prod_mirror";  // local mirror database (the sync target)
const src = db.getSiblingDB(SRC_DB).getCollection("movies");
const dstDb = new Mongo(DST).getDB(DST_DB);
const dst = dstDb.getCollection("movies");

print("[seed] copying prod movies → local…");
dst.drop();
let batch = [], n = 0;
src.find().forEach(d => {
  batch.push(d);
  if (batch.length >= 200) { dst.insertMany(batch, { ordered: false }); n += batch.length; batch = []; }
});
if (batch.length) { dst.insertMany(batch, { ordered: false }); n += batch.length; }

// Fresh snapshot → drop any stale resume token so the tailer starts from now.
dstDb.getCollection("__mirror_state").deleteOne({ _id: "movies" });
print(`[seed] copied ${n} docs; local now ${dst.countDocuments()}`);
