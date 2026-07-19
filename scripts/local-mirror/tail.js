// Mirror ONE prod database's mirrored collections → its local mirror database,
// via a resumable DATABASE-level change stream.
//
// Invoked by mirror.sh, one process per database (do not run directly):
//   mongosh "<PROD_URI>" \
//     --eval "var DST='<LOCAL_URI>'; var SRC_DB='kinowo_uk'" \
//     --file mirror-targets.js --file tail.js
//
// One stream per DATABASE, not per collection: four collection streams would be
// four blocking cursors that a single-threaded mongosh cannot interleave. The
// next level up — one deployment-wide stream covering every country at once —
// is what the credentials refuse (`db.getMongo().watch()` → Unauthorized), so
// the database is both the widest scope available and the natural unit of
// restart.
//
// `db` is the prod (source) connection the URI opened; `DST` is the local
// mirror URI. The resume token is persisted on the LOCAL side (so it survives a
// tailer restart), letting reconnects continue without re-seeding. If the token
// has aged out of prod's oplog, `watch()` throws and mirror.sh re-seeds.
const srcDb = db.getSiblingDB(SRC_DB);
const dstDb = new Mongo(DST).getDB(mirrorDbFor(SRC_DB));
const state = dstDb.getCollection("__mirror_state");

const saved = state.findOne({ _id: SRC_DB });
// Server-side filter: a database stream carries every collection in the
// database, and we mirror only four of them. Filtering here rather than in the
// loop keeps the untracked collections' churn off the tunnel entirely.
const pipeline = [{ $match: { "ns.coll": { $in: MIRRORED_COLLECTIONS } } }];
const opts = { fullDocument: "updateLookup" };
if (saved && saved.resumeToken) {
  opts.resumeAfter = saved.resumeToken;
  print(`[tail] ${SRC_DB}: resuming after saved token`);
} else {
  print(`[tail] ${SRC_DB}: starting from now (no saved token)`);
}

print(`[tail] ${SRC_DB}: watching ${MIRRORED_COLLECTIONS.join(", ")}…`);
// A stale `resumeAfter` token can fail EITHER when the stream opens OR — more
// commonly — on the first `getMore` once iteration starts ("Resume of change
// stream was not possible, as the resume point may no longer be in the oplog").
// So the recovery handler must wrap the iteration too, not just `watch()`; with
// it around only the open, the resume error escaped uncaught from `hasNext()`,
// mongosh exited 1, and mirror.sh re-ran us on the SAME dead token forever
// instead of re-seeding.
try {
  const cs = srcDb.watch(pipeline, opts);
  cs.disableBlockWarnings();   // hasNext() blocking on an idle stream is expected, not a problem
  let n = 0;
  while (cs.hasNext()) {
    const ev = cs.next();
    // Route by the event's own collection — one stream now feeds four mirrors.
    const dst = dstDb.getCollection(ev.ns.coll);
    switch (ev.operationType) {
      case "insert":
      case "replace":
      case "update":
        // updateLookup gives the full post-image, so every upsert is a clean
        // whole-document replace keyed by _id.
        if (ev.fullDocument) {
          dst.replaceOne({ _id: ev.documentKey._id }, ev.fullDocument, { upsert: true });
          n++;
        }
        break;
      case "delete":
        dst.deleteOne({ _id: ev.documentKey._id });
        n++;
        break;
      // drop / rename / invalidate: ignored — a full re-seed handles those.
    }
    state.updateOne({ _id: SRC_DB }, { $set: { resumeToken: ev._id } }, { upsert: true });
    if (n > 0 && n % 20 === 0) print(`[tail] ${SRC_DB}: applied ${n} changes`);
  }
} catch (e) {
  // The saved token has aged out of prod's oplog (ChangeStreamHistoryLost,
  // code 286 / 280) — resuming is impossible, so drop it and exit 2 to ask
  // mirror.sh for a full re-seed. Any other error (a tunnel blip) exits 1 so
  // mirror.sh just re-runs us and we resume from the same token.
  const lost = e.code === 286 || e.code === 280 ||
    /resume|history lost|no longer.*oplog|oplog/i.test(e.message || "");
  if (lost) { state.deleteOne({ _id: SRC_DB }); print(`[tail] ${SRC_DB}: resume token expired — re-seed needed`); quit(2); }
  print(`[tail] ${SRC_DB}: stream failed (will retry/resume): ` + (e.message || e));
  quit(1);
}
