// Mirror prod `movies` → local `movies` via a resumable change stream.
//
// Invoked by mirror.sh (do not run directly):
//   mongosh "<PROD_URI>" --eval "var DST='<LOCAL_URI>'" --file tail.js
//
// `db` is the prod (source) connection the URI opened; `DST` is the local
// mirror URI. The resume token is persisted on the LOCAL side (so it survives
// a tailer restart), letting reconnects continue without re-seeding. If the
// token has aged out of prod's oplog, `watch()` throws on open and mirror.sh
// falls back to a full re-seed.
const SRC_DB = "kinowo";              // prod's database (the sync source)
const DST_DB = "kinowo_prod_mirror";  // local mirror database (the sync target)
const src = db.getSiblingDB(SRC_DB).getCollection("movies");
const dstDb = new Mongo(DST).getDB(DST_DB);
const dst = dstDb.getCollection("movies");
const state = dstDb.getCollection("__mirror_state");

const saved = state.findOne({ _id: "movies" });
const opts = { fullDocument: "updateLookup" };
if (saved && saved.resumeToken) {
  opts.resumeAfter = saved.resumeToken;
  print("[tail] resuming after saved token");
} else {
  print("[tail] starting from now (no saved token)");
}

print(`[tail] local movies: ${dst.countDocuments()} docs — watching prod change stream…`);
// A stale `resumeAfter` token can fail EITHER when the stream opens OR — more
// commonly — on the first `getMore` once iteration starts ("Resume of change
// stream was not possible, as the resume point may no longer be in the oplog").
// So the recovery handler must wrap the iteration too, not just `watch()`; with
// it around only the open, the resume error escaped uncaught from `hasNext()`,
// mongosh exited 1, and mirror.sh re-ran us on the SAME dead token forever
// instead of re-seeding.
try {
  const cs = src.watch([], opts);
  cs.disableBlockWarnings();   // hasNext() blocking on an idle stream is expected, not a problem
  let n = 0;
  while (cs.hasNext()) {
    const ev = cs.next();
    switch (ev.operationType) {
      case "insert":
      case "replace":
      case "update":
        // updateLookup gives the full post-image, so every upsert is a clean
        // whole-document replace keyed by _id (`sanitize(title)|year`).
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
    state.updateOne({ _id: "movies" }, { $set: { resumeToken: ev._id } }, { upsert: true });
    if (n > 0 && n % 20 === 0) print(`[tail] applied ${n} changes`);
  }
} catch (e) {
  // The saved token has aged out of prod's oplog (ChangeStreamHistoryLost,
  // code 286 / 280) — resuming is impossible, so drop it and exit 2 to ask
  // mirror.sh for a full re-seed. Any other error (a tunnel blip) exits 1 so
  // mirror.sh just re-runs us and we resume from the same token.
  const lost = e.code === 286 || e.code === 280 ||
    /resume|history lost|no longer.*oplog|oplog/i.test(e.message || "");
  if (lost) { state.deleteOne({ _id: "movies" }); print("[tail] resume token expired — re-seed needed"); quit(2); }
  print("[tail] stream failed (will retry/resume): " + (e.message || e)); quit(1);
}
