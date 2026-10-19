// Observe how current each local mirror database is, for the tools that READ it (scripts/local-mirror/freshness runs
// this; do not run directly). LOCAL only: it reads the mirror on :28017 and never prod.
//
//   mongosh --nodb --quiet --eval "var LOCAL='<uri>'; var CCS=['uk']; var MAX_LAG_MS=…; var NOT_BEFORE_MS=…; var NEED=[…]" \
//     --file mirror-targets.js --file staleness-rule.js --file freshness-rule.js --file freshness.js
//
// Per database: the sync moment (the cluster time of the last change the tailer applied, from its resume token in
// `__mirror_state`; the seed's pre-copy operation time when it has applied none yet), and per mirrored collection its
// document count and its newest write where the collection stamps one. Exits 3 when any database is not fresh
// (freshness-rule.js), 2 when one is absent.

// The field each collection stamps its writes with, where it has one: Dates, ISO strings or epoch-millis longs.
const STAMPS = {
  movies: "updatedAt", screenings: "updatedAt", movie_slots: "updatedAt", cinema_scrapes: "scrapedAt",
  venue_pages: "readAt", identity_family_answers: "fetchedAt", tmdb_films: "fetchedAt",
  uptimeBuckets: "bucket",
};

function stampMs(v) {
  if (v === null || v === undefined) return null;
  if (v instanceof Date) return v.getTime();
  if (typeof v === "string") { const t = Date.parse(v); return isNaN(t) ? null : t; }
  if (typeof v === "number") return v;
  if (typeof v.toNumber === "function") return v.toNumber();
  return Number(v);
}
const iso = ms => ms === null ? "—" : new Date(ms).toISOString().replace(/\.\d+Z$/, "Z");

const local = new Mongo(LOCAL);
const now = Date.now();
const present = local.getDB("admin").adminCommand({ listDatabases: 1, nameOnly: true }).databases.map(d => d.name);
let worst = 0;

CCS.forEach(cc => {
  const prodDb = prodDbOf(cc);
  const name = mirrorDbFor(prodDb);
  if (!present.includes(name)) {
    print(`${cc}  ${name}: ABSENT — the mirror holds no copy of ${prodDb} (scripts/local-mirror/service.sh install)`);
    worst = Math.max(worst, 2);
    return;
  }
  const mdb = local.getDB(name);
  const state = mdb.getCollection("__mirror_state");
  const saved = state.findOne({ _id: prodDb }) || {};
  const fromToken = resumeTokenMs(saved.resumeToken && saved.resumeToken._data);
  const syncedAtMs = fromToken !== null ? fromToken
    : saved.startAtOperationTime ? saved.startAtOperationTime.getHighBits() * 1000 : null;
  const seedIncomplete = state.find({ _id: prodDb + ":seed" }).limit(1).toArray().length > 0;
  const rows = MIRRORED_COLLECTIONS.map(c => {
    const coll = mdb.getCollection(c);
    const count = coll.estimatedDocumentCount();
    const field = STAMPS[c];
    let newest = null;
    if (field && count > 0) {
      const doc = coll.find({ [field]: { $exists: true } }, { [field]: 1 }).sort({ [field]: -1 }).limit(1).toArray()[0];
      newest = doc ? stampMs(doc[field]) : null;
    }
    return { c, count, newest };
  });
  const missing = NEED.filter(c => { const r = rows.find(x => x.c === c); return !r || r.count === 0; });
  const verdict = freshnessVerdict({ syncedAtMs, nowMs: now, maxLagMs: MAX_LAG_MS, notBeforeMs: NOT_BEFORE_MS, seedIncomplete, missing });
  const lag = verdict.lagMs === null ? "?" : formatDuration(verdict.lagMs);
  print(`${cc}  ${name}: synced ${iso(syncedAtMs)} (${lag} ago${fromToken === null && syncedAtMs !== null ? ", seed start — no change applied yet" : ""}) — ` +
    (verdict.fresh ? "fresh" : "WARN: " + verdict.warnings.join("; ")));
  if (!QUIET) rows.forEach(r => print(`      ${r.c.padEnd(24)} ${String(r.count).padStart(8)}  ${r.newest === null ? "" : "newest " + iso(r.newest)}`));
  if (!verdict.fresh) worst = Math.max(worst, 3);
});
if (worst > 0) quit(worst);
