// backfill-title-searches.js — give every resolved production film the title-search evidence the
// pipeline now records at resolution (SourceData.titleSearches on the TMDB slot: where the film
// stood in each listing title's own yearless TMDB search — rank and same-titled rivals), for rows
// resolved before the field existed. The rating gate scores it (StoredIdentityConfidence).
//
// The PLAN is computed read-only, never here: `scripts.IdentityGateImpact --trees <recorded
// enrichment trees> --plan <dir>` measures each row with IdentityMeasures.titleSearch — the
// pipeline's own measure — over the recorded answers of a `Record scrape fixtures` run, and writes
// title-searches-<cc>.jsonl ({filmId, tmdbId, titleSearches}). This script only applies it.
//
// DRY RUN BY DEFAULT: prints what it would write and writes nothing.
//   dry run : PLAN=title-searches-pl.jsonl DB=kinowo mongosh "$URI" --quiet --file backfill-title-searches.js
//   apply   : BACKFILL_APPLY=1 PLAN=… DB=… mongosh "$URI" --quiet --file backfill-title-searches.js
// Per entry it writes ONLY `slot.titleSearches` of the film's TMDB slot, and only while the film
// still carries the planned tmdbId and its slot has no title searches yet — a row re-resolved
// since the plan was computed has recorded its own and is skipped.
const APPLY = process.env.BACKFILL_APPLY === '1';
const PLAN = process.env.PLAN;
const DB = process.env.DB;
if (!PLAN || !DB) { print('set PLAN (a title-searches-<cc>.jsonl) and DB (kinowo, kinowo_uk, …)'); quit(2); }

const SEP = '\x1F';
const d = db.getSiblingDB(DB);
const plan = fs.readFileSync(PLAN, 'utf8').split('\n').filter(l => l.trim()).map(l => JSON.parse(l));
const counts = { planned: plan.length, written: 0, gone: 0, reResolved: 0, alreadyMeasured: 0, noTmdbSlot: 0 };
let shown = 0;

for (const entry of plan) {
  const film = d.movies.findOne({ _id: entry.filmId }, { tmdbId: 1 });
  if (!film) { counts.gone++; continue; }
  if (film.tmdbId !== entry.tmdbId) { counts.reResolved++; continue; }
  const slotId = entry.filmId + SEP + 'TMDB';
  const slot = d.movie_slots.findOne({ _id: slotId }, { 'slot.titleSearches': 1 });
  if (!slot) { counts.noTmdbSlot++; continue; }
  if (slot.slot && Array.isArray(slot.slot.titleSearches) && slot.slot.titleSearches.length) { counts.alreadyMeasured++; continue; }
  if (shown++ < 5) print(`  ${slotId}: slot.titleSearches := ${EJSON.stringify(entry.titleSearches)}`);
  if (APPLY) {
    const r = d.movie_slots.updateOne(
      { _id: slotId, $or: [{ 'slot.titleSearches': { $exists: false } }, { 'slot.titleSearches': { $size: 0 } }] },
      { $set: { 'slot.titleSearches': entry.titleSearches.map(t => Object.assign({ titleKey: t.titleKey, rivals: NumberInt(t.rivals) },
          t.rank === undefined ? {} : { rank: NumberInt(t.rank) })) } });
    counts.written += r.modifiedCount;
  } else counts.written++;
}
print(`${DB}: ${APPLY ? 'APPLIED' : 'DRY RUN (nothing written)'} ${EJSON.stringify(counts)}`);
