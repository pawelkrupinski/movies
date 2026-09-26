// Read-only snapshot of production's identity decisions, for scripts/identity-calibrate.sh.
//
// Emits one JSON object per line: {"kind":"film",...} for every `movies` row (its tmdbId,
// imdbId and the rating-page URLs, which are production's DECISIONS, never labels on their
// own) and {"kind":"slot",...} for every `movie_slots` row (one venue's listing as the venue
// published it, and the film production filed it under). Synopses and showtimes are dropped.
// Only find() with projections: nothing here writes.
const dbName = process.env.KINOWO_CALIBRATE_DB;
const d = db.getSiblingDB(dbName);
const out = (o) => print(JSON.stringify(o));
d.movies.find({}, {tmdbId: 1, imdbId: 1, key: 1, tmdbNoMatch: 1, metacriticUrl: 1, rottenTomatoesUrl: 1, filmwebUrl: 1})
  .batchSize(2000)
  .forEach(m => out({kind: "film", db: dbName, id: m._id, tmdbId: m.tmdbId ?? null, imdbId: m.imdbId ?? null,
                     tmdbNoMatch: m.tmdbNoMatch ?? null, metacriticUrl: m.metacriticUrl ?? null,
                     rottenTomatoesUrl: m.rottenTomatoesUrl ?? null, filmwebUrl: m.filmwebUrl ?? null}));
d.movie_slots.find({}, {filmId: 1, slotKey: 1, "slot.title": 1, "slot.rawTitle": 1, "slot.originalTitle": 1,
                        "slot.director": 1, "slot.runtimeMinutes": 1, "slot.releaseYear": 1, "slot.countries": 1,
                        "slot.filmUrl": 1})
  .batchSize(5000)
  .forEach(s => out({kind: "slot", db: dbName, filmId: s.filmId, slotKey: s.slotKey, slot: s.slot || {}}));
