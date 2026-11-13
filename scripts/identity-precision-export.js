// One country's SERVED takes, as JSON lines, for scripts/identity_precision_sample.py: every (venue, raw title, TMDB
// film) a screening with an upcoming showtime is served on, with its upcoming showtimes counted, the billing the venue
// scraped and the TMDB record taken -- the evidence a judge reads.
// Run (read-only, the LOCAL mirror only):
//   NOW=<ISO instant> mongosh --quiet mongodb://127.0.0.1:28017/<db> scripts/identity-precision-export.js
//
// The served film is the read model's (`screenings.filmId` -> `movies.tmdbId`), not the trace's: what a visitor sees.
// The billing's venue and raw title are its identity trace's (`identity_traces`, keyed by the screening's listingKey);
// a listing with no trace is keyed from the listingKey itself ("N\0venue\0url\0title", "P\0venue\0title\0").
const now = new Date(process.env.NOW || new Date().toISOString());
function w(o) { print(JSON.stringify(o)); }
function fromKey(key) {
  const parts = key.split("\u0000");
  return parts[0] === "P" ? {venue: parts[1], rawTitle: parts[2]} : {venue: parts[1], rawTitle: parts[parts.length - 1]};
}
const takes = new Map();
db.screenings.aggregate([
  {$project: {filmId: 1, listingKey: 1, n: {$size: {$filter: {input: "$showtimes", cond: {$gte: ["$$this.dateTime", now]}}}}}},
  {$match: {n: {$gt: 0}}},
  {$lookup: {from: "movies", localField: "filmId", foreignField: "_id", as: "m", pipeline: [{$project: {tmdbId: 1}}]}},
  {$lookup: {from: "identity_traces", localField: "listingKey", foreignField: "_id", as: "t",
             pipeline: [{$project: {listing: 1, film: 1, basis: 1}}]}},
  {$project: {n: 1, listingKey: 1, tmdb: {$first: "$m.tmdbId"}, t: {$first: "$t"}}},
  {$match: {tmdb: {$ne: null}}}
], {allowDiskUse: true}).forEach(s => {
  const listing = s.t ? {venue: s.t.listing.venue, rawTitle: s.t.listing.rawTitle} : fromKey(s.listingKey);
  const key = [listing.venue, listing.rawTitle, s.tmdb].join("\u0000");
  const take = takes.get(key) || {venue: listing.venue, rawTitle: listing.rawTitle, film: "tmdb:" + s.tmdb, tmdb: s.tmdb,
                                  showtimes: 0, basis: s.t ? s.t.basis : null, traceFilm: s.t ? s.t.film : null};
  take.showtimes += s.n;
  takes.set(key, take);
});

// The billing as the venue scraped it: the first film entry of the venue's identity listing titled so.
const billings = new Map();
db.identity_listings.find({}, {"films.movie": 1, "films.director": 1, "films.synopsis": 1, "films.externalIds": 1,
                               "films.filmUrl": 1, "films.posterUrl": 1}).forEach(d =>
  (d.films || []).forEach(f => {
    const key = d._id + "\u0000" + f.movie.title;
    if (!billings.has(key)) billings.set(key, {
      year: f.movie.releaseYear || null, runtime: f.movie.runtimeMinutes || null, originalTitle: f.movie.originalTitle || null,
      directors: f.director || [], synopsis: (f.synopsis || "").slice(0, 300), ids: f.externalIds || {},
      url: f.filmUrl || null, poster: f.posterUrl || null});
  }));

const ids = [...new Set([...takes.values()].map(t => String(t.tmdb)))];
const records = new Map();
db.tmdb_films.find({_id: {$in: ids}}, {record: 1}).forEach(f => records.set(f._id, f.record || {}));

for (const t of takes.values()) {
  const r = records.get(String(t.tmdb)) || {};
  w(Object.assign(t, {
    billing: billings.get(t.venue + "\u0000" + t.rawTitle) || null,
    record: {title: r.title || null, originalTitle: r.originalTitle || null, year: r.year || null, runtime: r.runtime || null,
             directors: r.directors || []}}));
}
