// One country's listings on the local prod mirror, for scripts/unmatched_census.py. READ-ONLY.
// Run through scripts/local-mirror/mongosh-at.sh with --file; parameters come in the environment:
//   UNMATCHED_CENSUS_DATABASE  the country's mirror database (kinowo_prod_mirror, kinowo_<cc>_prod_mirror)
//   UNMATCHED_CENSUS_SINCE     an ISO date; a listing counts when one of its showtimes is on or after it
// Prints one JSON document: {screenings, listings: [{listingKey, filmId, tmdbId, searchTitle, showtimes, trace}]},
// one entry per listingKey, `trace` only for a listing whose film has no tmdbId (null when it has none stored).
const database = db.getSiblingDB(process.env.UNMATCHED_CENSUS_DATABASE);
const since = new Date(process.env.UNMATCHED_CENSUS_SINCE + 'T00:00:00Z');

const films = {};
database.movies.find({}, {tmdbId: 1, searchTitle: 1}).forEach(movie => {
  films[movie._id] = {tmdbId: movie.tmdbId || null, searchTitle: movie.searchTitle || null};
});

const listings = {};
let screenings = 0;
database.screenings.find({'showtimes.dateTime': {$gte: since}}, {filmId: 1, listingKey: 1, showtimes: 1}).forEach(screening => {
  screenings++;
  const film = films[screening.filmId] || {tmdbId: null, searchTitle: null};
  const upcoming = screening.showtimes.filter(showtime => showtime.dateTime >= since).length;
  const existing = listings[screening.listingKey];
  if (existing) { existing.showtimes += upcoming; return; }
  listings[screening.listingKey] = {listingKey: screening.listingKey, filmId: screening.filmId, tmdbId: film.tmdbId,
    searchTitle: film.searchTitle, showtimes: upcoming, trace: null};
});

const unmatchedKeys = Object.values(listings).filter(listing => !listing.tmdbId).map(listing => listing.listingKey);
for (let start = 0; start < unmatchedKeys.length; start += 1000) {
  database.identity_traces.find({_id: {$in: unmatchedKeys.slice(start, start + 1000)}},
      {listing: 1, basis: 1, candidates: 1, refusals: 1, searched: 1, vetoedBy: 1}).forEach(trace => {
    listings[trace._id].trace = {
      rawTitle: trace.listing && trace.listing.rawTitle, year: (trace.listing && trace.listing.year) || null,
      basis: trace.basis, candidates: (trace.candidates || []).slice(0, 4),
      refusals: (trace.refusals || []).slice(0, 3).map(refusal => refusal.rule + ': ' + refusal.why),
      vetoedBy: trace.vetoedBy || null};
  });
}
print(JSON.stringify({screenings: screenings, listings: Object.values(listings)}));
