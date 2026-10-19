// One country's identity takes, billings and TMDB records, as JSON lines, for scripts/ci/cross_country.py.
// Run (read-only, the LOCAL mirror only):
//   CC=<cc> MODE=traces|listings|films [LIGHT=1] mongosh --quiet mongodb://127.0.0.1:28017/<db> scripts/ml-identity/cross_country_export.js
// LIGHT=1 projects only what the Identity findings report's "countries disagree" section reads (no candidates,
// evidence, runtimes, urls or ids; only the films some trace takes).
const light = process.env.LIGHT === "1";
function w(o){ print(JSON.stringify(o)); }
const mode = process.env.MODE;
if (mode === "traces") {
  const fields = light ? {listing:1, film:1, basis:1, rules:1} : {listing:1, film:1, basis:1, rules:1, evidence:1, candidates:1};
  db.identity_traces.find({}, fields).forEach(t => w(light
    ? {v:t.listing.venue, r:t.listing.rawTitle, f:t.film, b:t.basis, ru:t.rules}
    : {v:t.listing.venue, r:t.listing.rawTitle, p:t.listing.page||null, f:t.film, b:t.basis, ru:t.rules, e:t.evidence, c:t.candidates}));
} else if (mode === "listings") {
  const fields = light ? {"films.movie.title":1,"films.movie.originalTitle":1,"films.movie.releaseYear":1,"films.director":1,"films.showtimes.dateTime":1}
    : {"films.movie.title":1,"films.movie.originalTitle":1,"films.movie.releaseYear":1,"films.movie.runtimeMinutes":1,"films.director":1,"films.showtimes.dateTime":1,"films.filmUrl":1,"films.externalIds":1};
  db.identity_listings.find({}, fields).forEach(d =>
    (d.films||[]).forEach(f => { const ds=(f.showtimes||[]).map(s=>s.dateTime).filter(x=>x).map(x=>x.getTime());
      const window = {s0:ds.length?Math.min(...ds):null, s1:ds.length?Math.max(...ds):null, n:ds.length};
      w(light ? Object.assign({v:d._id, t:f.movie.title, o:f.movie.originalTitle||null, y:f.movie.releaseYear||null, d:f.director||[]}, window)
        : Object.assign({v:d._id, t:f.movie.title, o:f.movie.originalTitle||null, y:f.movie.releaseYear||null, rt:f.movie.runtimeMinutes||null, d:f.director||[], u:f.filmUrl||null, x:f.externalIds||{}}, window)); }));
} else if (mode === "films") {
  const filter = light ? {_id: {$in: db.identity_traces.distinct("film", {film: {$ne: null}}).map(String)}} : {};
  const fields = light ? {"record.title":1,"record.year":1,"record.directors":1} : {"record.title":1,"record.originalTitle":1,"record.year":1,"record.runtime":1,"record.directors":1};
  db.tmdb_films.find(filter, fields).forEach(f =>
    w(light ? {id:f._id, t:f.record?f.record.title:null, y:f.record?f.record.year:null, d:f.record?f.record.directors:null}
      : {id:f._id, t:f.record?f.record.title:null, o:f.record?f.record.originalTitle:null, y:f.record?f.record.year:null, rt:f.record?f.record.runtime:null, d:f.record?f.record.directors:null}));
}
