const cc = process.env.CC;
const out = [];
function w(o){ print(JSON.stringify(o)); }
const mode = process.env.MODE;
if (mode === "traces") {
  db.identity_traces.find({}, {listing:1, film:1, basis:1, rules:1, evidence:1, candidates:1}).forEach(t =>
    w({v:t.listing.venue, r:t.listing.rawTitle, p:t.listing.page||null, f:t.film, b:t.basis, ru:t.rules, e:t.evidence, c:t.candidates}));
} else if (mode === "listings") {
  db.identity_listings.find({}, {"films.movie.title":1,"films.movie.originalTitle":1,"films.movie.releaseYear":1,"films.movie.runtimeMinutes":1,"films.director":1,"films.showtimes.dateTime":1,"films.filmUrl":1,"films.externalIds":1}).forEach(d =>
    (d.films||[]).forEach(f => { const ds=(f.showtimes||[]).map(s=>s.dateTime).filter(x=>x).map(x=>x.getTime()); 
      w({v:d._id, t:f.movie.title, o:f.movie.originalTitle||null, y:f.movie.releaseYear||null, rt:f.movie.runtimeMinutes||null, d:f.director||[], u:f.filmUrl||null, x:f.externalIds||{}, s0:ds.length?Math.min(...ds):null, s1:ds.length?Math.max(...ds):null, n:ds.length}); }));
} else if (mode === "films") {
  db.tmdb_films.find({}, {"record.title":1,"record.originalTitle":1,"record.year":1,"record.runtime":1,"record.directors":1}).forEach(f =>
    w({id:f._id, t:f.record?f.record.title:null, o:f.record?f.record.originalTitle:null, y:f.record?f.record.year:null, rt:f.record?f.record.runtime:null, d:f.record?f.record.directors:null}));
}
