// scripts/venue-replay --prod: one venue's rows out of the LOCAL prod mirror (:28017, read-only), as the recording's
// corpus file spells them (StoredScrapeDto: instants as ISO strings, a showtime as its local date-time), so
// VenueReplay reads prod's current listings through the same parser it reads the recording with.
//
//   mongosh --quiet <mirror uri> --eval "var SELECTOR='…'" --file scripts/venue-replay-prod.js
//   -> {"cinema_scrapes":[rows], "identity_listings":[rows], "venue_pages":[{page, readAt, synopsis, director, group}]}
function fold(s) {
  return s.normalize("NFD").replace(/\p{M}/gu, "").replace(/ł/g, "l").replace(/Ł/g, "L").toLowerCase().replace(/\s+/g, " ").trim();
}
function plain(v, inShowtime) {
  if (v instanceof Date) return inShowtime ? v.toISOString().slice(0, 19) : v.toISOString();
  if (Array.isArray(v)) return v.map(x => plain(x, inShowtime));
  if (v && typeof v === "object" && !(v instanceof ObjectId)) {
    const out = {};
    for (const k of Object.keys(v)) out[k] = plain(v[k], inShowtime || k === "showtimes");
    return out;
  }
  return v;
}
const want = fold(SELECTOR);
function rowsOf(coll) {
  const all = db.getCollection(coll).find({}, { _id: 1 }).toArray().map(d => d._id).filter(id => typeof id === "string");
  const exact = all.filter(id => fold(id) === want);
  const ids = exact.length ? exact : all.filter(id => fold(id).includes(want));
  return db.getCollection(coll).find({ _id: { $in: ids } }).toArray().map(d => plain(d, false));
}
const scrapes = rowsOf("cinema_scrapes");
const urls = [...new Set(scrapes.flatMap(r => (r.films || []).map(f => f.filmUrl).filter(u => u)))];
const pages = db.venue_pages.find({ page: { $in: urls } }, { page: 1, group: 1, readAt: 1, synopsis: 1, director: 1 }).toArray()
  .map(d => plain(d, false));
print(JSON.stringify({ cinema_scrapes: scrapes, identity_listings: rowsOf("identity_listings"), venue_pages: pages }));
