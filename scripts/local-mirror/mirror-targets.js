// What the local `/debug` mirror holds, shared by seed.js and tail.js.
//
// Loaded via mongosh's `--file` list BEFORE the script that uses it, so these
// names are plain globals — mongosh has no module system.
//
// These are what a /debug page load reads: the corpus table (`movies`, with
// `screenings` stitched back in for showtimes and `movie_slots` carrying the
// per-film slot rows the corpus readers resolve alongside it), the per-row
// expand's two stores (`enrichment_attempts`, `rating_cadence`), the staging
// table (`pending_movies`), and the read-cache dump (`web_movies`,
// `web_screenings`).
//
// `cinema_scrapes` is the exception: it backs no page. It holds each cinema's
// last content-bearing scrape — the listing the client actually produced, before
// the corpus merge touched it — which is the first thing you want when a scrape
// looks wrong, and the corpus a local replay replays. Reading that over the
// tunnel is the latency this mirror exists to remove.
//
// Adding a collection here is what makes it readable at LAN latency; anything
// absent reads as permanently EMPTY, NOT slowly-from-prod — the /debug country
// stacks read the mirror unconditionally, with no fall-back to the tunnel.
// `services.DebugMirror` is the Scala side of this list, and
// `MongoConnectionSpec` fails when the two disagree.
//
// A collection added here is MISSING from every existing mirror until it is
// re-seeded — which is why staleness.js treats "prod has documents, the mirror
// has none of that collection" as stale, so the next cycle heals it by itself.
const MIRRORED_COLLECTIONS = [
  "movies", "screenings", "movie_slots",
  "enrichment_attempts", "rating_cadence",
  "pending_movies",
  "web_movies", "web_screenings",
  "cinema_scrapes",
];

// Prod's per-country databases sit side by side on the ONE local mirror
// instance, each suffixed rather than reusing prod's name. The suffix is load
// bearing: a locally-run worker pointed at the same instance defaults to the
// `kinowo` database, and without it that worker would write straight into the
// mirrored corpus (the two-database split the README's `kinowo_local` warning
// is about). `services.MongoConnection.mirrorDbFor` derives the SAME name on
// the Scala side — change one and you must change the other.
function mirrorDbFor(prodDb) { return prodDb + "_prod_mirror"; }
