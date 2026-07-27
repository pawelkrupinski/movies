// What the local `/debug` mirror holds, shared by seed.js and tail.js.
//
// Loaded via mongosh's `--file` list BEFORE the script that uses it, so these
// names are plain globals — mongosh has no module system.
//
// These are what a /debug page load reads: the corpus table (`movies`, with
// `screenings` stitched back in for showtimes), the per-row expand's two stores
// (`enrichment_attempts`, `rating_cadence`), and `movie_slots` — the per-film
// slot rows the corpus readers resolve alongside `movies`. Adding a collection
// here is what makes it readable at LAN latency; anything absent still resolves
// against prod over the tunnel, just slowly.
//
// A collection added here is MISSING from every existing mirror until it is
// re-seeded — which is why staleness.js treats "prod has documents, the mirror
// has none of that collection" as stale, so the next cycle heals it by itself.
const MIRRORED_COLLECTIONS = ["movies", "screenings", "enrichment_attempts", "rating_cadence", "movie_slots"];

// Prod's per-country databases sit side by side on the ONE local mirror
// instance, each suffixed rather than reusing prod's name. The suffix is load
// bearing: a locally-run worker pointed at the same instance defaults to the
// `kinowo` database, and without it that worker would write straight into the
// mirrored corpus (the two-database split the README's `kinowo_local` warning
// is about). `services.MongoConnection.mirrorDbFor` derives the SAME name on
// the Scala side — change one and you must change the other.
function mirrorDbFor(prodDb) { return prodDb + "_prod_mirror"; }
