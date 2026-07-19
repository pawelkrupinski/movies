// What the local `/debug` mirror holds, shared by seed.js and tail.js.
//
// Loaded via mongosh's `--file` list BEFORE the script that uses it, so these
// names are plain globals — mongosh has no module system.
//
// The four collections are exactly what a /debug page load reads: the corpus
// table (`movies`, with `screenings` stitched back in for showtimes) and the
// per-row expand's two stores (`enrichment_attempts`, `rating_cadence`). Adding
// a collection here is what makes it readable at LAN latency; anything absent
// still resolves against prod over the tunnel, just slowly.
const MIRRORED_COLLECTIONS = ["movies", "screenings", "enrichment_attempts", "rating_cadence"];

// Prod's per-country databases sit side by side on the ONE local mirror
// instance, each suffixed rather than reusing prod's name. The suffix is load
// bearing: a locally-run worker pointed at the same instance defaults to the
// `kinowo` database, and without it that worker would write straight into the
// mirrored corpus (the two-database split the README's `kinowo_local` warning
// is about). `services.MongoConnection.mirrorDbFor` derives the SAME name on
// the Scala side — change one and you must change the other.
function mirrorDbFor(prodDb) { return prodDb + "_prod_mirror"; }
