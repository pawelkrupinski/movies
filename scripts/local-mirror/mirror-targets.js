// What the local `/debug` mirror holds, shared by seed.js and tail.js.
//
// Loaded via mongosh's `--file` list BEFORE the script that uses it, so these
// names are plain globals — mongosh has no module system.
//
// The list itself lives in ONE place, `common/src/main/resources/debug-mirror-collections.json`,
// with each collection's rationale beside it: `services.DebugMirror.Collections` reads the
// same file, so a collection is added in one edit and the Scala readers and this sync
// cannot disagree. This script re-reads the file every time it runs, so a RUNNING mirror
// notices an edit (targets-fingerprint.js digests the names; mirror.sh polls it).
//
// Adding a collection there is what makes it readable at LAN latency; anything
// absent reads as permanently EMPTY, NOT slowly-from-prod — the /debug country
// stacks read the mirror unconditionally, with no fall-back to the tunnel.
// `MongoConnectionSpec` and `MirroredStoresSpec` hold the stores' collection names to it.
//
// A collection added there is MISSING from every existing mirror until it is
// re-seeded — which is why staleness.js treats "prod has documents, the mirror
// has none of that collection" as stale, so the next cycle heals it by itself.
//
// An unreadable or malformed file THROWS: every caller then prints nothing, which
// mirror.sh reads as "don't know", never as an empty list.
const MIRRORED_COLLECTIONS_FILE = __dirname + "/../../common/src/main/resources/debug-mirror-collections.json";
const MIRRORED_COLLECTIONS = JSON.parse(require("fs").readFileSync(MIRRORED_COLLECTIONS_FILE, "utf8"))
  .collections.map(c => c.name);

// Prod's per-country databases sit side by side on the ONE local mirror
// instance, each suffixed rather than reusing prod's name. The suffix is load
// bearing: a locally-run worker pointed at the same instance defaults to the
// `kinowo` database, and without it that worker would write straight into the
// mirrored corpus (the two-database split the README's `kinowo_local` warning
// is about). `services.MongoConnection.mirrorDbFor` derives the SAME name on
// the Scala side — change one and you must change the other.
function mirrorDbFor(prodDb) { return prodDb + "_prod_mirror"; }
