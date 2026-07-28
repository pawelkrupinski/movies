// Assertions for targets-fingerprint.js — the "did the mirrored-collection list
// change?" digest. No Mongo, no prod, no tunnel: the digest is a pure function,
// so this runs anywhere mongosh does.
//
//   mongosh --nodb --quiet --eval 'var FINGERPRINT_SILENT=1' \
//     --file targets-fingerprint.js --file targets-fingerprint-spec.js
//
// Exits 0 when every case passes, 1 on the first failure.

let failures = 0;
function check(what, condition) {
  if (condition) { print(`  ok   ${what}`); return; }
  failures++;
  print(`  FAIL ${what}`);
}

const base = ["movies", "screenings", "movie_slots"];
const digest = collectionsFingerprint;

// The property mirror.sh depends on: same set ⇒ same digest, every time. If this
// ever wobbles, every tailer restarts on every poll.
check("is stable across calls", digest(base) === digest(base));

// Reordering the literal is not a change to what gets mirrored — restarting
// every tailer over it would be pure churn.
check("ignores order", digest(base) === digest(["screenings", "movie_slots", "movies"]));

// Nor is listing a name twice.
check("ignores duplicates", digest(base) === digest(base.concat("movies")));

// The case this exists for: adding a collection MUST be seen, or the running
// tailer keeps filtering the change stream on the old list and the new
// collection is seeded once and then never kept up to date.
check("sees an added collection", digest(base) !== digest(base.concat("cinema_scrapes")));
check("sees a removed collection", digest(base) !== digest(["movies", "screenings"]));
check("sees a renamed collection", digest(base) !== digest(["movies", "screenings", "film_slots"]));

// Names that differ only by where the split falls must not collide — the digest
// joins on a separator for exactly this reason.
check("does not collide on join boundaries", digest(["ab", "c"]) !== digest(["a", "bc"]));

// An empty list is a legitimate (if useless) configuration; it must produce a
// digest rather than something mirror.sh could confuse with a failed read.
check("digests an empty list to a non-empty string", digest([]).length > 0);
check("distinguishes empty from populated", digest([]) !== digest(base));

// mirror.sh treats an EMPTY string as "could not read the list" and skips the
// comparison, so no real digest may ever be empty.
check("never returns an empty string", [base, [], ["x"]].every(c => digest(c) !== ""));

if (failures > 0) { print(`[spec] ${failures} failure(s)`); quit(1); }
print("[spec] all cases pass");
