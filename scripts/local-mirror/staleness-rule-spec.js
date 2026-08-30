// Assertions for staleness-rule.js — the re-seed decision. No Mongo, no prod,
// no tunnel: the rule is a pure function, so this runs anywhere mongosh does.
//
//   mongosh --nodb --quiet --file staleness-rule.js --file staleness-rule-spec.js
//
// Exits 0 when every case passes, 1 on the first failure (so mirror.sh's own
// callers, and anyone editing the thresholds, get a real signal).

const MINUTE = 60 * 1000;
const NOW = 1_800_000_000_000;   // fixed epoch: the rule compares two timestamps, never `now`

let failures = 0;
function check(what, observed, expectedStale) {
  const verdict = stalenessVerdict(observed);
  if (verdict.stale === expectedStale) { print(`  ok   ${what} — ${verdict.reason}`); return; }
  failures++;
  print(`  FAIL ${what}: expected stale=${expectedStale}, got stale=${verdict.stale} (${verdict.reason})`);
}

const caughtUp = {
  prodCount: 900, mirrorCount: 900,
  prodMaxUpdatedAtMs: NOW, mirrorMaxUpdatedAtMs: NOW,
};

print("[spec] staleness rule");
check("a caught-up mirror tails", caughtUp, false);
check("seconds of catch-up lag still tails",
  { ...caughtUp, mirrorMaxUpdatedAtMs: NOW - 20 * 1000 }, false);
check("29 minutes behind still tails",
  { ...caughtUp, mirrorMaxUpdatedAtMs: NOW - 29 * MINUTE }, false);
check("31 minutes behind re-seeds",
  { ...caughtUp, mirrorMaxUpdatedAtMs: NOW - 31 * MINUTE }, true);
check("weeks behind re-seeds — the crash-looping-tailer case",
  { prodCount: 1555, mirrorCount: 406, prodMaxUpdatedAtMs: NOW, mirrorMaxUpdatedAtMs: NOW - 14 * 24 * 60 * MINUTE }, true);
check("an empty mirror re-seeds",
  { ...caughtUp, mirrorCount: 0, mirrorMaxUpdatedAtMs: null }, true);

// An idle source must not read as staleness — prod's newest write is old, but
// the mirror holds that same write, so lag is 0. This is the 04:00 false
// positive a wall-clock threshold would produce.
check("an idle prod does not re-seed",
  { ...caughtUp, prodMaxUpdatedAtMs: NOW - 6 * 60 * MINUTE, mirrorMaxUpdatedAtMs: NOW - 6 * 60 * MINUTE }, false);
check("a not-yet-scraped country does not re-seed",
  { prodCount: 0, mirrorCount: 3, prodMaxUpdatedAtMs: null, mirrorMaxUpdatedAtMs: null }, false);

// Count drift catches what the timestamp cannot: deletes carry no `updatedAt`,
// so a mirror that missed only deletions keeps pace and quietly over-reports.
check("18 of 900 missed deletes (2%) still tails",
  { ...caughtUp, mirrorCount: 918 }, false);
check("45 of 900 missed deletes (5%) re-seeds",
  { ...caughtUp, mirrorCount: 945 }, true);
check("prod ahead by more documents than tolerance re-seeds",
  { ...caughtUp, mirrorCount: 800 }, true);

// The drift ratio above reads `movies` ALONE, at a threshold three stray rows
// never reach — so a mirror holding documents prod deleted is invisible to it
// in every other collection. That is how three folded DE films sat in the
// mirror's `pending_movies` while /debug showed them stuck in staging
// (2026-08-29). Anything reaching this rule is structural: the observer only
// reports an excess that has STOOD for minutes, so a mirror still catching up
// never gets here.
check("three ghost rows in a small collection re-seed — the /debug staging case",
  { ...caughtUp, mirrorAhead: [{ name: "pending_movies", prod: 0, mirror: 3, forMs: 20 * MINUTE }] }, true);
check("a single ghost row re-seeds — no ratio to hide behind",
  { ...caughtUp, mirrorAhead: [{ name: "pending_movies", prod: 4, mirror: 5 }] }, true);
check("ghosts in more than one collection re-seed",
  { ...caughtUp, mirrorAhead: [
    { name: "pending_movies", prod: 0, mirror: 3 },
    { name: "screenings", prod: 17_800, mirror: 17_812 }] }, true);
check("no collection ahead does not re-seed",
  { ...caughtUp, mirrorAhead: [] }, false);
// The mirror trailing prod is ordinary catch-up, not a ghost: the observer only
// ever reports the ahead direction, and the rule must not invent the other one.
check("a mirror merely behind on a collection does not re-seed",
  { ...caughtUp, mirrorAhead: [] }, false);

check("a mirror with no timestamps while prod has them re-seeds",
  { ...caughtUp, mirrorMaxUpdatedAtMs: null }, true);

// A collection added to MIRRORED_COLLECTIONS is absent from every existing
// mirror, and no amount of tailing fills it — only a re-seed does.
check("a mirrored collection prod has but the mirror lacks re-seeds",
  { ...caughtUp, missingCollections: ["movie_slots"] }, true);
check("nothing missing does not re-seed",
  { ...caughtUp, missingCollections: [] }, false);

// A seed that died partway leaves a snapshot torn in ways NO other signal here
// sees: `movies` is fresh (it is copied first), so count and lag both read
// healthy, and a collection copied down to 11600 of prod's 14400 documents has
// documents — so `missingCollections` stays empty. Only the seed's own
// unfinished mark says the snapshot is a fragment.
check("a seed that never finished re-seeds",
  { ...caughtUp, seedIncomplete: true }, true);
check("a finished seed does not re-seed",
  { ...caughtUp, seedIncomplete: false }, false);

if (failures > 0) { print(`[spec] ${failures} failure(s)`); quit(1); }
print("[spec] all cases pass");
