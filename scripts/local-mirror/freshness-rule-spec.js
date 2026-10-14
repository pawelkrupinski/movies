// Assertions for freshness-rule.js — whether a tool READING the mirror (identity-slice --prod-mirror,
// listing-history) can trust it. No Mongo: the rule is a pure function of timestamps.
//
//   mongosh --nodb --quiet --file staleness-rule.js --file freshness-rule.js --file freshness-rule-spec.js
//
// Exits 0 when every case passes, 1 on the first failure.

const HOUR = 60 * 60 * 1000;
const NOW = 1_800_000_000_000;

let failures = 0;
function check(what, actual, expected) {
  if (JSON.stringify(actual) === JSON.stringify(expected)) { print(`  ok   ${what}`); return; }
  failures++;
  print(`  FAIL ${what}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
}
function fresh(what, observed, expected) {
  const verdict = freshnessVerdict(observed);
  check(what + (verdict.warnings.length ? ` — ${verdict.warnings.join("; ")}` : ""), verdict.fresh, expected);
}

print("[spec] freshness rule");

// A change-stream resume token's `_data` opens with 0x82 and the event's cluster time: 4 bytes of seconds, 4 of
// increment. The token the tailer saves is the last change it APPLIED, so its time is when the mirror last caught a
// write — the sync moment, with nothing to ask of prod.
check("a resume token's cluster time", resumeTokenMs("826ACE076B0000005D2B042C0100"), 0x6ACE076B * 1000);
check("a token of another shape reads as unknown", resumeTokenMs("01ABCD"), null);
check("no token reads as unknown", resumeTokenMs(undefined), null);

const synced = { syncedAtMs: NOW - 5 * 60 * 1000, nowMs: NOW, maxLagMs: 6 * HOUR };
fresh("a mirror minutes behind is fresh", synced, true);
fresh("a day behind is not", { ...synced, syncedAtMs: NOW - 26 * HOUR }, false);
fresh("just inside the allowed lag is fresh", { ...synced, syncedAtMs: NOW - 6 * HOUR + 1 }, true);
// The recording the slice replays and the mirror's answers must be one moment: a mirror last synced BEFORE the
// recording was made answers with state older than the listings it decides.
fresh("synced before the recording mixes moments", { ...synced, notBeforeMs: NOW - 60 * 1000 }, false);
fresh("synced after the recording is fresh", { ...synced, notBeforeMs: NOW - 2 * HOUR }, true);
fresh("never tailed (no resume point) is not fresh", { ...synced, syncedAtMs: null }, false);
// A seed in flight drops each collection before refilling it: the copy is a fragment until its mark clears.
fresh("a seed in flight is not fresh", { ...synced, seedIncomplete: true }, false);
fresh("a collection the reader needs that the mirror lacks is not fresh", { ...synced, missing: ["identity_agreements"] }, false);
check("the lag is reported", freshnessVerdict(synced).lagMs, 5 * 60 * 1000);

check("cc pl is prod's kinowo", prodDbOf("pl"), "kinowo");
check("any other cc is kinowo_<cc>", prodDbOf("UK"), "kinowo_uk");

if (failures > 0) { print(`[spec] ${failures} failure(s)`); quit(1); }
print("[spec] all cases pass");
