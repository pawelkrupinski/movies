// Assertions for stream-start.js — where a tailer opens its change stream. No
// Mongo, no prod, no tunnel: the rule is a pure function, so this runs anywhere
// mongosh does.
//
//   mongosh --nodb --quiet --file stream-start.js --file stream-start-spec.js
//
// Exits 0 when every case passes, 1 on the first failure.

let failures = 0;
function check(what, state, expected) {
  const got = streamStartFor(state);
  const keys = Object.keys(got.opts).sort().join(",");
  if (keys === expected) { print(`  ok   ${what} — ${got.how}`); return; }
  failures++;
  print(`  FAIL ${what}: expected opts [${expected || "none"}], got [${keys || "none"}] (${got.how})`);
}

print("[spec] change-stream start point");

const TOKEN = { _data: "82ABCDEF" };
const TS    = { t: 1756500000, i: 1 };

check("a saved token resumes exactly where the tailer left off",
  { resumeToken: TOKEN }, "resumeAfter");

// The case the whole file exists for: a fresh seed leaves no token, and
// "from now" would silently drop every write the copy raced past.
check("a fresh seed starts at the operation time the seed captured",
  { startAtOperationTime: TS }, "startAtOperationTime");

check("a token wins over the seed's time — the token is strictly newer",
  { resumeToken: TOKEN, startAtOperationTime: TS }, "resumeAfter");

check("no state at all falls back to now", null, "");
check("an empty state document falls back to now", {}, "");

// A seed that recorded no time (an older mirror, or a source that reported
// none) must still tail rather than refuse to start.
check("state with neither field falls back to now", { somethingElse: 1 }, "");

print(failures === 0 ? "[spec] stream-start: all cases pass" : `[spec] stream-start: ${failures} FAILED`);
quit(failures === 0 ? 0 : 1);
