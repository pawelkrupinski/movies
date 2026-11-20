// Assertions for fence-rule.js -- whether the tailer holds a change while a what-if needs the mirror to stand still.
// No Mongo, no disk: the rule is a pure function of fences and timestamps.
//
//   mongosh --nodb --quiet --file fence-rule.js --file fence-rule-spec.js
//
// Exits 0 when every case passes, 1 on any failure.

const MIN = 60 * 1000;
const HOUR = 60 * MIN;
const NOW = 1_800_000_000_000;

let failures = 0;
function check(what, actual, expected) {
  if (JSON.stringify(actual) === JSON.stringify(expected)) { print(`  ok   ${what}`); return; }
  failures++;
  print(`  FAIL ${what}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
}
const secs = ms => Math.floor(ms / 1000);
const fence = (name, kind, expiryMs, writtenMs) => parseFence(name, `${secs(expiryMs)} ${kind}\n`, writtenMs === undefined ? NOW : writtenMs);

check("a fence file reads as its expiry and kind", parseFence("what-if-1", `${secs(NOW + HOUR)} run\n`, NOW),
  { name: "what-if-1", kind: "run", expiryMs: NOW + HOUR });
check("a half-written file is no fence", parseFence("what-if-1", "18000", NOW), null);
check("an expiry far past its write reads as the longest hold", fence("f", "run", NOW + 48 * HOUR).expiryMs, NOW + 4 * HOUR);

const fresh = NOW - 5000; // a change the tailer just received
check("no fence holds nothing", fenceHolding([], NOW, fresh), null);
check("a run's fence holds the change", fenceHolding([fence("what-if-1", "run", NOW + HOUR)], NOW, fresh), "what-if-1");
check("an expired fence holds nothing", fenceHolding([fence("what-if-1", "run", NOW - 1000)], NOW, fresh), null);
check("a linger fence holds the change too (the next what-if finds the same copy)",
  fenceHolding([fence("what-if-1", "linger", NOW + 20 * MIN)], NOW, fresh), "what-if-1");
check("...but not a change already held past the linger's lag: back-to-back what-ifs never freeze the mirror",
  fenceHolding([fence("what-if-1", "linger", NOW + 20 * MIN)], NOW, NOW - 20 * MIN - 1), null);
check("a run's fence holds past that lag (a run in flight is not torn)",
  fenceHolding([fence("what-if-1", "run", NOW + HOUR)], NOW, NOW - 3 * HOUR), "what-if-1");
check("...but not past the age the mirror stops being fresh",
  fenceHolding([fence("what-if-1", "run", NOW + HOUR)], NOW, NOW - 6 * HOUR - 1), null);
check("a drain fence holds nothing itself", fenceHolding([fence("what-if-2", "drain", NOW + HOUR)], NOW, fresh), null);
check("...and lifts every linger while it drains, so the backlog drains to now",
  fenceHolding([fence("what-if-1", "linger", NOW + 20 * MIN), fence("what-if-2", "drain", NOW + HOUR)], NOW, fresh), null);
check("...but never another run's fence", fenceHolding([fence("what-if-1", "run", NOW + HOUR), fence("what-if-2", "drain", NOW + HOUR)], NOW, fresh), "what-if-1");
check("an expired drain lifts nothing", fenceHolding([fence("what-if-1", "linger", NOW + 20 * MIN), fence("what-if-2", "drain", NOW - 1)], NOW, fresh), "what-if-1");
check("any one fence of several holds", fenceHolding([fence("a", "run", NOW - 1), fence("b", "run", NOW + HOUR)], NOW, fresh), "b");
check("with no change in hand (mirror.sh's audit), any unexpired fence is in force, whatever the lag",
  fenceHolding([fence("what-if-1", "linger", NOW + MIN)], NOW, null), "what-if-1");

if (failures > 0) { print(`[spec] ${failures} failure(s)`); quit(1); }
print("[spec] all cases pass");
