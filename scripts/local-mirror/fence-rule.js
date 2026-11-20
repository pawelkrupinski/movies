// THE WHAT-IF FENCE: whether the tailer may apply its next change to a mirror database, or must hold it while a reader
// needs the copy to stand still. scripts/identity-what-if resolves its base and its head over the mirror's CURRENT
// listings minutes apart; a change tailed between the two had each side read another corpus (2026-11-20, lane A3: a DE
// what-if moved a take that its own base-vs-base control moved too, and a rerun moved none). So a what-if, for its run,
// writes a fence file `<FENCE_DIR>/<prod db>/<holder>` reading `<expiry epoch seconds> <run|linger>`, and tail.js holds
// every change while a fence holds it (mirror.sh skips its re-seed audit then too); a run's fence turns into a `linger`
// one when it ends, so the next what-if within the linger finds the copy -- and the base side it kept -- unchanged.
//
// A fence holds the change while it has not expired, but never past a lag: a `run` fence for at most runMaxLagMs
// (the age past which the mirror is not fresh: scripts/local-mirror/freshness; one run is never torn), a `linger`
// fence for at most lingerMaxLagMs, 20 min -- the age at which identity-what-if, before it fences a new run, waits
// for every other run to end and lets the tail drain its backlog (its mirror_fence). So back-to-back what-ifs never
// hold the mirror further behind than that lag plus the longest run in flight. A change that goes through anyway moves
// the tail position, which the what-if refuses its run on (exit 2), so a fence that did not hold is never a wrong
// answer, only a refused one. An expiry further than MAX_HOLD_MS past its file's write reads as that far (a bad clock
// or a hand-written file freezes nothing for long).
//
// A pure rule, asserted with no Mongo (rules-test.sh):
//   mongosh --nodb --quiet --file fence-rule.js --file fence-rule-spec.js
// Loaded before tail.js and fence-check.js; `fencesIn` alone reads the disk.

const FENCE_LIMITS = { maxHoldMs: 4 * 3600 * 1000, runMaxLagMs: 6 * 3600 * 1000, lingerMaxLagMs: 20 * 60 * 1000 };

// One fence file's text -> { name, expiryMs, kind }, or null for a file that is no fence (half-written, foreign).
function parseFence(name, text, writtenMs) {
  const m = /^\s*(\d+)\s+(run|linger)\s*$/.exec(String(text));
  if (!m) return null;
  return { name, kind: m[2], expiryMs: Math.min(Number(m[1]) * 1000, writtenMs + FENCE_LIMITS.maxHoldMs) };
}

// The name of the fence holding a change of cluster time `eventMs` at `nowMs`, or null when none does. `eventMs` null
// (mirror.sh's audit: no change in hand) asks whether any fence is in force, whatever the lag.
function fenceHolding(fences, nowMs, eventMs) {
  for (const f of fences) {
    if (!f || f.expiryMs <= nowMs) continue;
    if (eventMs !== null && eventMs !== undefined) {
      const lag = nowMs - eventMs;
      if (lag > (f.kind === "linger" ? FENCE_LIMITS.lingerMaxLagMs : FENCE_LIMITS.runMaxLagMs)) continue;
    }
    return f.name;
  }
  return null;
}

// The fences of one prod database: `<dir>/<prodDb>/*` (none when the directory is absent). Reads the disk (mongosh's
// `require`); the specs never call it.
function fencesIn(dir, prodDb) {
  if (!dir) return [];
  const fs = require("fs");
  const at = dir + "/" + prodDb;
  let names;
  try { names = fs.readdirSync(at); } catch (e) { return []; }
  return names.map(n => {
    try { const p = at + "/" + n; return parseFence(n, fs.readFileSync(p, "utf8"), fs.statSync(p).mtimeMs); }
    catch (e) { return null; } // removed between the listing and the read
  }).filter(f => f !== null);
}
