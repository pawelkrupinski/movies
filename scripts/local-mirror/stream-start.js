// Decide WHERE a database's change stream should start: from the saved resume
// token, from the operation time the seed captured, or from now.
//
// Split out of tail.js so the decision is a pure function of the persisted
// `__mirror_state` document, runnable — and asserted — with no Mongo at all:
//   mongosh --nodb --file stream-start.js --file stream-start-spec.js
// Loaded via mongosh's `--file` list BEFORE its callers, like mirror-targets.js:
// mongosh has no module system, so these are plain globals.
//
// Why the seed's operation time exists at all: seed.js copies collection by
// collection and only then hands over to the tailer, so every write landing on
// an ALREADY-COPIED collection before the stream opens falls into a gap that
// nothing ever repairs. "Start from now" makes that gap permanent. Observed
// 2026-08-29: a re-seed copied `pending_movies` while three DE films were still
// incubating, the fold deleted their rows during the copy's remaining
// collections, and /debug showed three films stuck in staging that prod had
// folded an hour earlier — with `movies` count and lag both reading healthy,
// because a delete carries no `updatedAt` and staleness.js counts only `movies`.
// Starting at the time the seed BEGAN replays the copy's own window instead,
// which is harmless: every apply is idempotent (`replaceOne` upsert / `deleteOne`).
function streamStartFor(state) {
  if (state && state.resumeToken)
    return { opts: { resumeAfter: state.resumeToken }, how: "resuming after saved token" };

  if (state && state.startAtOperationTime)
    return {
      opts: { startAtOperationTime: state.startAtOperationTime },
      how: "starting at the seed's pre-copy operation time",
    };

  // No seed has run against this mirror (or its state was cleared by hand).
  // Nothing is known to be missing, so now is the only defensible start.
  return { opts: {}, how: "starting from now (no saved token)" };
}
