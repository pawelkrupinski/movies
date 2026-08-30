// Decide whether a mirror database has fallen far enough behind prod that
// resuming its change stream can no longer catch it up — i.e. whether mirror.sh
// should re-seed instead of tailing.
//
// Split out from staleness.js (which does the observing) so the rule itself is
// a pure function of numbers, runnable — and asserted — with no Mongo at all:
//   mongosh --nodb --file staleness-rule.js --file staleness-rule-spec.js
// Loaded via mongosh's `--file` list BEFORE its caller, like mirror-targets.js:
// mongosh has no module system, so these are plain globals.
//
// Why this exists: the only re-seed trigger used to be "the mirror's `movies`
// is EMPTY". A tailer that crash-loops on a token it can never advance leaves a
// mirror that is non-empty and weeks out of date, which that check calls
// healthy — /debug then serves stale data indefinitely with nothing to notice.
// (Observed: a supervisor crash-looping ~1000 times while UK sat at 406 of
// prod's 1555 movies.)

// Lag is measured mirror-vs-PROD, never against wall-clock: an idle prod at
// 04:00 moves both maxima together and stays "fresh" at lag 0. 30 minutes is
// comfortably above normal tailer catch-up (seconds) and below the point where
// a stalled mirror is visibly wrong on /debug.
const STALENESS_LIMITS = { maxLagMs: 30 * 60 * 1000, maxCountDrift: 0.02 };

// observed: { prodCount, mirrorCount, prodMaxUpdatedAtMs, mirrorMaxUpdatedAtMs,
//             missingCollections, mirrorAhead, seedIncomplete } — counts from
// `movies`, the newest `updatedAt` across the collections that carry one, the
// mirrored collections prod has documents for while the mirror has none, the
// mirrored collections holding MORE documents than prod (as
// `{ name, prod, mirror, forMs }`, already confirmed to have stood for minutes),
// and whether the last seed left its unfinished mark behind. Timestamps are
// epoch millis or null (a collection with no docs, or none carrying
// `updatedAt`).
// Returns { stale, reason } — `reason` is what mirror.sh logs, so it has to
// read as an explanation on its own.
function stalenessVerdict(observed, limits) {
  const lim = limits || STALENESS_LIMITS;
  const { prodCount, mirrorCount, prodMaxUpdatedAtMs, mirrorMaxUpdatedAtMs } = observed;
  const missing = observed.missingCollections || [];

  // First, because a torn snapshot makes every signal below it lie. seed.js
  // copies collection by collection, dropping each before refilling it, so a
  // seed killed partway leaves `movies` (copied first) fresh and complete while
  // a later collection is empty or truncated — count, lag and missing-collection
  // checks all come back healthy on a mirror that is a fragment.
  if (observed.seedIncomplete)
    return { stale: true, reason: "last seed did not finish — snapshot is torn" };

  if (mirrorCount === 0) return { stale: true, reason: "mirror is empty" };

  // A collection newly added to MIRRORED_COLLECTIONS exists in prod but has
  // never been copied, and the tailer only carries CHANGES — so without this
  // the mirror stays permanently short of it and /debug renders as if the data
  // did not exist (how `movie_slots` went missing while every other signal
  // looked healthy). Re-seed is the only thing that fills it.
  if (missing.length > 0)
    return { stale: true, reason: `mirror is missing ${missing.join(", ")}` };

  // Documents the mirror holds that prod does not — the only shape that says
  // "this mirror missed DELETES" outright, and the one every other signal here
  // is blind to: a delete moves no `updatedAt`, so lag stays 0, and the drift
  // ratio below reads `movies` alone at a 2% threshold that three stray rows in
  // a 1700-film corpus never reach. Found 2026-08-29 in `pending_movies`, where
  // three folded films sat as ghosts and /debug showed them stuck in staging for
  // an hour. The observer only reports a collection here once the excess has
  // stood for MINUTES, so neither a mirror still catching up nor two counts
  // taken a round-trip apart mid-rewrite ever reaches this rule.
  const ahead = observed.mirrorAhead || [];
  if (ahead.length > 0)
    return {
      stale: true,
      reason: "mirror holds documents prod deleted: " +
        ahead.map(c => `${c.name} ${c.mirror} vs ${c.prod}` +
          (c.forMs ? ` for ${formatDuration(c.forMs)}` : "")).join(", "),
    };
  // Nothing upstream to be behind of (a country not yet scraped) — tailing is
  // the right move, and a re-seed would copy the same nothing.
  if (prodCount === 0) return { stale: false, reason: "source is empty — nothing to mirror" };

  if (prodMaxUpdatedAtMs !== null && mirrorMaxUpdatedAtMs === null)
    return { stale: true, reason: "mirror carries no timestamped documents while prod does" };

  const lagMs = prodMaxUpdatedAtMs === null || mirrorMaxUpdatedAtMs === null
    ? 0
    : Math.max(0, prodMaxUpdatedAtMs - mirrorMaxUpdatedAtMs);
  if (lagMs > lim.maxLagMs)
    return { stale: true, reason: `newest write lags prod by ${formatDuration(lagMs)}` };

  // A mirror that missed only DELETES keeps pace on `updatedAt` while its count
  // drifts up, so the two signals catch different failures. Ratio, not an
  // absolute: a handful of in-flight documents is normal churn on any corpus.
  const drift = Math.abs(prodCount - mirrorCount) / prodCount;
  if (drift > lim.maxCountDrift)
    return {
      stale: true,
      reason: `movies count off by ${Math.abs(prodCount - mirrorCount)} ` +
        `(${(drift * 100).toFixed(1)}% of prod's ${prodCount})`,
    };

  return {
    stale: false,
    reason: `fresh (lag ${formatDuration(lagMs)}, ${mirrorCount}/${prodCount} movies)`,
  };
}

function formatDuration(ms) {
  if (ms < 1000) return `${ms}ms`;
  const s = Math.round(ms / 1000);
  if (s < 60) return `${s}s`;
  const m = Math.round(s / 60);
  if (m < 60) return `${m}m`;
  const h = Math.floor(m / 60);
  return `${h}h${String(m % 60).padStart(2, "0")}m`;
}
