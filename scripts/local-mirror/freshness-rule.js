// Can a tool READING the mirror trust it? The reader's question, not mirror.sh's: staleness-rule.js decides whether
// to RE-SEED by comparing the mirror with prod; a reader (identity-slice --prod-mirror, listing-history --replay) never
// talks to prod, so it judges the mirror by its own sync moment — the cluster time of the last change the tailer
// applied — against the wall clock and against the recording it is about to mix the mirror's state with.
//
// A pure function, asserted with no Mongo:
//   mongosh --nodb --quiet --file staleness-rule.js --file freshness-rule.js --file freshness-rule-spec.js
// Loaded after staleness-rule.js (formatDuration), before freshness.js.

// The epoch millis of a change-stream resume token's cluster time, or null. `_data` is a hex KeyString whose first
// byte 0x82 tags a Timestamp, followed by its 4-byte seconds (and 4-byte increment).
function resumeTokenMs(data) {
  if (typeof data !== "string" || !/^82[0-9A-Fa-f]{16}/.test(data)) return null;
  return parseInt(data.substr(2, 8), 16) * 1000;
}

// The prod database a country lives in (`MONGODB_DB`): Poland's is the original `kinowo`.
function prodDbOf(cc) {
  const code = String(cc).toLowerCase();
  return code === "pl" ? "kinowo" : "kinowo_" + code;
}

// observed: { syncedAtMs, nowMs, maxLagMs, notBeforeMs?, seedIncomplete?, missing? } — the sync moment (null when the
// database was never tailed), the wall clock, the lag allowed, the moment the mirror must not predate (the recording's),
// whether a seed is in flight, and the collections the reader needs that the mirror holds nothing of.
// Returns { fresh, lagMs, warnings } — each warning reads as an explanation on its own.
function freshnessVerdict(observed) {
  const warnings = [];
  const { syncedAtMs, nowMs, maxLagMs, notBeforeMs } = observed;
  if (observed.seedIncomplete) warnings.push("a seed is in flight — the copy is a fragment until it finishes");
  if ((observed.missing || []).length > 0) warnings.push(`holds nothing of ${observed.missing.join(", ")}`);
  const lagMs = syncedAtMs === null || syncedAtMs === undefined ? null : Math.max(0, nowMs - syncedAtMs);
  if (lagMs === null) warnings.push("never tailed — no resume point to date it by");
  else {
    if (lagMs > maxLagMs) warnings.push(`last synced ${formatDuration(lagMs)} ago (more than ${formatDuration(maxLagMs)})`);
    if (notBeforeMs !== undefined && notBeforeMs !== null && syncedAtMs < notBeforeMs)
      warnings.push(`last synced ${formatDuration(notBeforeMs - syncedAtMs)} BEFORE the recording — the slice would mix moments`);
  }
  return { fresh: warnings.length === 0, lagMs, warnings };
}
