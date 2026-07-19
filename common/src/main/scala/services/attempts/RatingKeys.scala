package services.attempts

import services.freshness.FreshnessKind

/**
 * The tmdbId-keyed dedup/freshness key shape, `<site>|tmdb:<id>`, in ONE place.
 *
 * Four stores key on this exact string — freshness, the task queue's dedup, the
 * rating cadence, and the enrichment-attempt log — and the /debug page joins all
 * four on it. It used to be spelled only inside `services.tasks.RatingTasks`,
 * which the WEB module can't see (it's worker-side), so surfacing the join on the
 * debug page would have meant a second copy of the format. A drift between the
 * two would not fail to compile; it would silently show an empty history.
 */
object RatingKeys {
  def tmdbKey(kind: FreshnessKind, tmdbId: Int): String = s"${kind.label}|tmdb:$tmdbId"
}
