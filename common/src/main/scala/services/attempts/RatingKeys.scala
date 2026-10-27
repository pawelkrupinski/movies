package services.attempts

import services.freshness.FreshnessKind

/**
 * The tmdbId-keyed dedup/freshness key shape, `<site>|tmdb:<id>`, in ONE place.
 *
 * Four stores key on this exact string — freshness, the task queue's dedup, the
 * rating cadence, and the enrichment-attempt log. A second spelling of the format
 * would not fail to compile on a drift; it would silently join nothing.
 */
object RatingKeys {
  def tmdbKey(kind: FreshnessKind, tmdbId: Int): String = s"${kind.label}|tmdb:$tmdbId"
}
