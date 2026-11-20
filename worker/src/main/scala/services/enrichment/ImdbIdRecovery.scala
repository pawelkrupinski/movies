package services.enrichment

import models.CacheKey
import models.MovieRecord
import play.api.Logging
import services.evidence.LatestTitleYear
import services.movies.MovieCache

/**
 * Recovers one row's missing IMDb id: the id the ladder names ([[ImdbIdLadder]]), written back to the cached row —
 * from where the `EnrichmentReaper` picks up the now-eligible IMDb rating on its next pass (no rating event is fired).
 * The id write is the only effect.
 *
 * No-op when the row already carries an imdbId (a stale event raced with another resolver), has no TMDB id (its IMDb id
 * is the identity resolver's fallback source's to give), or when the search returns nothing — we'd rather leave the row
 * imdbId-less than guess a wrong id. A lookup that FAILED throws ([[ImdbIdResolver]] retries it).
 */
final class ImdbIdRecovery(cache: MovieCache, ladder: ImdbIdLadder)(using LatestTitleYear) extends Logging {

  def resolve(title: String, year: Option[Int], searchTitle: String): Unit = tools.HttpPath.within(tools.HttpPath.Enrich) {
    val key = cache.keyOf(title, year)
    // Only a TMDB film TMDB gave no IMDb id: a film TMDB has no record of takes its IMDb id from the identity
    // resolver's fallback source (`ResolverDecision.fallback`), on every fact it publishes, never from a title search.
    cache.get(key).filter(record => record.imdbId.isEmpty && record.tmdbId.isDefined).foreach { record =>
      logger.info(s"IMDb-id: looking up '${key.cleanTitle}' (${key.year.getOrElse("?")}) [search='$searchTitle']")
      ladder.lookupId(ImdbIdQuery.of(searchTitle, year, record)) match {
        case Some(id) if heldByAnotherFilm(id, key, record) =>
          logger.warn(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → $id refused: another film " +
            s"already holds it [search='$searchTitle']")
        case Some(id) =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → resolved $id")
          // putIfPresent so a concurrent `cache.invalidate` between the lookup and
          // the write-back can't resurrect the row; and only over the id the search
          // started from (none), so an id another writer (TMDB's resolution) landed while the
          // search was out stays.
          cache.putIfPresent(key, _.withImdbIdIfMissing(id))
        case None =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match [search='$searchTitle']")
      }
    }
  }

  /** Whether a row other than `key`'s — and not the same TMDB film — already carries `id`. Two
   *  films sharing an imdbId show one's ratings on the other, so the second never takes it. */
  private def heldByAnotherFilm(id: String, key: CacheKey, record: MovieRecord): Boolean =
    cache.entries.exists { case (other, row) =>
      other != key && row.imdbId.contains(id) && !(record.tmdbId.isDefined && row.tmdbId == record.tmdbId)
    }
}
