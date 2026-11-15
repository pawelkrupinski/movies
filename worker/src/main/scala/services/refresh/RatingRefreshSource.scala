package services.refresh

import services.enrichment.CacheRefresher
import services.movies.MovieCache
import services.resolution.ResolutionCache
import services.tasks.BulkRefreshResult
import play.api.Logging

import scala.util.{Failure, Success, Try}

/**
 * A rating source's rows refreshed for given TMDB film ids: every film row the cache holds under one of them has the
 * source's memoised resolution forgotten (`links`, as the `/tasks` button forgets them all — else the refresh re-derives
 * from the very answer it exists to re-check) and its rating fetched again through the source's own per-row path
 * (`CacheRefresher.refreshOneReported`, the queue's `RatingHandler` path), whose write moves the row and so its projection, and
 * whose moved badge the adaptive cadence is told of, as the full-corpus walk tells it.
 *
 * A row whose refresh throws (the site down for it, a page that will not parse) counts as failed and the rest go on, as
 * in the full-corpus walk: one row's failure ending the task left the rows after it unasked and reported nothing of
 * the ratings already moved, and the queue's retry forgot and fetched every row again.
 */
final class RatingRefreshSource(val name: String, refresher: CacheRefresher, cache: MovieCache, links: ResolutionCache)
    extends RefreshableSource with Logging {
  val keys: String = "TMDB film ids"

  def refresh(keys: Seq[String]): BulkRefreshResult = {
    val films = keys.flatMap(_.toIntOption).toSet
    val entries  = cache.entries
    val rows     = entries.filter { case (_, record) => record.tmdbId.exists(films) }
    val outcomes = rows.map { case (key, record) =>
      links.forget(key.cleanTitle)
      Try(refresher.refreshOneReported(key, record.tmdbId)) match {
        case Success(moved) => Some(moved.isDefined)
        case Failure(exception) =>
          logger.warn(s"refresh $name: '${key.cleanTitle}' (${key.year.getOrElse("?")}) failed: ${exception.getMessage}")
          None
      }
    }
    val moved  = outcomes.count(_.contains(true))
    val failed = outcomes.count(_.isEmpty)
    val held   = entries.flatMap(_._2.tmdbId).toSet
    BulkRefreshResult.counts(walked = rows.size, changed = moved, discovered = 0, failed = failed,
      message = s"${rows.size} rows of ${films.size} films refreshed, $moved ratings moved" +
        (if (failed == 0) "" else s", $failed failed") +
        (if ((films -- held).isEmpty) "" else s"; no row holds ${(films -- held).toSeq.sorted.mkString(",")}"))
  }
}
