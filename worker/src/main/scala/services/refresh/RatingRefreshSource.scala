package services.refresh

import services.enrichment.CacheRefresher
import services.movies.MovieCache
import services.resolution.ResolutionCache
import services.tasks.{BulkRefreshResult, RefreshableSource}

/**
 * A rating source's rows refreshed for given TMDB film ids: every film row the cache holds under one of them has the
 * source's memoised resolution forgotten (`links`, as the `/tasks` button forgets them all — else the refresh re-derives
 * from the very answer it exists to re-check) and is re-checked as the source's full-corpus walk re-checks a row
 * (`CacheRefresher.recheck`): a URL-keyed source (Metacritic, RT, Filmweb) derives the row's URL again, so a wrong one
 * is replaced, then reads the score off it. Each write moves the row and so its projection, and each moved badge is
 * told to the adaptive cadence, as the full-corpus walk tells it.
 *
 * A row whose refresh throws (the site down for it, a page that will not parse) counts as failed and the rest go on, as
 * in the full-corpus walk: one row's failure ending the task left the rows after it unasked and reported nothing of
 * the ratings already moved, and the queue's retry forgot and fetched every row again.
 */
final class RatingRefreshSource(val name: String, refresher: CacheRefresher, cache: MovieCache, links: ResolutionCache)
    extends RefreshableSource {
  val keys: String = "TMDB film ids"

  def refresh(keys: Seq[String]): BulkRefreshResult = {
    val films   = keys.flatMap(_.toIntOption).toSet
    val entries = cache.entries
    val rows    = entries.filter { case (_, record) => record.tmdbId.exists(films) }
    rows.foreach { case (key, _) => links.forget(key.cleanTitle) }
    val result = refresher.recheck(rows)
    val moved  = result.changed.getOrElse(0)
    val failed = result.failed.getOrElse(0)
    val held   = entries.flatMap(_._2.tmdbId).toSet
    BulkRefreshResult.counts(walked = rows.size, changed = moved, discovered = result.discovered.getOrElse(0), failed = failed,
      message = s"${rows.size} rows of ${films.size} films refreshed, $moved ratings moved" +
        (if (failed == 0) "" else s", $failed failed") +
        (if ((films -- held).isEmpty) "" else s"; no row holds ${(films -- held).toSeq.sorted.mkString(",")}"))
  }
}
