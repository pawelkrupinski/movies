package services.refresh

import services.enrichment.CacheRefresher
import services.movies.MovieCache
import services.resolution.ResolutionCache
import services.tasks.BulkRefreshResult

/**
 * A rating source's rows refreshed for given TMDB film ids: every film row the cache holds under one of them has the
 * source's memoised resolution forgotten (`links`, as the `/tasks` button forgets them all — else the refresh re-derives
 * from the very answer it exists to re-check) and its rating fetched again through the source's own per-row path
 * (`CacheRefresher.refreshOneSync`, the queue's `RatingHandler` path), whose write moves the row and so its projection.
 */
final class RatingRefreshSource(val name: String, refresher: CacheRefresher, cache: MovieCache, links: ResolutionCache)
    extends RefreshableSource {
  val keys: String = "TMDB film ids"

  def refresh(keys: Seq[String]): BulkRefreshResult = {
    val films = keys.flatMap(_.toIntOption).toSet
    val rows  = cache.entries.filter { case (_, record) => record.tmdbId.exists(films) }.map(_._1)
    val moved = rows.count { key => links.forget(key.cleanTitle); refresher.refreshOneSync(key).isDefined }
    val held  = cache.entries.flatMap(_._2.tmdbId).toSet
    BulkRefreshResult.counts(walked = rows.size, changed = moved, discovered = 0, failed = 0,
      message = s"${rows.size} rows of ${films.size} films refreshed, $moved ratings moved" +
        (if ((films -- held).isEmpty) "" else s"; no row holds ${(films -- held).toSeq.sorted.mkString(",")}"))
  }
}
