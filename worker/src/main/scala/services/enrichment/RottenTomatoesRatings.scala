package services.enrichment

import clients.TmdbClient
import services.movies.{CacheKey, MovieCache}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.tasks.BulkRefreshResult

import scala.util.Success

/**
 * Rotten Tomatoes side of enrichment — owns BOTH:
 *   - `rottenTomatoesUrl` discovery (slug probe with year-suffix preference,
 *     cleanTitle fallback, lazy `englishTitle` fallback for non-English films).
 *   - `rottenTomatoes` (Tomatometer percentage) scrape from the resolved URL.
 *
 * Shared entry points live in [[CacheRefresher]]; the queue drives refresh
 * (`RatingHandler` per row, the operator-triggered `refreshAll` for the bulk).
 *
 * URL resolution needs TMDB data (release year, English title) that the
 * MovieRecord row alone doesn't carry — we hit `tmdb.details(tmdbId)` lazily
 * only when a row needs URL discovery.
 */
class RottenTomatoesRatings(
  cache: MovieCache,
  tmdb:  TmdbClient,
  rt:    RottenTomatoesClient,
  // Caches the RT url discovery keyed by (title, fallback, year), so the same
  // film's slug probe runs once for 24h. Passthrough by default (tests).
  rtLinkCache: ResolutionCache = ResolutionCache.passthrough,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(cache, cadenceRecorder) {

  override protected def sourceName: String = "RT"

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings / MetascoreRatings:
  //   - URL already known → cheap: scrape Tomatometer, write back if changed.
  //   - URL missing       → expensive: probe RT slug variants (with year-
  //     suffix preference + English-title fallback for non-English films),
  //     write the URL, then scrape the score.
  // Per-row failures are swallowed; the next refresh tries again.
  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      e.rottenTomatoesUrl.orElse(resolveAndPersistUrl(key, e)) match {
        case Some(url) => refreshScoreFromUrl(key, e, url)
        case None      => logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no URL match"); None
      }
    }

  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      // `apiQuery` strips accessibility-programme decoration so an "Kino
      // bez barier: Arco (AD)" row queries RT as just "Arco". Cache key
      // stays decorated so the accessibility screening keeps its own row.
      val cleanLookup = cache.normalizer.searchQuery(key.cleanTitle)
      val linkTitle   = e.originalTitle.getOrElse(cleanLookup)
      val rtFallback  = if (linkTitle != cleanLookup) Some(cleanLookup) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      // englishTitle fallback for non-English films (TMDB's en-US `title`).
      // usTitle fallback for UK/US release-title divergence (HP1 etc.) — TMDB
      // keeps the British title in the en-US locale, but the US alternative
      // title from /alternative_titles is the one RT indexes under.
      val englishTitle = details.flatMap(_.englishTitle)
        .filterNot(_.equalsIgnoreCase(linkTitle))
        .filterNot(t => rtFallback.exists(_.equalsIgnoreCase(t)))
      val usTitle = details.flatMap(_.usTitle)
        .filterNot(_.equalsIgnoreCase(linkTitle))
        .filterNot(t => rtFallback.exists(_.equalsIgnoreCase(t)))
        .filterNot(t => englishTitle.exists(_.equalsIgnoreCase(t)))
      // Cache the whole slug-probe chain keyed by the primary identity, so a
      // cache hit skips the RT HTTP probes entirely.
      val resolved = rtLinkCache.getOrResolve(ResolutionKeys.rt(linkTitle, rtFallback, year, cache.normalizer)) {
        // One attempt across all three candidate titles rather than three
        // independent ones: same ladder, same order, but the titles share a
        // fetch memo so a slug an earlier title already probed isn't probed
        // again ("The Sting" and "Sting" both end at /m/sting).
        rt.urlForAny(Seq(linkTitle) ++ englishTitle ++ usTitle, rtFallback, year)
      }

      resolved.foreach { url =>
        logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → URL discovered $url")
        cache.putIfPresent(key, _.copy(rottenTomatoesUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Option[String] = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    val fetched = rt.scoreAndYearFor(url)
    // The page we just fetched says which film it is about. When it names a year
    // the row's own year positively contradicts, this url is a DIFFERENT film's —
    // drop it (and the score that came off it) so the next tick re-resolves.
    // Re-resolution alone never fixes this: `resolveAndPersistUrl` writes only
    // when it finds a page, so a row with no RT page of its own kept scoring the
    // namesake's forever — Wanda Jakubowska's "Zaproszenie" (1986) served the
    // Tomatometer of Olivia Wilde's 2026 film. Only a POSITIVE conflict counts;
    // an undated page is not evidence and is left alone.
    if (fetched.exists { case (_, pageYear) => !MetacriticClient.yearsCompatible(key.year, pageYear) }) {
      logger.info(s"RT: $label $url → page names ${fetched.flatMap(_._2).getOrElse("?")}, not this film — dropping the URL")
      cache.putIfPresent(key, _.copy(rottenTomatoesUrl = None, rottenTomatoes = None))
      return None
    }
    fetched.flatMap(_._1) match {
      case Some(score) => persistIfMoved(key, url, "Tomatometer", e.rottenTomatoes, score, withTomatometer, badge)
      case None =>
        logger.info(s"RT: $label $url → no Tomatometer on page")
        None
    }
  }

  private def withTomatometer(row: models.MovieRecord, score: Option[Int]): models.MovieRecord = row.copy(rottenTomatoes = score)
  private def badge(score: Int): String = s"$score%"

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row: re-resolve the URL of every row with a tmdbId, then
   *  refresh the Tomatometer off whatever URL the row holds. */
  private[services] def refreshAll(): BulkRefreshResult =
    refreshAllUrlThenScore[Int](
      walkLabel     = "RT refresh",
      urlOf         = _.rottenTomatoesUrl,
      scoreOf       = _.rottenTomatoes,
      rediscoverUrl = (key, row) => Success(resolveAndPersistUrl(key, row).isDefined),
      fetchScore    = rt.scoreFor,
      withScore     = withTomatometer,
      badge         = badge
    )
}
