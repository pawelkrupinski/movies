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
      val titles = RatingSiteTitles.derive(key, e, tmdb.details(tmdbId), cache.normalizer)
      // Cache the whole slug-probe chain keyed by the primary identity, so a
      // cache hit skips the RT HTTP probes entirely.
      val resolved = rtLinkCache.getOrResolve(ResolutionKeys.rt(titles.linkTitle, titles.fallback, titles.year, cache.normalizer)) {
        // One attempt across all the candidate titles rather than one each:
        // same ladder, same order, but the titles share a fetch memo so a slug
        // an earlier title already probed isn't probed again ("The Sting" and
        // "Sting" both end at /m/sting).
        rt.urlForAny(titles.candidates, titles.fallback, titles.year, RatingPageIdentity.directorsOf(e, tmdb.directorsFor))
      }

      resolved.foreach { url =>
        logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → URL discovered $url")
        cache.putIfPresent(key, _.copy(rottenTomatoesUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Option[String] =
    tomatometerIfThisFilm(key, url).flatMap {
      case Some(score) => persistIfMoved(key, url, "Tomatometer", e.rottenTomatoes, score, withTomatometer, badge)
      case None =>
        logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) $url → no Tomatometer on page")
        None
    }

  /** The Tomatometer on `url`'s page — `Some(None)` when it carries none — or
   *  `None` when the page is a DIFFERENT film's, in which case the url and the
   *  score that came off it are dropped here so the next tick re-resolves.
   *
   *  The page says which film it is about: when it names a year the row's own
   *  year positively contradicts, the url is another film's. Re-resolution alone
   *  never fixes this — `resolveAndPersistUrl` writes only when it finds a page,
   *  so a row with no RT page of its own kept scoring the namesake's forever:
   *  Wanda Jakubowska's "Zaproszenie" (1986) served the Tomatometer of Olivia
   *  Wilde's 2026 film. Only a POSITIVE conflict counts; an undated page is not
   *  evidence and is left alone. Both the per-row refresh and the bulk walk ask
   *  this, so neither can re-score a url the other would drop. */
  private def tomatometerIfThisFilm(key: CacheKey, url: String): Option[Option[Int]] = {
    val fetched = rt.pageFor(url)
    val deniedBy = fetched.flatMap { page =>
      if (!MetacriticClient.yearsCompatible(key.year, page.year)) Some(s"names ${page.year.getOrElse("?")}")
      // An UNDATED page is no evidence on year, and RT leaves many undated — so the
      // year guard waved through /m/sacrifice (Umberto Lenzi's 1972 "Sacrifice!") for
      // Romain Gavras's 2026 "Sacrifice" ("Bogaci i martwi"). Its credit is not silent.
      else if (cache.get(key).exists(RatingPageIdentity.directorDenies(_, page.directors, tmdb.directorsFor)))
        Some(s"credits ${page.directors.mkString(", ")}")
      else None
    }
    deniedBy match {
      case Some(why) =>
        logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) $url → page $why, not this film — dropping the URL")
        cache.putIfPresent(key, _.copy(rottenTomatoesUrl = None, rottenTomatoes = None))
        None
      case None => Some(fetched.flatMap(_.score))
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
      fetchScore    = tomatometerIfThisFilm,
      withScore     = withTomatometer,
      badge         = badge
    )
}
