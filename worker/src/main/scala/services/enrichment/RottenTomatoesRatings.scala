package services.enrichment

import clients.TmdbClient
import services.movies.{CacheKey, MovieCache, MovieService}
import tools.BoundedParallel

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try}

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
  rt:    RottenTomatoesClient
) extends CacheRefresher(cache) {

  override protected def sourceName: String = "RT"

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings / MetascoreRatings:
  //   - URL already known → cheap: scrape Tomatometer, write back if changed.
  //   - URL missing       → expensive: probe RT slug variants (with year-
  //     suffix preference + English-title fallback for non-English films),
  //     write the URL, then scrape the score.
  // Per-row failures are swallowed; the next refresh tries again.
  protected def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      e.rottenTomatoesUrl.orElse(resolveAndPersistUrl(key, e)) match {
        case Some(url) => refreshScoreFromUrl(key, e, url)
        case None      => logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no URL match")
      }
    }

  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      // `apiQuery` strips accessibility-programme decoration so an "Kino
      // bez barier: Arco (AD)" row queries RT as just "Arco". Cache key
      // stays decorated so the accessibility screening keeps its own row.
      val cleanLookup = MovieService.apiQuery(key.cleanTitle)
      val linkTitle   = e.originalTitle.getOrElse(cleanLookup)
      val rtFallback  = if (linkTitle != cleanLookup) Some(cleanLookup) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      val primary = Try(rt.urlFor(linkTitle, rtFallback, year)).toOption.flatten
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
      val resolved = primary
        .orElse(englishTitle.flatMap(t => Try(rt.urlFor(t, None, year)).toOption.flatten))
        .orElse(usTitle.flatMap(t => Try(rt.urlFor(t, None, year)).toOption.flatten))

      resolved.foreach { url =>
        logger.info(s"RT: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → URL discovered $url")
        cache.putIfPresent(key, _.copy(rottenTomatoesUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Unit = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    Try(rt.scoreFor(url)).toOption.flatten match {
      case Some(score) =>
        logger.info(s"RT: $label $url → Tomatometer $score" +
          (if (e.rottenTomatoes.contains(score)) " (unchanged)" else s" (was ${e.rottenTomatoes.getOrElse("—")})"))
        if (!e.rottenTomatoes.contains(score)) cache.putIfPresent(key, _.copy(rottenTomatoes = Some(score)))
      case None =>
        logger.info(s"RT: $label $url → no Tomatometer on page")
    }
  }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `rottenTomatoesUrl` get a cheap
   *  Tomatometer refresh; rows without one get the full URL-discovery probe
   *  (and a score refresh if discovery succeeds). */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.rottenTomatoesUrl.isDefined }
    logger.info(s"RT refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → score-only, ${missingUrl.size} without → URL discovery).")
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)

    BoundedParallel.foreach("RT-refresh-score", withUrl, refreshConcurrency) { case (key, enrichment) =>
      val url = enrichment.rottenTomatoesUrl.get
      Try(rt.scoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.rottenTomatoes =>
          logger.debug(s"RT refresh: ${key.cleanTitle} $url ${enrichment.rottenTomatoes.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(rottenTomatoes = fresh))
          changed.incrementAndGet()
        case Success(_) => ()
        case Failure(ex) =>
          failed.incrementAndGet()
          logger.debug(s"RT refresh: $url lookup failed: ${ex.getMessage}")
      }
    }

    BoundedParallel.foreach("RT-refresh-discover", missingUrl, refreshConcurrency) { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered.incrementAndGet()
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"RT refresh: tick done in ${took}ms — ${changed.get} score(s) changed, " +
                s"${urlDiscovered.get} URL(s) newly discovered, ${failed.get} failed.")
  }
}
