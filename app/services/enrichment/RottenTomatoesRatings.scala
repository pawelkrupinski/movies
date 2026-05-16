package services.enrichment

import clients.TmdbClient
import services.events.{DomainEvent, ImdbIdMissing, TmdbResolved}
import services.movies.{CacheKey, MovieCache}

import scala.util.{Failure, Success, Try}

/**
 * Rotten Tomatoes side of enrichment — owns BOTH:
 *   - `rottenTomatoesUrl` discovery (slug probe with year-suffix preference,
 *     cleanTitle fallback, lazy `englishTitle` fallback for non-English films).
 *   - `rottenTomatoes` (Tomatometer percentage) scrape from the resolved URL.
 *
 * Shared lifecycle + worker plumbing lives in [[PeriodicCacheRefresher]].
 *
 * URL resolution needs TMDB data (release year, English title) that the
 * MovieRecord row alone doesn't carry — we hit `tmdb.details(tmdbId)` lazily
 * only when a row needs URL discovery.
 */
class RottenTomatoesRatings(
  cache: MovieCache,
  tmdb:  TmdbClient,
  rt:    RottenTomatoesClient
) extends PeriodicCacheRefresher(
  name                = "RT",
  // 5 workers comfortably under CLAUDE.md's "5–10" band for undocumented
  // services. Each refresh is a single GET to a /m/ page.
  workers             = 5,
  // Stagger startup against IMDb (10s) so first-tick bursts don't pile up.
  startupDelaySeconds = 15L,
  refreshHours        = 1L,
  cache               = cache
) {

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: discover the RT URL (if missing) and refresh the Tomatometer
   *  as soon as the TMDB stage produces a row. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  /** Sibling listener: fire on `ImdbIdMissing` too. The TMDB stage publishes
   *  this when TMDB resolved the film but had no IMDb cross-reference yet
   *  (common for very recent Polish releases). The RT URL + score don't
   *  depend on the IMDb id, so we want to refresh on either signal. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings / MetascoreRatings:
  //   - URL already known → cheap: scrape Tomatometer, write back if changed.
  //   - URL missing       → expensive: probe RT slug variants (with year-
  //     suffix preference + English-title fallback for non-English films),
  //     write the URL, then scrape the score.
  // Per-row failures are swallowed; the next periodic tick tries again.
  protected def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      val urlOpt = e.rottenTomatoesUrl.orElse(resolveAndPersistUrl(key, e))
      urlOpt.foreach(url => refreshScoreFromUrl(key, e, url))
    }

  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      val linkTitle  = e.originalTitle.getOrElse(key.cleanTitle)
      val rtFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
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
        logger.debug(s"RT: ${key.cleanTitle} discovered $url")
        cache.putIfPresent(key, _.copy(rottenTomatoesUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Unit =
    Try(rt.scoreFor(url)).toOption.flatten match {
      case Some(score) if !e.rottenTomatoes.contains(score) =>
        cache.putIfPresent(key, current => {
          logger.debug(s"RT: ${key.cleanTitle} $url ${current.rottenTomatoes.getOrElse("—")} → $score")
          current.copy(rottenTomatoes = Some(score))
        })
      case _ => ()
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `rottenTomatoesUrl` get a cheap
   *  Tomatometer refresh; rows without one get the full URL-discovery probe
   *  (and a score refresh if discovery succeeds). */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.rottenTomatoesUrl.isDefined }
    logger.info(s"RT refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → score-only, ${missingUrl.size} without → URL discovery).")
    var changed       = 0
    var failed        = 0
    var urlDiscovered = 0

    withUrl.foreach { case (key, enrichment) =>
      val url = enrichment.rottenTomatoesUrl.get
      Try(rt.scoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.rottenTomatoes =>
          logger.debug(s"RT refresh: ${key.cleanTitle} $url ${enrichment.rottenTomatoes.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(rottenTomatoes = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"RT refresh: $url lookup failed: ${ex.getMessage}")
      }
    }

    missingUrl.foreach { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered += 1
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"RT refresh: tick done in ${took}ms — $changed score(s) changed, " +
                s"$urlDiscovered URL(s) newly discovered, $failed failed.")
  }
}
