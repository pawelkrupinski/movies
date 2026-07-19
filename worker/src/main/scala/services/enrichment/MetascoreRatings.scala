package services.enrichment

import clients.TmdbClient
import services.movies.{CacheKey, MovieCache, MovieService}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.tasks.BulkRefreshResult
import tools.BoundedParallel

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try}

/**
 * Metacritic side of enrichment — owns BOTH:
 *   - `metacriticUrl` discovery (slug probe + cleanTitle fallback + lazy
 *     `englishTitle` fallback for non-English films).
 *   - `metascore` scrape from the resolved URL.
 *
 * Shared entry points live in [[CacheRefresher]]; the queue drives refresh
 * (`RatingHandler` per row, the operator-triggered `refreshAll` for the bulk).
 *
 * URL resolution needs TMDB data (release year, English title) that the
 * MovieRecord row alone doesn't carry — we hit `tmdb.details(tmdbId)` lazily
 * for rows that need URL discovery, and never for rows that already have a
 * canonical URL.
 */
class MetascoreRatings(
  cache:      MovieCache,
  tmdb:       TmdbClient,
  metacritic: MetacriticClient,
  // Caches the MC url discovery keyed by (title, fallback, year), so the same
  // film's slug probe runs once for 24h. Passthrough by default (tests).
  mcLinkCache: ResolutionCache = ResolutionCache.passthrough,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(cache, cadenceRecorder) {

  override protected def sourceName: String = "Metacritic"

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings:
  //   - URL already known → cheap: scrape metascore, write back if changed.
  //   - URL missing       → expensive: probe MC slug variants (with TMDB
  //     details for English title + year disambiguation), write the URL,
  //     then settle the metascore.
  // Per-row failures are swallowed; the next refresh tries again.
  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      e.metacriticUrl match {
        case Some(url) => refreshScoreFromUrl(key, e, url)
        case None =>
          resolveAndPersistUrl(key, e) match {
            case Some(resolved) => settleScore(key, resolved)
            case None           => logger.info(s"Metacritic: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no URL match"); None
          }
      }
    }

  // Probe MC for a canonical /movie/ URL using TMDB's title data; write it
  // back to the cache when found. Returns the resolution (URL + any Metascore
  // the slug-probe fetch already parsed off the page), or None if MC didn't
  // index the film.
  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[MetacriticClient.Resolved] =
    e.tmdbId.flatMap { tmdbId =>
      // `apiQuery` strips accessibility-programme decoration so an "Kino
      // bez barier: Arco (AD)" row queries MC as just "Arco". Cache key
      // stays decorated so the accessibility screening keeps its own row.
      val cleanLookup = MovieService.searchQuery(key.cleanTitle)
      val linkTitle   = e.originalTitle.getOrElse(cleanLookup)
      val mcFallback  = if (linkTitle != cleanLookup) Some(cleanLookup) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      // englishTitle fallback for non-English films (TMDB's en-US `title`).
      // usTitle fallback for UK/US release-title divergence (HP1 etc.) — TMDB
      // keeps the British title in the en-US locale, but the US alternative
      // title from /alternative_titles is the one MC indexes under.
      val englishTitle = details.flatMap(_.englishTitle)
        .filterNot(_.equalsIgnoreCase(linkTitle))
        .filterNot(t => mcFallback.exists(_.equalsIgnoreCase(t)))
      val usTitle = details.flatMap(_.usTitle)
        .filterNot(_.equalsIgnoreCase(linkTitle))
        .filterNot(t => mcFallback.exists(_.equalsIgnoreCase(t)))
        .filterNot(t => englishTitle.exists(_.equalsIgnoreCase(t)))
      // The link cache stores the URL only (the score is read fresh each
      // refresh). We thread the Metascore the resolving probe already parsed
      // out-of-band via `freshScore`: it's set ONLY when the resolve closure
      // actually ran — i.e. on a cache MISS, where we just fetched the movie
      // page and can hand the score back so the caller skips a redundant
      // re-fetch. On a cache HIT the closure is skipped, `freshScore` stays
      // None, and the caller reads the score via `metascoreFor`.
      var freshScore: Option[Int] = None
      val url = mcLinkCache.getOrResolve(ResolutionKeys.mc(linkTitle, mcFallback, year)) {
        val resolved = Try(metacritic.resolve(linkTitle, mcFallback, year)).toOption.flatten
          .orElse(englishTitle.flatMap(t => Try(metacritic.resolve(t, None, year)).toOption.flatten))
          .orElse(usTitle.flatMap(t => Try(metacritic.resolve(t, None, year)).toOption.flatten))
        freshScore = resolved.flatMap(_.metascore)
        resolved.map(_.url)
      }

      url.map { u =>
        logger.info(s"Metacritic: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → URL discovered $u")
        cache.putIfPresent(key, _.copy(metacriticUrl = Some(u)))
        MetacriticClient.Resolved(u, freshScore)
      }
    }

  // Settle a freshly-discovered row's score: use the Metascore the slug probe
  // already parsed when present (no extra fetch), else read it from the URL.
  // Returns the new displayed metascore if it changed, else None.
  private def settleScore(key: CacheKey, resolved: MetacriticClient.Resolved): Option[String] =
    resolved.metascore match {
      case Some(score) => cache.get(key).flatMap(applyScore(key, _, resolved.url, score))
      // No score off the probe (search-fallback URL, link-cache hit, or a page
      // with no aggregateRating) — read it from the resolved URL. Re-read the
      // post-write row so the score copy doesn't clobber the URL we just wrote.
      case None        => cache.get(key).flatMap(refreshScoreFromUrl(key, _, resolved.url))
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Option[String] =
    Try(metacritic.metascoreFor(url)).toOption.flatten match {
      case Some(score) => applyScore(key, e, url, score)
      case None        => logger.info(s"Metacritic: '${key.cleanTitle}' (${key.year.getOrElse("?")}) $url → no metascore on page"); None
    }

  // Write a known score back, skipping the no-op when it's unchanged; returns the
  // new displayed value (the badge text) iff it changed. The updater receives the
  // live cached row — that's the merge point for both the URL we may have just
  // written and any other listener's concurrent updates.
  private def applyScore(key: CacheKey, e: models.MovieRecord, url: String, score: Int): Option[String] = {
    val label  = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    val commit = !e.metascore.contains(score)
    logger.info(s"Metacritic: $label $url → metascore $score" +
      (if (commit) s" (was ${e.metascore.getOrElse("—")})" else " (unchanged)"))
    if (commit) { cache.putIfPresent(key, _.copy(metascore = Some(score))); Some(score.toString) }
    else None
  }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `metacriticUrl` get a cheap score
   *  refresh; rows without one get the full URL-discovery probe (and then a
   *  score refresh if discovery succeeds). Per-row failures are logged at
   *  debug — one bad row can't poison the whole tick. */
  private[services] def refreshAll(): BulkRefreshResult = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val resolvable = snapshot.count { case (_, e) => e.tmdbId.isDefined }
    logger.info(s"Metascore refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"($resolvable re-resolving their URL first).")
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)

    // ONE pass, two steps per row. It used to be two passes split on whether the
    // row already had a URL, which made a stored URL permanently authoritative:
    // the operator's button could only re-scrape the score off whatever was
    // there, so a WRONG URL was never corrected and the run reported "0 changed"
    // while films sat on another film's page.
    //
    // Both steps run, deliberately. Re-resolving ALONE would be a regression:
    // a row whose re-resolution fails (transient, or MC genuinely has no page)
    // would stop refreshing its score at all — which is what two specs caught.
    BoundedParallel.foreach("Metascore-refresh", snapshot, refreshConcurrency) { case (key, enrichment) =>
      // 1. Re-derive the URL when the row has a tmdbId to derive it from. A
      //    better match replaces the stored one; a failure leaves it be.
      if (enrichment.tmdbId.isDefined && resolveAndPersistUrl(key, enrichment).isDefined) urlDiscovered.incrementAndGet()

      // 2. Refresh the score off whatever URL the row NOW holds — possibly the
      //    one just re-resolved, possibly the pre-existing one.
      val current = cache.get(key).getOrElse(enrichment)
      current.metacriticUrl.foreach { url =>
        Try(metacritic.metascoreFor(url)) match {
          case Success(fresh) if fresh != current.metascore =>
            logger.debug(s"Metascore refresh: ${key.cleanTitle} $url ${current.metascore.getOrElse("—")} → ${fresh.getOrElse("—")}")
            cache.putIfPresent(key, _.copy(metascore = fresh))
            fresh.foreach(s => recordCadenceChange(key, enrichment.tmdbId, Some(s.toString)))
            changed.incrementAndGet()
          case Success(_) => ()
          case Failure(exception) =>
            failed.incrementAndGet()
            logger.debug(s"Metascore refresh: $url lookup failed: ${exception.getMessage}")
        }
      }
    }

    val took = System.currentTimeMillis() - startedAt
    val message = s"tick done in ${took}ms — ${changed.get} score(s) changed, " +
                  s"${urlDiscovered.get} URL(s) newly discovered, ${failed.get} failed."
    logger.info(s"Metascore refresh: $message")
    BulkRefreshResult.counts(walked = snapshot.size, changed = changed.get, discovered = urlDiscovered.get, failed = failed.get, message = message)
  }
}
