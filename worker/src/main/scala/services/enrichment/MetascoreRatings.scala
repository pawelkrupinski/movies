package services.enrichment

import clients.TmdbClient
import services.movies.{CacheKey, MovieCache, MovieService}
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
  metacritic: MetacriticClient
) extends CacheRefresher(cache) {

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings:
  //   - URL already known → cheap: scrape metascore, write back if changed.
  //   - URL missing       → expensive: probe MC slug variants (with TMDB
  //     details for English title + year disambiguation), write the URL,
  //     then scrape the metascore.
  // Per-row failures are swallowed; the next refresh tries again.
  protected def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      val urlOpt = e.metacriticUrl.orElse(resolveAndPersistUrl(key, e))
      urlOpt.foreach(url => refreshScoreFromUrl(key, e, url))
    }

  // Probe MC for a canonical /movie/ URL using TMDB's title data; write it
  // back to the cache when found. Returns the new URL (or None if MC didn't
  // index the film).
  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      // `apiQuery` strips accessibility-programme decoration so an "Kino
      // bez barier: Arco (AD)" row queries MC as just "Arco". Cache key
      // stays decorated so the accessibility screening keeps its own row.
      val cleanLookup = MovieService.apiQuery(key.cleanTitle)
      val linkTitle   = e.originalTitle.getOrElse(cleanLookup)
      val mcFallback  = if (linkTitle != cleanLookup) Some(cleanLookup) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      val primary = Try(metacritic.urlFor(linkTitle, mcFallback, year)).toOption.flatten
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
      val resolved = primary
        .orElse(englishTitle.flatMap(t => Try(metacritic.urlFor(t, None, year)).toOption.flatten))
        .orElse(usTitle.flatMap(t => Try(metacritic.urlFor(t, None, year)).toOption.flatten))

      resolved.foreach { url =>
        logger.debug(s"Metascore: ${key.cleanTitle} discovered $url")
        cache.putIfPresent(key, _.copy(metacriticUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Unit =
    Try(metacritic.metascoreFor(url)).toOption.flatten match {
      case Some(score) if !e.metascore.contains(score) =>
        // The updater receives the live cached row — that's the merge point
        // for both the URL we may have just written and any other listener's
        // concurrent updates. No risk of clobbering.
        cache.putIfPresent(key, current => {
          logger.debug(s"Metascore: ${key.cleanTitle} $url ${current.metascore.getOrElse("—")} → $score")
          current.copy(metascore = Some(score))
        })
      case _ => ()
    }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `metacriticUrl` get a cheap score
   *  refresh; rows without one get the full URL-discovery probe (and then a
   *  score refresh if discovery succeeds). Per-row failures are logged at
   *  debug — one bad row can't poison the whole tick. */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.metacriticUrl.isDefined }
    logger.info(s"Metascore refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → score-only, ${missingUrl.size} without → URL discovery).")
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)

    BoundedParallel.foreach("Metascore-refresh-score", withUrl, refreshConcurrency) { case (key, enrichment) =>
      val url = enrichment.metacriticUrl.get
      Try(metacritic.metascoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.metascore =>
          logger.debug(s"Metascore refresh: ${key.cleanTitle} $url ${enrichment.metascore.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(metascore = fresh))
          changed.incrementAndGet()
        case Success(_) => ()
        case Failure(ex) =>
          failed.incrementAndGet()
          logger.debug(s"Metascore refresh: $url lookup failed: ${ex.getMessage}")
      }
    }

    BoundedParallel.foreach("Metascore-refresh-discover", missingUrl, refreshConcurrency) { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered.incrementAndGet()
        // Use the post-write row so the score copy doesn't clobber the URL.
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Metascore refresh: tick done in ${took}ms — ${changed.get} score(s) changed, " +
                s"${urlDiscovered.get} URL(s) newly discovered, ${failed.get} failed.")
  }
}
