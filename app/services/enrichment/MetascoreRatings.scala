package services.enrichment

import clients.TmdbClient
import services.events.{DomainEvent, ImdbIdMissing, TmdbResolved}
import services.movies.{CacheKey, MovieCache}

import scala.util.{Failure, Success, Try}

/**
 * Metacritic side of enrichment — owns BOTH:
 *   - `metacriticUrl` discovery (slug probe + cleanTitle fallback + lazy
 *     `englishTitle` fallback for non-English films).
 *   - `metascore` scrape from the resolved URL.
 *
 * Shared lifecycle + worker plumbing lives in [[PeriodicCacheRefresher]].
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
) extends PeriodicCacheRefresher(
  name                = "Metascore",
  // 3 workers — MC pages are heavier than IMDb's GraphQL endpoint, and the
  // hourly walk makes hundreds of requests. 3 keeps us under MC's tolerated
  // rate while still parallelising the I/O wait.
  workers             = 3,
  // Stagger the startup tick so we don't race the IMDb / RT refreshes —
  // they all walk the same cache and serialising the bursts avoids Mongo
  // fan-in.
  startupDelaySeconds = 30L,
  refreshHours        = 1L,
  cache               = cache
) {

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: discover the MC URL (if missing) and refresh the metascore
   *  as soon as the TMDB stage produces a row. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  /** Sibling listener: fire on `ImdbIdMissing` too. The TMDB stage publishes
   *  this when TMDB resolved the film but had no IMDb cross-reference yet
   *  (common for very recent Polish releases). The MC URL + metascore don't
   *  depend on the IMDb id, so we want to refresh on either signal. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Two paths, mirroring FilmwebRatings:
  //   - URL already known → cheap: scrape metascore, write back if changed.
  //   - URL missing       → expensive: probe MC slug variants (with TMDB
  //     details for English title + year disambiguation), write the URL,
  //     then scrape the metascore.
  // Per-row failures are swallowed; the next periodic tick tries again.
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
      val linkTitle  = e.originalTitle.getOrElse(key.cleanTitle)
      val mcFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
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

  // ── Periodic walk ──────────────────────────────────────────────────────────

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
    var changed       = 0
    var failed        = 0
    var urlDiscovered = 0

    withUrl.foreach { case (key, enrichment) =>
      val url = enrichment.metacriticUrl.get
      Try(metacritic.metascoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.metascore =>
          logger.debug(s"Metascore refresh: ${key.cleanTitle} $url ${enrichment.metascore.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(metascore = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"Metascore refresh: $url lookup failed: ${ex.getMessage}")
      }
    }

    missingUrl.foreach { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered += 1
        // Use the post-write row so the score copy doesn't clobber the URL.
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Metascore refresh: tick done in ${took}ms — $changed score(s) changed, " +
                s"$urlDiscovered URL(s) newly discovered, $failed failed.")
  }
}
