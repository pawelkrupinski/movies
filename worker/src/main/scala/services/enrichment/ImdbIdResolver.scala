package services.enrichment

import play.api.Logging
import services.Stoppable
import services.events.{DomainEvent, ImdbIdMissing}
import services.movies.{CacheKey, MovieCache}
import services.resolution.{ResolutionCache, ResolutionKeys}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.Try

/**
 * Recovers a missing IMDb id by querying IMDb's suggestion endpoint and writes
 * the id back to the cached row — from where the `EnrichmentReaper` picks up the
 * now-eligible IMDb rating on its next pass (no rating event is fired).
 *
 * Split out of `ImdbRatings` so rating maintenance doesn't entangle with
 * id discovery — `ImdbRatings` now only deals with the rating lifecycle and
 * can rely on `imdbId` being already set on the rows it touches.
 *
 * Async — runs on its own worker pool so the publisher (the TMDB stage
 * worker) isn't blocked on IMDb. Lifecycle owned by `AppLoader`.
 */
class ImdbIdResolver(
  cache: MovieCache,
  imdb:  ImdbClient,
  // IMDb's suggestion endpoint is fast and the event-driven path is sparse (the
  // TMDB stage emits many resolutions at startup but very few carry an
  // `ImdbIdMissing`). Virtual threads keep per-task overhead trivial; the rate
  // cap, if any, sits at the HTTP layer. Defaults to a dedicated unbounded
  // pool (tests/scripts unchanged); `Wiring` injects a shared-budget EC. See
  // `SharedExecutionBudget`.
  executionContext:    ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC("imdb-id-resolver"),
  // Caches the IMDb suggestion lookup keyed by (search title, year), so the same
  // search resolves once for 24h across the staging and event-driven paths.
  // Defaults to passthrough so unit specs resolve live unless they wire one.
  imdbIdCache: ResolutionCache = ResolutionCache.passthrough,
  // Wikidata cross-reference fallback: when both the IMDb suggestion endpoint and
  // the director-based path return nothing, try Filmweb-ID → Wikidata P5032 →
  // IMDb P345. Covers classic/repertoire films whose titles differ too much
  // between the cinema listing and IMDb for the suggestion endpoint to match.
  // None disables the fallback (default for tests that don't wire Wikidata).
  wikidata: Option[WikidataClient] = None,
  // Final id-crosswalk backstops, tried only after IMDb suggestion + director +
  // Wikidata all abstain: Trakt's corroborated title search (its result carries
  // the imdbId), then — when the row already has a tmdbId — Letterboxd's film
  // page, which echoes the imdbId. Both default None so specs resolve as before;
  // `Wiring` injects them (Trakt no-ops without `TRAKT_API_CLIENT_ID`).
  traktIdResolver:      Option[TraktIdResolver]      = None,
  letterboxdIdResolver: Option[LetterboxdIdResolver] = None,
  // OMDb id backstop — its English DB carries much of the niche/foreign long tail
  // (Indian, Malayalam, festival titles) that IMDb's suggestion endpoint and Trakt
  // miss. Previously only the once-daily `OmdbBackfill` sweep hit it; wiring it as a
  // ladder rung lets a TMDB-less newcomer's id land promptly. `findImdbId` is
  // title+year+director corroborated, so a fuzzy hit can't bind an unrelated film.
  // None (default / `OMDB_API_KEY` unset) skips it.
  omdb: Option[OMDbClient] = None,
  // Cinemeta (Stremio catalogue) — the final rung. IMDb-keyed, indexes a broad
  // foreign/regional long tail; corroborated by title+year. Free, no key. None
  // disables it (default for specs that don't wire it).
  cinemeta: Option[CinemetaClient] = None
) extends Stoppable with Logging {

  /** Cached IMDb-id lookup shared by both call sites. Hits-only — a no-match
   *  re-queries next time. */
  private def cachedFindId(searchTitle: String, year: Option[Int]): Option[String] =
    imdbIdCache.getOrResolve(ResolutionKeys.imdb(searchTitle, year))(Try(imdb.findId(searchTitle, year)).toOption.flatten)

  /** Bus listener: when the TMDB stage resolved a film but TMDB has no IMDb
   *  cross-reference for it, recover the id via IMDb's suggestion endpoint
   *  (`ImdbClient.findId`) and write it back to the cached row — the
   *  `EnrichmentReaper` then enqueues its IMDb rating on the next pass.
   *
   *  No-op when the row already carries an imdbId (a stale event raced with
   *  another resolver) or when the search returns nothing — we'd rather leave
   *  the row imdbId-less than guess a wrong id. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, searchTitle) =>
      Future(resolve(title, year, searchTitle))(using executionContext)
      ()
  }

  /** Synchronous resolution — public for tests/scripts (e.g. `Wiring.fullySyncOne`),
   *  which drive the downstream `*Ratings.refreshOneSync` themselves on the calling
   *  thread. Same work as the event-driven path; the id write is the only effect. */
  def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit =
    resolve(title, year, searchTitle)

  /** Cache-free id lookup: query IMDb's suggestion endpoint for `searchTitle`
   *  and return the id, or None on no match / a transient failure. Used by the
   *  staging promoter to recover a missing imdbId INLINE — a staging row isn't in
   *  the cache, so the event-driven `onImdbIdMissing` (which reads + `putIfPresent`s
   *  the cache) can't reach it; recovering here folds the row already carrying the
   *  id, the same end state the direct path's `ImdbIdMissing` chain produces. */
  def findIdFor(searchTitle: String, year: Option[Int]): Option[String] = {
    logger.info(s"IMDb-id (staging): looking up [search='$searchTitle'] (${year.getOrElse("?")})")
    val id = cachedFindId(searchTitle, year)
    logger.info(s"IMDb-id (staging): [search='$searchTitle'] (${year.getOrElse("?")}) → ${id.getOrElse("no match")}")
    id
  }

  private def resolve(title: String, year: Option[Int], searchTitle: String): Unit = {
    val key = cache.keyOf(title, year)
    cache.get(key).filter(_.imdbId.isEmpty).foreach { record =>
      logger.info(s"IMDb-id: looking up '${key.cleanTitle}' (${key.year.getOrElse("?")}) [search='$searchTitle']")
      // Try every year the film's cinemas report (plus the key year), sorted — the
      // mirror of the staging recovery. IMDb's release year can sit at any cinema's
      // reported (production) year, not the canonical TMDB one ("Chłopiec na krańcach
      // świata": TMDB 2026, IMDb + the cinemas 2025), so a single-key-year lookup left
      // the id flickering present/absent with arrival order (StagingOrderDeterminismSpec).
      // The sorted year set is order-independent; the per-year EXACT match still refuses
      // a same-series sibling ("Kicia Kocia w przedszkolu" 2024) at no reported year.
      val years = (record.cinemaData.values.flatMap(_.releaseYear).toSet ++ year).toSeq.sorted
      val yearSeq = if (years.isEmpty) Seq(year) else years.map(Option(_))
      val found = yearSeq.iterator.flatMap(y => cachedFindId(searchTitle, y)).nextOption()
        .orElse {
          // Director-based fallback: when the year-anchored cached search returns nothing
          // (e.g. IMDb hasn't set a release year yet for a fresh film), try confirming an
          // exact-deburr-title candidate via director. Not routed through the cache — the
          // basic path already cached a miss; this live fallback only fires when directors
          // are known and can disambiguate.
          val directors = record.director.toSet
          if (directors.nonEmpty)
            yearSeq.iterator.flatMap(y => Try(imdb.findId(searchTitle, y, directors)).toOption.flatten).nextOption()
          else None
        }
        .orElse {
          // Wikidata fallback: ONE claims call cross-references via the Filmweb
          // entity id (P5032) and yields every film-database id at once. Only fires
          // when the filmwebUrl is a real entity page (not a search redirect) and a
          // WikidataClient has been wired. Never throws — findIdsByFilmwebId absorbs
          // all network failures and returns None.
          val harvested = for {
            client    <- wikidata
            url       <- record.filmwebUrl
            filmwebId <- WikidataClient.filmwebEntityId(url)
            ids       <- client.findIdsByFilmwebId(filmwebId)
          } yield ids
          // Backfill the RT / Metacritic page URLs the harvest turned up — their
          // rating clients otherwise slug-probe/scrape to discover them. (TMDB's
          // P4947 is intentionally NOT applied here: a film reaching this bridge
          // already has a tmdbId — that's what fired `ImdbIdMissing` and gated its
          // Filmweb enrichment — so harvesting it would be a no-op.)
          harvested.foreach(backfillRatingUrls(key, _))
          harvested.flatMap(_.imdbId)
        }
        .orElse {
          // Trakt backstop — a corroborated title+year search whose matched film
          // carries the imdbId (exact deburred-title + non-contradicting year +
          // lone match; TraktIdResolver never guesses among several candidates).
          traktIdResolver.flatMap(_.resolve(None, (searchTitle +: record.cinemaTitles.toSeq).distinct, year).imdbId)
        }
        .orElse {
          // Letterboxd backstop — when the row already has a tmdbId, its Letterboxd
          // film page echoes the imdbId (echo-checked against the queried tmdbId).
          for {
            resolver <- letterboxdIdResolver
            tmdbId   <- record.tmdbId
            imdbId   <- resolver.resolveImdbId(tmdbId)
          } yield imdbId
        }
        .orElse {
          // OMDb backstop — the English DB that covers most of the TMDB-less
          // long tail (Indian/Malayalam/festival titles). title+year+director
          // corroborated (see OMDbClient) so a fuzzy hit can't bind a wrong film.
          // This is the id the once-daily OmdbBackfill sweep would have supplied
          // hours later; running it inline lands it now.
          omdb.flatMap(_.findImdbId((searchTitle +: record.cinemaTitles.toSeq).distinct, year, record.director.toSet))
        }
        .orElse {
          // Wikidata DIRECT-title — distinct from the Filmweb-id path above: for a
          // TMDB-less film with no Filmweb entity page, search Wikidata's film items
          // by title and bind the first whose label + P577 year corroborate. Catches
          // films with a Wikidata entry (hence RT/MC/Letterboxd slugs too) that the
          // English-DB resolvers miss.
          wikidata.flatMap(_.findImdbIdByTitle(searchTitle, year))
        }
        .orElse {
          // Cinemeta (Stremio) — final rung. IMDb-keyed catalogue covering a broad
          // foreign/regional long tail; corroborated by title+year so a fuzzy hit
          // can't bind a wrong film. Free, no API key.
          cinemeta.flatMap(_.findImdbId((searchTitle +: record.cinemaTitles.toSeq).distinct, year))
        }
      found match {
        case Some(id) =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → resolved $id")
          // putIfPresent so a concurrent `cache.invalidate` between the lookup and
          // the write-back can't resurrect the row.
          cache.putIfPresent(key, _.copy(imdbId = Some(id)))
        case None =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match [search='$searchTitle']")
      }
    }
  }

  /** Fill the row's Rotten Tomatoes / Metacritic page URLs from a Wikidata
   *  harvest when they're still empty. Those rating clients skip their expensive
   *  slug-probe/scrape once the URL is present (`e.rottenTomatoesUrl.orElse(...)`
   *  / `e.metacriticUrl match`). `orElse` against the live row so a canonical
   *  writer that filled either in between keeps its value — Wikidata only
   *  unblocks a still-empty slug (the `OmdbBackfill` precedent for RT). */
  private def backfillRatingUrls(key: CacheKey, ids: WikidataIds): Unit = {
    val rtUrl = ids.rottenTomatoesId.map(WikidataClient.rottenTomatoesUrl)
    val mcUrl = ids.metacriticId.map(WikidataClient.metacriticUrl)
    if (rtUrl.isDefined || mcUrl.isDefined)
      cache.putIfPresent(key, cur => cur.copy(
        rottenTomatoesUrl = cur.rottenTomatoesUrl.orElse(rtUrl),
        metacriticUrl     = cur.metacriticUrl.orElse(mcUrl)
      ))
  }

  /** Drain the worker pool so in-flight id write-backs have completed before the
   *  caller moves on. Waits for the queue to drain, not a fixed window — the
   *  bounded cap was returning before real-network suggestion lookups finished
   *  and `cascadeDrainOrder`'s next entry was shutting down a pool that still had
   *  inbound work coming. */
  def stop(): Unit = {
    executionContext.shutdown()
    while (!executionContext.isTerminated) executionContext.awaitTermination(1, TimeUnit.HOURS)
  }
}
