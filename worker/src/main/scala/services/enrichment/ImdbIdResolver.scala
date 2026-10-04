package services.enrichment

import play.api.Logging
import services.Drainable
import services.events.{DomainEvent, ImdbIdMissing}
import services.movies.MovieCache
import services.resolution.{ResolutionCache, ResolutionKeys}
import tools.{AnswerLadder, DaemonExecutors}

import scala.concurrent.ExecutionContextExecutorService
import scala.util.control.NonFatal

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
  // Final id-crosswalk backstop, tried only after IMDb suggestion + director +
  // Wikidata all abstain: when the row already has a tmdbId, Letterboxd's film
  // page echoes the imdbId. Defaults None so specs resolve as before; `Wiring`
  // injects it.
  letterboxdIdResolver: Option[LetterboxdIdResolver] = None,
  // OMDb id backstop — its English DB carries much of the niche/foreign long tail
  // (Indian, Malayalam, festival titles) that IMDb's suggestion endpoint and
  // Letterboxd miss. Previously only the once-daily `OmdbBackfill` sweep hit it; wiring it as a
  // ladder rung lets a newcomer's id land promptly. `findImdbId` is
  // title+year+director corroborated, so a fuzzy hit can't bind an unrelated film.
  // None (default / `OMDB_API_KEY` unset) skips it.
  omdb: Option[OMDbClient] = None,
  // Cinemeta (Stremio catalogue) — the final rung. IMDb-keyed, indexes a broad
  // foreign/regional long tail; corroborated by title+year. Free, no key. None
  // disables it (default for specs that don't wire it).
  cinemeta: Option[CinemetaClient] = None
) extends Drainable with Logging {
  // Fold titles with the rules the corpus was keyed under, not a process default.
  private val normalizer: services.movies.TitleNormalizer = cache.normalizer

  /**
   * Cached IMDb-id lookup shared by both call sites. Hits-only — a no-match
   * re-queries next time.
   *
   * Tries the title AS GIVEN first, then its DE-DECORATED forms, because a cinema's
   * programme banner is part of the row's title and IMDb has never heard of it. TMDB
   * has always been asked both ways — `resolveTmdbId` builds its candidates through
   * `searchTitleCandidates`, which splits a `"X | Y"` pipe and drops a trailing
   * parenthetical — and this path was not, so it went to IMDb with the banner still
   * attached and could only miss: `ImdbIdMissing(search='Ghost in the Shell | Kino
   * Azji')` → no match, for a film IMDb knows perfectly well. The asymmetry cost those
   * rows an imdbId and, through `tmdb.findByImdbId`, the tmdbId that id would have
   * recovered.
   *
   * Costs nothing on an undecorated title: `searchTitleCandidates` returns the one
   * form, so there is exactly one query, as before. Only a decorated row pays a second
   * lookup, and only when the first missed.
   *
   * Deliberately NOT a fold of the decorated row onto the base film — a cycle
   * screening is its own card by design. It just needs the film's id.
   */
  private def cachedFindId(searchTitle: String, year: Option[Int]): Option[String] =
    services.resolution.SearchTitles.candidates(searchTitle, originalTitle = None)
      .map(cache.normalizer.apiQuery).filter(_.nonEmpty).distinct
      .iterator
      .flatMap(query => imdbIdCache.getOrResolve(ResolutionKeys.imdb(query, year, cache.normalizer))(
        imdb.findId(query, year)))
      .nextOption()

  private val pool = new tools.DrainablePool(executionContext)

  /** Bus listener: when the TMDB stage resolved a film but TMDB has no IMDb
   *  cross-reference for it, recover the id via IMDb's suggestion endpoint
   *  (`ImdbClient.findId`) and write it back to the cached row — the
   *  `EnrichmentReaper` then enqueues its IMDb rating on the next pass.
   *
   *  No-op when the row already carries an imdbId (a stale event raced with another
   *  resolver), has no TMDB id (its IMDb id is the identity resolver's fallback source's to
   *  give), or when the search returns nothing — we'd rather leave the row imdbId-less than
   *  guess a wrong id. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, searchTitle) => pool.submit(resolveOrWarn(title, year, searchTitle))
  }

  /** The event path has no caller to hand a failure to, so it says so here — a failed
   *  lookup is not a "no match", and the row is searched again on its next trigger. */
  private def resolveOrWarn(title: String, year: Option[Int], searchTitle: String): Unit =
    try resolve(title, year, searchTitle)
    catch {
      case NonFatal(failure) =>
        logger.warn(s"IMDb-id: lookup for '$title' (${year.getOrElse("?")}) [search='$searchTitle'] failed, " +
          s"not concluded: ${failure.getMessage}")
    }

  /** Synchronous resolution — public for tests/scripts (e.g. `Wiring.fullySyncOne`),
   *  which drive the downstream `*Ratings.refreshOneSync` themselves on the calling
   *  thread. Same work as the event-driven path; the id write is the only effect. */
  def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit =
    resolve(title, year, searchTitle)

  /**
   * Every rung, cache-free — suggestion endpoint, director corroboration, Wikidata,
   * Letterboxd, OMDb, Cinemeta — for a row that is NOT in the movie cache.
   *
   * Extracted because a recovery holding a row that is not cached (the since-deleted
   * staging fold's) reached only the first rung; the other five lived inside
   * `resolve`, behind a `cache.get` — and one replay leg logged 1,696 such lookups against
   * 11 that reached the full ladder, precisely the long tail those rungs exist for. IMDb's suggestion endpoint answers a Polish
   * query with the film's ENGLISH title ("Brzezina" → "The Birch Wood") and the matcher
   * rejects it, while Cinemeta returns tt0068321 for the same query — the exact id
   * production holds.
   */
  private[enrichment] def lookupId(searchTitle: String, year: Option[Int], record: models.MovieRecord = models.MovieRecord()): Option[String] = {
    // A row no source dated still has a year when its title brackets one ("It (1990)"): without
    // it the search is yearless, and the lone IMDb film of the bare name binds (It, 2017).
    val reported = record.cinemaData.values.flatMap(_.releaseYear).toSet ++ year
    val years = (if (reported.nonEmpty) reported
                 else services.movies.EmbeddedYear.ofAll(searchTitle +: record.evidence.titles.toSeq).toSet).toSeq.sorted
    val yearSeq = if (years.isEmpty) Seq(year) else years.map(Option(_))
    // Each rung is asked in turn and the first id wins; a rung whose source FAILED does not
    // stop the ones after it, but if none answers the failure is thrown, not booked as
    // "no match" (AnswerLadder).
    val directors = record.director.toSet
    AnswerLadder.firstAnswer(
      yearSeq.map(y => () => cachedFindId(searchTitle, y)) ++
      // Director-based fallback: when the year-anchored cached search returns nothing
      // (e.g. IMDb hasn't set a release year yet for a fresh film), try confirming an
      // exact-deburr-title candidate via director. Not routed through the cache — the
      // basic path already cached a miss; this live fallback only fires when directors
      // are known and can disambiguate.
      (if (directors.nonEmpty) yearSeq.map(y => () => imdb.findId(searchTitle, y, directors)) else Nil) ++
      Seq(
        // Wikidata fallback: ONE claims call cross-references via the Filmweb
        // entity id (P5032) and yields every film-database id at once. Only fires
        // when the filmwebUrl is a real entity page (not a search redirect) and a
        // WikidataClient has been wired.
        // Only the imdbId is taken. The claims also name RT and Metacritic pages, but those
        // links belong to their rating tasks (`RottenTomatoesRatings`, `MetascoreRatings`):
        // written here they raced a task resolving the same row, and whichever read the row
        // first decided what was fetched — Wikidata's stale "m/1016356-pippi_longstocking"
        // (a 404) in some PL convergence runs and not others.
        () => for {
          client    <- wikidata
          url       <- record.filmwebUrl
          filmwebId <- WikidataClient.filmwebEntityId(url)
          ids       <- client.findIdsByFilmwebId(filmwebId)
          imdbId    <- ids.imdbId
        } yield imdbId,
        // Letterboxd backstop — when the row already has a tmdbId, its Letterboxd
        // film page echoes the imdbId (echo-checked against the queried tmdbId).
        () => for {
          resolver <- letterboxdIdResolver
          tmdbId   <- record.tmdbId
          imdbId   <- resolver.resolveImdbId(tmdbId)
        } yield imdbId,
        // OMDb backstop — the English DB that covers much of the long tail TMDB's
        // IMDb cross-references miss (Indian/Malayalam/festival titles). title+year+director
        // corroborated (see OMDbClient) so a fuzzy hit can't bind a wrong film.
        // This is the id the once-daily OmdbBackfill sweep would have supplied
        // hours later; running it inline lands it now. A lookup OMDb could not answer
        // (down, quota spent) falls through to the next rung; nothing is recorded for it.
        () => omdb.flatMap(_.findImdbId((searchTitle +: record.evidence.titles.toSeq).distinct, year, directors)),
        // Wikidata DIRECT-title — distinct from the Filmweb-id path above: for a
        // film with no Filmweb entity page, search Wikidata's film items
        // by title and bind the first whose label + P577 year corroborate. Catches
        // films with a Wikidata entry (hence RT/MC/Letterboxd slugs too) that the
        // English-DB resolvers miss.
        () => wikidata.flatMap(_.findImdbIdByTitle(searchTitle, year)),
        // Cinemeta (Stremio) — final rung. IMDb-keyed catalogue covering a broad
        // foreign/regional long tail; corroborated by title+year so a fuzzy hit
        // can't bind a wrong film. Free, no API key.
        () => cinemeta.flatMap(_.findImdbId((searchTitle +: record.evidence.titles.toSeq).distinct, year))
      )*)
  }


  private def resolve(title: String, year: Option[Int], searchTitle: String): Unit = {
    val key = cache.keyOf(title, year)
    // Only a TMDB film TMDB gave no IMDb id: a film TMDB has no record of takes its IMDb id from the identity
    // resolver's fallback source (`ResolverDecision.fallback`), on every fact it publishes, never from a title search.
    cache.get(key).filter(record => record.imdbId.isEmpty && record.tmdbId.isDefined).foreach { record =>
      logger.info(s"IMDb-id: looking up '${key.cleanTitle}' (${key.year.getOrElse("?")}) [search='$searchTitle']")
      // Try every year the film's cinemas report (plus the key year), sorted — the
      // mirror of the staging recovery. IMDb's release year can sit at any cinema's
      // reported (production) year, not the canonical TMDB one ("Chłopiec na krańcach
      // świata": TMDB 2026, IMDb + the cinemas 2025), so a single-key-year lookup left
      // the id flickering present/absent with arrival order.
      // The sorted year set is order-independent; the per-year EXACT match still refuses
      // a same-series sibling ("Kicia Kocia w przedszkolu" 2024) at no reported year.
      lookupId(searchTitle, year, record) match {
        case Some(id) if heldByAnotherFilm(id, key, record) =>
          logger.warn(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → $id refused: another film " +
            s"already holds it [search='$searchTitle']")
        case Some(id) =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → resolved $id")
          // putIfPresent so a concurrent `cache.invalidate` between the lookup and
          // the write-back can't resurrect the row; and only over the id the search
          // started from, so an id another writer (TMDB's resolution) landed while the
          // search was out stays.
          if (!record.imdbId.contains(id))
            cache.putIfPresent(key, current => if (current.imdbId == record.imdbId) current.copy(imdbId = Some(id)) else current)
        case None =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match [search='$searchTitle']")
      }
    }
  }

  /** Whether a row other than `key`'s — and not the same TMDB film — already carries `id`. Two
   *  films sharing an imdbId show one's ratings on the other, so the second never takes it. */
  private def heldByAnotherFilm(id: String, key: services.movies.CacheKey, record: models.MovieRecord): Boolean =
    cache.entries.exists { case (other, row) =>
      other != key && row.imdbId.contains(id) && !(record.tmdbId.isDefined && row.tmdbId == record.tmdbId)
    }

  /** Wait for in-flight id write-backs, leaving the pool able to take more. Waits for
   *  the queue to drain, not a fixed window — the bounded cap was returning before
   *  real-network suggestion lookups finished and `cascadeDrainOrder`'s next entry was
   *  shutting down a pool that still had inbound work coming. */
  def drain(): Unit = pool.drain()

  /** Drain within `budget`, then end the pool — shutdown only. A caller that merely wants the
   *  in-flight work finished wants [[drain]]: this one rejects everything after it,
   *  and the replay boot drains BEFORE the staging fold publishes the
   *  `ImdbIdMissing` events that need this resolver.
   *
   *  Lookups still queued at the budget are dropped: the row keeps no imdbId, so it is asked
   *  again — on its next Filmweb rating refresh, the daily OMDb backfill sweep, or its next
   *  TMDB (re-)identification — exactly as a failed lookup is. */
  override def stopWithin(budget: scala.concurrent.duration.FiniteDuration): Unit = {
    val dropped = pool.stop(budget)
    if (dropped > 0) logger.info(s"ImdbIdResolver: stopped with $dropped id lookup(s) unfinished — dropped; each row is asked again on its next trigger.")
  }

  def stop(): Unit = stopWithin(tools.ManagedResources.Grace)
}
