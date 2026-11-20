package services.corpus

import models.{CacheKey, SlotFields, StoredMovieRecord}

import services.movies.*

import clients.TmdbClient
import play.api.Logging
import services.events.{EventBus, ImdbIdMissing}
import services.freshness.{FreshnessKind, FreshnessStore}
import services.tasks.{RatingTasks, TaskWorker}

import models.{MovieRecord, Source, SourceData, TitleSearch, Tmdb}
import services.identity.{Hit, PinnedGateMeasures, StoredIdentityConfidence}
import scala.util.{Failure, Try}

/**
 * The films' TMDB side, as the identity projection writes it: a film's details fetched BY ID for a
 * film new to its record ([[withFilmDetails]]), built into the record by [[buildResolvedRecord]] —
 * cinema data and, for the same film, ratings carried forward — and the announcements that follow a
 * (re)identified film: `ImdbIdMissing` when TMDB has no IMDb cross-reference, so `ImdbIdResolver`
 * recovers the id, and its rating tasks enqueued now rather than at the `EnrichmentReaper`'s next tick.
 * Which film a listing is, is the identity model's to decide; nothing here searches.
 */
class MovieService(
  cache: MovieCache,
  bus:   EventBus,
  tmdb:  TmdbClient,
  // Where the row's TMDB-resolution TIME is stamped (FreshnessKind.TmdbResolve),
  // so `RatingHandler` can measure how long after resolution each site's first
  // rating attempt fired (the EnrichmentReaper first-pass latency metric).
  // Production injects the SHARED store the rating handlers read; a spec hands in a throwaway
  // in-memory one (the stamp is observability, not correctness).
  freshness: FreshnessStore,
  // Immediately enqueue a freshly-identified film's due rating tasks (see
  // `announceResolvedNewMovie`) so its ratings don't wait for the
  // `EnrichmentReaper`'s next tick. Default no-op for tests/scripts without a task
  // queue; production passes `RatingEnqueuer.enqueueDueFor` (the SAME enqueuer the
  // reaper walks the corpus with).
  enqueueNewcomerRatings: (CacheKey, MovieRecord) => Unit = (_, _) => (),
  // Re-fetch EVERY rating source for a re-identified film, ignoring the adaptive cadence:
  // the projection builds its record afresh, but the rating freshness stamps survive — so
  // the reaper would judge each source "recently checked" and leave the film rating-less.
  // Default no-op; production passes the shared `RatingEnqueuer.enqueueDueFor(..., force = true)`.
  forceRatingRefresh: (CacheKey, MovieRecord) => Unit = (_, _) => (),
  // Stamps the resolution time the rating-latency metric reads.
  clock:                java.time.Clock
) extends Logging {

  /** Pure cache lookup — never blocks, never schedules. */
  def get(title: String, year: Option[Int]): Option[MovieRecord] =
    cache.get(cache.keyOf(title, year))

  /** Snapshot of every cached enrichment — for debug tooling. */
  def snapshot(): Seq[StoredMovieRecord] = cache.snapshot()

  /** Reload the positive cache from Mongo. Returns the number of rows loaded.
   *  Wired to the `/debug/rehydrate` admin endpoint; useful when Mongo has
   *  been edited out-of-band and the in-memory cache needs to catch up. */
  def rehydrate(): Int = cache.rehydrate()

  /** Announce a film newly identified, or identified as another film: stamp its resolution time
   *  (for the first-rating delay metric), kick IMDb-id recovery for a TMDB film TMDB gave no IMDb id (`ImdbIdMissing`
   *  → `ImdbIdResolver`), and IMMEDIATELY enqueue the now-eligible rating tasks (`enqueueNewcomerRatings`)
   *  so its ratings don't wait for the `EnrichmentReaper`'s next tick. A film TMDB has no record of carries the IMDb id
   *  the identity resolver's fallback source gave it, if any (`ResolverDecision.fallback`) — never one a title search
   *  guessed — so its ratings, IMDb's among them, are enqueued as they stand. */
  def announceResolvedNewMovie(key: CacheKey, record: MovieRecord): Unit =
    if (record.tmdbId.isDefined) {
      publishTmdbOutcome(key, record)
      enqueueNewcomerRatings(key, record)
    } else enqueueNewcomerRatings(key, record)

  /** Announce a film an identity projection wrote under a new TMDB answer. The projection builds
   *  such a record afresh, without the ratings its former identity held, so — as after a forced
   *  re-resolve — every rating source's schedule restarts: stamps a TMDB-less film left under its
   *  title key would otherwise read fresh and leave the rebuilt record unrated until they lapse. */
  def announceReidentified(key: CacheKey, record: MovieRecord): Unit = {
    forceRatingRefresh(key, record)
    announceResolvedNewMovie(key, record)
  }

  // Publish the post-resolution event so the rating refreshers re-run for the
  // row off the existing event chain.
  private def publishTmdbOutcome(finalKey: CacheKey, movieRecord: MovieRecord): Unit = {
    // Stamp WHEN this row resolved (keyed by the immutable tmdbId) so the rating
    // handler can measure the resolved → first-rating-attempt delay per site.
    movieRecord.tmdbId.foreach(id => freshness.markFresh(RatingTasks.tmdbResolvedAtKey(id), FreshnessKind.TmdbResolve, clock.instant()))
    movieRecord.imdbId match {
      case Some(id) =>
        // imdbId already known → nothing to recover; the EnrichmentReaper picks up
        // this row's ratings on its next due pass (no per-resolution rating event).
        logger.info(s"TMDB: '${finalKey.cleanTitle}' (${finalKey.year.getOrElse("?")}) → matched tmdbId=${movieRecord.tmdbId.getOrElse("—")} imdbId=$id")
      case None =>
        // IMDb's suggestion endpoint sees the cleaned-up form when TMDB didn't
        // ship an originalTitle, so accessibility-decorated rows ("Kino bez
        // barier: Freak Show (AD)") query IMDb as just "Freak Show". TMDB's
        // originalTitle, when present, is already canonical and needs no stripping.
        val searchTitle = movieRecord.searchTitle.orElse(movieRecord.originalTitle).getOrElse(cache.normalizer.searchQuery(finalKey.cleanTitle))
        logger.info(s"TMDB: '${finalKey.cleanTitle}' (${finalKey.year.getOrElse("?")}) → matched tmdbId=${movieRecord.tmdbId.getOrElse("—")} (no IMDb cross-reference yet); publishing ImdbIdMissing(search='$searchTitle')")
        bus.publish(ImdbIdMissing(finalKey.cleanTitle, finalKey.year, searchTitle))
    }
  }

  /** Build the resolved `MovieRecord` from a film's TMDB details + the row's `existing`
   *  record — pure (no cache, no lock): how a resolution writes the TMDB-side fields
   *  + `Tmdb` slot while carrying the cinema-side data and score fields forward.
   *
   *  The previously-known `imdbId`/`wikidataId` are preserved when TMDB resolved the
   *  same film (same `tmdbId`) but momentarily dropped a cross-reference —
   *  happens for very recent releases and occasional TMDB data hiccups. A
   *  DIFFERENT tmdbId accepts the new film's ids (even None) so a stale id
   *  can't leak across.
   *
   *  Every rating url and score describes a FILM, so they follow the film.
   *  Carrying them forward unconditionally meant a corrected row kept the WRONG
   *  film's numbers until each source's own refresh cadence came round: prod
   *  served Michel Franco's "Dreams" with the Norwegian film's
   *  `/movie/dreams-drommer` and metascore 81 long after the tmdbId was fixed.
   *  The trigger is a film that demonstrably CHANGED — not merely "not the same
   *  one". A row resolving for the FIRST time has no previous tmdbId, so there is
   *  nothing stale to drop and a rating a refresher already wrote must survive
   *  (the since-deleted `TmdbCarryForwardReadFailureSpec`). Once cleared, the `*Ratings` enrichers
   *  re-resolve from the new identity.
   *
   *  The row keeps the TMDB basis it already recorded: a fill by id asserts none of its own. */
  private def buildResolvedRecord(
    tmdbId:      Int,
    externalIds: TmdbClient.ExternalIds,
    details:     TmdbClient.FullDetails,
    existing:    MovieRecord
  ): MovieRecord = {
    val sameFilm      = existing.tmdbId.contains(tmdbId)
    val differentFilm = existing.tmdbId.exists(_ != tmdbId)
    def ifSameFilm[A](value: Option[A]): Option[A] = if (differentFilm) None else value
    MovieRecord(
      imdbId            = externalIds.imdbId.orElse(if (sameFilm) existing.imdbId else None),
      imdbRating        = ifSameFilm(existing.imdbRating),
      metascore         = ifSameFilm(existing.metascore),
      filmwebUrl        = ifSameFilm(existing.filmwebUrl),
      filmwebRating     = ifSameFilm(existing.filmwebRating),
      rottenTomatoes    = ifSameFilm(existing.rottenTomatoes),
      tmdbId            = Some(tmdbId),
      // Keep the EVIDENCE beside the conclusion. Without it a guess from a bare
      // title is indistinguishable ever after from an answer a director's
      // filmography confirmed — which is how five wrong resolutions survived weeks
      // in prod on rows that had since acquired the hints to correct them.
      tmdbBasis         = existing.tmdbBasis,
      // whether the identity take it resolved is provisional is the projection's, which hands it the record to complete
      tmdbProvisional   = existing.tmdbProvisional,
      wikidataId        = externalIds.wikidataId.orElse(if (sameFilm) existing.wikidataId else None),
      metacriticUrl     = ifSameFilm(existing.metacriticUrl),
      rottenTomatoesUrl = ifSameFilm(existing.rottenTomatoesUrl),
      // Carry the cinema-side fields forward — the TMDB stage doesn't own cinema
      // data; without this a fresh resolve would wipe every cinema's slot.
      data              = existing.data + ((Tmdb: Source) -> resolvedTmdbSlot(tmdbId, details, existing, differentFilm))
    )
  }

  /** The row's `Tmdb` slot after a resolution to `tmdbId`, from its full TMDB record (fetched in a single round-trip),
   *  so the slot carries the localized synopsis, director, cast, runtime, year, countries and poster. */
  private def resolvedTmdbSlot(tmdbId: Int, details: TmdbClient.FullDetails,
                               existing: MovieRecord, differentFilm: Boolean): SourceData = {
    val existingTmdbSlot = existing.data.getOrElse(Tmdb, SourceData())
    // TMDB's English release title (en-US `title`, via the same `details` call
    // MC/RT use). For a non-Latin-original film whose Polish `title` and
    // `originalTitle` both differ from the English title a cinema lists it under
    // ("Left-Handed Girl"), this is the title a cinema's English listing names it by
    // (see `SourceData.englishTitle`).
    val englishTitle = tmdb.englishTitle(tmdbId).orElse(existingTmdbSlot.englishTitle)
    // Where the film stands in each of the row's listing titles' own yearless TMDB searches —
    // the identity evidence the rating gate scores (`StoredIdentityConfidence`). Kept while the
    // film stays the same; a different film is measured afresh.
    val titleSearches = measureTitleSearches(tmdbId, existing,
      carried = if (differentFilm) Nil else existingTmdbSlot.titleSearches)
    detailsSlot(details, existingTmdbSlot).copy(englishTitle = englishTitle, titleSearches = titleSearches)
  }

  /** The `Tmdb` slot from fetched details `d`, each field falling back to what the slot already held. */
  private def detailsSlot(d: TmdbClient.FullDetails, existingTmdbSlot: SourceData): SourceData = {
    // Whether the slot we're merging onto was fetched in the language we're
    // fetching in NOW. When it wasn't — the stale-language re-resolve
    // the since-deleted `UnresolvedTmdbReaper` existed to drive — its LOCALIZED text (title,
    // synopsis, countries, genres, poster) is exactly the wrong-language content we came
    // to replace, so it is not `carried` as a fallback. Letting it
    // through while stamping the slot with the new language would seal the row
    // in: it reads as correctly-localised, and the reaper never looks again.
    // Language-neutral fields (runtime, year, cast, director) carry over as before.
    val carried =
      if (existingTmdbSlot.fetchedLanguageTag == tmdb.language.toLanguageTag) existingTmdbSlot
      else existingTmdbSlot.copy(title = None, synopsis = None, countries = Nil, genres = Nil, posterUrl = None)
    SourceData(
      title          = d.title.orElse(carried.title),
      originalTitle  = d.originalTitle.orElse(carried.originalTitle),
      synopsis       = d.synopsis.orElse(carried.synopsis),
      cast           = nonEmptyOr(d.cast, carried.cast),
      director       = nonEmptyOr(d.director, carried.director),
      runtimeMinutes = d.runtimeMinutes.orElse(carried.runtimeMinutes),
      releaseYear    = d.releaseYear.orElse(carried.releaseYear),
      // Canonicalise TMDB's country names in the SAME language TMDB returned
      // them (the client's deployment language): Poland folds "United States
      // of America" → "USA" to match the strings cinemas write; a non-Polish
      // deployment keeps TMDB's already-localised name ("United Kingdom") so
      // it isn't mislabelled with a Polish one.
      countries      = if (d.countries.nonEmpty) SlotFields.countries(d.countries, tmdb.language) else carried.countries,
      genres         = nonEmptyOr(d.genres, carried.genres),
      posterUrl      = d.posterUrl.orElse(carried.posterUrl),
      // Per-country age rating (TMDB's certification for the deployment country) —
      // the fallback beneath any cinema-scraped rating (MovieRecord.ageRating is
      // cinema-first). Carried forward if a re-resolve returns none.
      ageRating      = d.ageRating.orElse(carried.ageRating),
      // Stamp the language these fields were fetched in, so a slot frozen by a
      // pre-locale-fix resolve is detectable rather than silently Polish forever.
      language       = Some(tmdb.language.toLanguageTag)
    )
  }

  private def nonEmptyOr[A](fetched: Seq[A], carried: Seq[A]): Seq[A] = if (fetched.nonEmpty) fetched else carried

  /** `PinnedGateMeasures.titleSearch` (the live gate's frozen measurement: a resolver change in
   *  shadow never moves a stored row) for every distinct listing title of `row` the same film's slot
   *  has not measured yet (`carried`), over live yearless searches in TMDB's own order (each query
   *  asked once). Measured once per film and title: a re-resolve to the same film — most of them
   *  answered off the id cache — asks TMDB nothing more. A title whose searches all failed is
   *  simply not measured. */
  private def measureTitleSearches(tmdbId: Int, row: MovieRecord, carried: Seq[TitleSearch]): Seq[TitleSearch] = {
    val asked = scala.collection.mutable.HashMap.empty[String, Option[Seq[Hit]]]
    val search: String => Option[Seq[Hit]] = query => asked.getOrElseUpdate(query,
      Try(tmdb.searchAsRanked(query)).toOption.flatten.map(_.map(r => Hit(r.id, r.title, r.originalTitle, r.releaseYear, r.popularity))))
    val known    = carried.map(_.titleKey).toSet
    val listings = MovieService.titleSearchListings(row.cinemaShowings.map(_._2))
      .filterNot(l => known(PinnedGateMeasures.key(l.title)))
    (carried ++ listings.flatMap(PinnedGateMeasures.titleSearch(_, tmdbId, search))).sortBy(_.titleKey)
  }

  /** Offer one row's ratings to the enqueuer, addressed by `(title, year)`.
   *
   *  Exists because `CacheKey` is `private[services]`, so a caller outside the
   *  package cannot name a row. The
   *  fixture harness stands in for `EnrichmentReaper`'s tick, and without this it had
   *  to restate the reaper's eligibility itself — which is precisely how it came to
   *  gate every rating source on `tmdbId` while production gates IMDb on an `imdbId`
   *  and Filmweb on `tmdbId OR filmwebUrl`. Handing the row to the real enqueuer keeps
   *  that judgement in one place. */
  def enqueueRatingsFor(title: String, year: Option[Int]): Unit =
    cache.get(cache.keyOf(title, year)).foreach(record =>
      enqueueNewcomerRatings(cache.keyOf(title, year), record))

  /** `existing` carrying TMDB film `tmdbId`'s details — fetched BY ID, never a search — through the
   *  builder a resolution writes with, ratings and cinemas carried forward. `None` when TMDB could
   *  not answer. What the identity projection (phase 5) fills a film the resolver named with. */
  def withFilmDetails(existing: MovieRecord, tmdbId: Int): Option[MovieRecord] =
    tools.HttpPath.within(tools.HttpPath.Enrich)(detailsOf(tmdbId, existing).map(_(existing)))

  /** The resolution builder applied with `tmdbId`'s fetched details, the cross-reference ids falling
   *  back to `row`'s when TMDB answers that it has none. When TMDB does not answer at all (an outage,
   *  a rate limit, a timeout — not [[services.tasks.TaskWorker.failedDefinitively]]) nothing is applied: details
   *  without the film's ids would read as a film with none, and never be asked for again. */
  private def detailsOf(tmdbId: Int, row: MovieRecord): Option[MovieRecord => MovieRecord] =
    tmdb.fullDetails(tmdbId).flatMap { details =>
      Try(tmdb.externalIds(tmdbId)) match {
        case Failure(e) if !TaskWorker.failedDefinitively(e) => None
        case read =>
          val ids = read.getOrElse(TmdbClient.ExternalIds(row.imdbId, row.wikidataId))
          Some(cur => buildResolvedRecord(tmdbId, ids, details, cur))
      }
    }
}

object MovieService {

  /** The listings a film's title searches are measured for ([[MovieService.measureTitleSearches]]): one per title
   *  key, the first in a TOTAL order on everything a listing holds -- a pure function of the slots, whatever order
   *  they come in. Two venues billing "Der Fremde" under different original titles ("The Stranger", "L'Etranger")
   *  search differently, and the tie, left to the slots' map order, measured whichever one the JVM's identity-hash
   *  order put first: Germany's order passes in two JVMs disagreed on 6 films' TMDB title searches. */
  private[corpus] def titleSearchListings(slots: Seq[SourceData]): Seq[services.identity.MeasuredListing] =
    slots.map(StoredIdentityConfidence.listing).filter(_.title.trim.nonEmpty)
      .sortBy(l => (l.title, l.rawTitle.getOrElse(""), l.originalTitle.getOrElse(""), l.year.getOrElse(0), l.runtime.getOrElse(0),
                    l.directors.mkString("\u0000"), l.countries.mkString("\u0000")))
      .distinctBy(l => PinnedGateMeasures.key(l.title))
}
