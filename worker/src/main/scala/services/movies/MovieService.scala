package services.movies

import clients.TmdbClient
import play.api.Logging
import services.cinemas.CountryNames
import services.events.{EventBus, ImdbIdMissing}
import services.freshness.{FreshnessKind, FreshnessStore, InMemoryFreshnessStore}
import services.resolution.TmdbBasis
import services.tasks.RatingTasks

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
  // Production injects the SHARED store the rating handlers read; tests default
  // to a throwaway in-memory one (the stamp is observability, not correctness).
  freshness: FreshnessStore = new InMemoryFreshnessStore,
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
   *  (for the first-rating delay metric), kick IMDb-id recovery for a TMDB-only hit (`ImdbIdMissing`
   *  → `ImdbIdResolver`), and IMMEDIATELY enqueue the now-eligible rating tasks (`enqueueNewcomerRatings`)
   *  so its ratings don't wait for the `EnrichmentReaper`'s next tick. A film resolved without an imdbId
   *  enqueues only its non-IMDb ratings now; IMDb follows once `ImdbIdResolver` lands the id. */
  def announceResolvedNewMovie(key: CacheKey, record: MovieRecord): Unit =
    if (record.tmdbId.isDefined) {
      publishTmdbOutcome(key, record)
      enqueueNewcomerRatings(key, record)
    } else if (record.tmdbNoMatch && record.imdbId.isEmpty) {
      // TMDB found nothing, so the match path above never published `ImdbIdMissing`
      // and the film would only ever get an id from the once-daily OMDb sweep. Kick
      // the same id-recovery chain HERE too: `ImdbIdResolver` runs its full ladder
      // (IMDb suggestion → director → Filmweb/Wikidata → Letterboxd → OMDb → Wikidata-title
      // → Cinemeta) against the freshly-folded cached row. This is what lets the
      // TMDB-less long tail (niche/foreign titles — the Flicks catalogue in
      // particular) land an imdbId → rating AND a resolved year that stabilises its
      // read-model key, instead of waiting hours for the sweep. The id is the only
      // effect; ratings follow once the reaper sees the now-eligible row.
      val searchTitle = record.searchTitle.orElse(record.originalTitle).getOrElse(cache.normalizer.searchQuery(key.cleanTitle))
      logger.info(s"TMDB: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match; publishing ImdbIdMissing(search='$searchTitle') to attempt id recovery")
      bus.publish(ImdbIdMissing(key.cleanTitle, key.year, searchTitle))
    }

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

  /** Build the resolved `MovieRecord` from a TMDB hit + the row's `existing`
   *  record — pure (no cache, no lock), so the movies path (then `settleResolved`)
   *  and the staging promoter (then `stagingRepository.upsert`) share ONE definition of
   *  how a resolution writes the TMDB-side fields + `Tmdb` slot while carrying the
   *  cinema-side data and score fields forward. */
  private def buildResolvedRecord(
    tmdbId:      Int,
    hit:         Option[TmdbClient.SearchResult],
    externalIds: TmdbClient.ExternalIds,
    detailsOpt:  Option[TmdbClient.FullDetails],
    existing:    MovieRecord,
    // None when the resolution came off the id cache, so the conclusion's real basis
    // is unknown here; the row then KEEPS the basis it already recorded rather than
    // being downgraded to the weakest one. See `resolveTmdbId`.
    basis:       Option[TmdbBasis]
  ): MovieRecord = {
    // Preserve the previously-known `imdbId`/`wikidataId` when TMDB resolved the
    // same film (same `tmdbId`) but momentarily dropped a cross-reference —
    // happens for very recent releases and occasional TMDB data hiccups. A
    // DIFFERENT tmdbId accepts the new film's ids (even None) so a stale id
    // can't leak across.
    val sameFilm         = existing.tmdbId.contains(tmdbId)
    val resolvedImdbId   = externalIds.imdbId.orElse(if (sameFilm) existing.imdbId else None)
    val resolvedWikidata = externalIds.wikidataId.orElse(if (sameFilm) existing.wikidataId else None)
    // Every rating url and score describes a FILM, so they follow the film.
    // Carrying them forward unconditionally meant a corrected row kept the WRONG
    // film's numbers until each source's own refresh cadence came round: prod
    // served Michel Franco's "Dreams" with the Norwegian film's
    // `/movie/dreams-drommer` and metascore 81 long after the tmdbId was fixed.
    //
    // The trigger is a film that demonstrably CHANGED — not merely "not the same
    // one". A row resolving for the FIRST time has no previous tmdbId, so there is
    // nothing stale to drop and a rating a refresher already wrote must survive
    // (the since-deleted `TmdbCarryForwardReadFailureSpec`). Once cleared, the `*Ratings` enrichers
    // re-resolve from the new identity.
    val differentFilm = existing.tmdbId.exists(_ != tmdbId)
    def ifSameFilm[A](value: Option[A]): Option[A] = if (differentFilm) None else value
    // Carry the cinema-side fields forward — the TMDB stage doesn't own cinema
    // data; without this a fresh resolve would wipe every cinema's slot.
    val carriedData      = existing.data
    // Fetch the full TMDB record in a single round-trip so the SourceData (Tmdb)
    // slot carries the Polish synopsis, director, cast, runtime, year, countries
    // and poster — not just the search-hit-shape fields. On a fetch failure fall
    // back to the search-hit shape so the row at least keeps title + year.
    val existingTmdbSlot = carriedData.getOrElse(Tmdb, SourceData())
    // The search hit's title/originalTitle/year are the fallback when the full
    // details fetch fails. `hit` is present on a fresh resolution and None on a
    // cache hit (the cache stores only the id) — in that rare double case the
    // slot keeps whatever it already had, and the next resolution fills it.
    val hitTitle = hit.map(_.title).filter(_.nonEmpty)
    // TMDB's English release title (en-US `title`, via the same `details` call
    // MC/RT use). For a non-Latin-original film whose Polish `title` and
    // `originalTitle` both differ from the English title a cinema lists it under
    // ("Left-Handed Girl"), this is the alias that folds the English-keyed
    // duplicate onto the Polish-titled row — see `MovieRecord.tmdbTitleAliases`.
    val englishTitle = tmdb.englishTitle(tmdbId).orElse(existingTmdbSlot.englishTitle)
    // Whether the slot we're merging onto was fetched in the language we're
    // fetching in NOW. When it wasn't — the stale-language re-resolve
    // the since-deleted `UnresolvedTmdbReaper` existed to drive — its LOCALIZED text (title,
    // synopsis, genres, poster) is exactly the wrong-language content we came
    // to replace, so it must not survive as an `.orElse` fallback. Letting it
    // through while stamping the slot with the new language would seal the row
    // in: it reads as correctly-localised, and the reaper never looks again.
    // Language-neutral fields (runtime, year, cast, director) carry over as before.
    val existingMatchesLanguage = existingTmdbSlot.fetchedLanguageTag == tmdb.language.toLanguageTag
    def carriedLocalized[A](existing: Option[A]): Option[A] =
      if (existingMatchesLanguage) existing else None
    // Where the film stands in each of the row's listing titles' own yearless TMDB searches —
    // the identity evidence the rating gate scores (`StoredIdentityConfidence`). Kept while the
    // film stays the same; a different film is measured afresh.
    val titleSearches = measureTitleSearches(tmdbId, existing,
      carried = if (differentFilm) Nil else existingTmdbSlot.titleSearches)
    val tmdbSlot = detailsOpt match {
      case Some(d) => SourceData(
        title          = d.title.orElse(hitTitle).orElse(carriedLocalized(existingTmdbSlot.title)),
        originalTitle  = d.originalTitle.orElse(hit.flatMap(_.originalTitle)).orElse(existingTmdbSlot.originalTitle),
        englishTitle   = englishTitle,
        synopsis       = d.synopsis.orElse(carriedLocalized(existingTmdbSlot.synopsis)),
        cast           = if (d.cast.nonEmpty) d.cast else existingTmdbSlot.cast,
        director       = if (d.director.nonEmpty) d.director else existingTmdbSlot.director,
        runtimeMinutes = d.runtimeMinutes.orElse(existingTmdbSlot.runtimeMinutes),
        releaseYear    = d.releaseYear.orElse(hit.flatMap(_.releaseYear)).orElse(existingTmdbSlot.releaseYear),
        // Canonicalise TMDB's country names in the SAME language TMDB returned
        // them (the client's deployment language): Poland folds "United States
        // of America" → "USA" to match the strings cinemas write; a non-Polish
        // deployment keeps TMDB's already-localised name ("United Kingdom") so
        // it isn't mislabelled with a Polish one.
        countries      = if (d.countries.nonEmpty) d.countries.map(c => CountryNames.canonical(c, tmdb.language)).distinct
                         else if (existingMatchesLanguage) existingTmdbSlot.countries else Seq.empty,
        genres         = if (d.genres.nonEmpty) d.genres
                         else if (existingMatchesLanguage) existingTmdbSlot.genres else Seq.empty,
        posterUrl      = d.posterUrl.orElse(carriedLocalized(existingTmdbSlot.posterUrl)),
        // Per-country age rating (TMDB's certification for the deployment country) —
        // the fallback beneath any cinema-scraped rating (MovieRecord.ageRating is
        // cinema-first). Carried forward if a re-resolve returns none.
        ageRating      = d.ageRating.orElse(existingTmdbSlot.ageRating),
        // Stamp the language these fields were fetched in, so a slot frozen by a
        // pre-locale-fix resolve is detectable rather than silently Polish forever.
        language       = Some(tmdb.language.toLanguageTag),
        titleSearches  = titleSearches
      )
      case None => existingTmdbSlot.copy(
        title         = hitTitle.orElse(existingTmdbSlot.title),
        originalTitle = hit.flatMap(_.originalTitle).orElse(existingTmdbSlot.originalTitle),
        englishTitle  = englishTitle,
        releaseYear   = hit.flatMap(_.releaseYear).orElse(existingTmdbSlot.releaseYear),
        titleSearches = titleSearches
      )
    }
    MovieRecord(
      imdbId            = resolvedImdbId,
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
      tmdbBasis         = basis.map(_.toString).orElse(existing.tmdbBasis),
      wikidataId        = resolvedWikidata,
      metacriticUrl     = ifSameFilm(existing.metacriticUrl),
      rottenTomatoesUrl = ifSameFilm(existing.rottenTomatoesUrl),
      // A resolve clears any prior `tmdbNoMatch` (default `false` here); carry a
      // pending deferred-detail fetch forward so resolving TMDB first doesn't
      // prematurely mark the row detail-done.
      detailPending     = existing.detailPending,
      data              = carriedData + ((Tmdb: Source) -> tmdbSlot)
    )
  }

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
    val listings = row.cinemaShowings.map { case (_, slot) => StoredIdentityConfidence.listing(slot) }
      .filter(_.title.trim.nonEmpty).sortBy(l => (l.title, l.rawTitle.getOrElse(""))).distinctBy(l => PinnedGateMeasures.key(l.title))
      .filterNot(l => known(PinnedGateMeasures.key(l.title)))
    (carried ++ listings.flatMap(PinnedGateMeasures.titleSearch(_, tmdbId, search))).sortBy(_.titleKey)
  }

  /** Offer one row's ratings to the enqueuer, addressed by `(title, year)`.
   *
   *  Exists for the same reason the [[retryResolve]] overload above does: `CacheKey`
   *  is `private[services]`, so a caller outside the package cannot name a row. The
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
    detailsOf(tmdbId, existing).map(_(existing))

  /** The resolution builder applied with `tmdbId`'s fetched details, the cross-reference ids falling
   *  back to `row`'s when TMDB answers that it has none. When TMDB does not answer at all (an outage,
   *  a rate limit, a timeout — not [[MovieService.failedDefinitively]]) nothing is applied: details
   *  without the film's ids would read as a film with none, and never be asked for again. */
  private def detailsOf(tmdbId: Int, row: MovieRecord): Option[MovieRecord => MovieRecord] =
    tmdb.fullDetails(tmdbId).flatMap { details =>
      Try(tmdb.externalIds(tmdbId)) match {
        case Failure(e) if !MovieService.failedDefinitively(e) => None
        case read =>
          val ids = read.getOrElse(TmdbClient.ExternalIds(row.imdbId, row.wikidataId))
          Some(cur => buildResolvedRecord(tmdbId, hit = None, ids, Some(details), cur, basis = None))
      }
    }
}

object MovieService {

  /** A failed TMDB lookup that is an ANSWER about the film, not a read that did not happen:
   *  TMDB's own "not found" (`ReadOutcome.isAbsent`), a deterministic failure a retry would
   *  replay (`TaskWorker.isDeterministic`), or a missing replay fixture — which only the
   *  fixture harness throws, and treats as permanent (see `TmdbClient.isTransient`). Anything
   *  else — an outage, a rate limit, a timeout, a 401 on a bad key — is about TMDB, never the
   *  film, and must not conclude it. */
  def failedDefinitively(failure: Throwable): Boolean =
    tools.ReadOutcome.isAbsent(failure) || services.tasks.TaskWorker.isDeterministic(failure) ||
      failure.isInstanceOf[java.io.FileNotFoundException]
}
