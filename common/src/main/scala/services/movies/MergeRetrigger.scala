package services.movies

import models.MovieRecord

/** One enrichment a merge can invalidate by changing the input field(s) it
 *  resolves from. Mapped to a concrete worker task + freshness kind by the
 *  worker-side [[EnrichmentRetrigger]] impl. */
sealed trait RetriggerKind
object RetriggerKind {
  case object ResolveImdbId extends RetriggerKind
  case object ImdbRating    extends RetriggerKind
  case object FilmwebRating extends RetriggerKind
  case object RtRating      extends RetriggerKind
  case object McRating      extends RetriggerKind

  /** The title/tmdbId-driven ratings (queried by search title, not imdbId). */
  val titleRatings: Set[RetriggerKind] = Set(FilmwebRating, RtRating, McRating)
}

/** Sink the cache calls after a merge to re-kick the enrichments whose inputs
 *  changed. A narrow port (DIP): the cache (in `common`) stays unaware of the
 *  task queue + freshness store; the worker wires the real
 *  `QueueEnrichmentRetrigger`, tests a capturing fake. Default no-op for the
 *  unit tests + non-worker builds that drive the cache directly. */
trait EnrichmentRetrigger {
  def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit
}

object EnrichmentRetrigger {
  val noop: EnrichmentRetrigger = (_, _, _) => ()
}

/**
 * The PURE decision half of "re-kick enrichment after a merge": given the row
 * that stood at the surviving key BEFORE a merge pulled other rows in, and the
 * merged record now stored there, which enrichments did the merge invalidate by
 * changing an INPUT they resolve from?
 *
 * Per case — never one blanket "re-fetch all" — so a merge that only touched,
 * say, the imdbId doesn't re-burst all four rating sources. The decision is
 * AGGRESSIVE on inputs: an input change re-kicks the enrichment even if its
 * output is already present, because the present value was computed for the
 * pre-merge inputs and may now be wrong (e.g. a Filmweb rating fetched under a
 * title the merge has since corrected). TMDB resolution is not one of them:
 * the identity projection resolves a film from its listings, not on a merge.
 */
object MergeRetrigger {

  def changedEnrichments(
    before:    MovieRecord, beforeKey: CacheKey,
    after:     MovieRecord, afterKey:  CacheKey
  ): Set[RetriggerKind] = {
    // Compare the SANITIZED title, not the raw spelling. A pure case/punctuation
    // re-spelling (sanitize-equal) is not a real change of the film's identity:
    // every enrichment lookup folds the key to `sanitize`, and Filmweb/RT/Metacritic
    // match case- and diacritic-insensitively, so re-fetching under the re-cased
    // title would return the same rating. Comparing the RAW spelling re-kicked all
    // three title-ratings on EVERY boot for any row whose hydrate key (rebuilt as
    // `displayTitle` by `StoredMovieRecord.fromStorage`) differed only in
    // case/punctuation from the settle's `minSpelling` canonical — e.g. "Federico
    // Fellini: Słodkie życie" vs "…SŁODKIE ŻYCIE". That was the ~83-retrigger/boot
    // rating spike. Only a genuine title change
    // (different `sanitize`) or a year change re-kicks now.
    // Read each key's OWN normalised form rather than re-sanitizing its title: a
    // CacheKey now carries the identity its builder computed, so this compares
    // what the corpus actually keyed the rows under instead of re-deriving it
    // under whichever rules happened to be in scope here.
    val titleOrYearChanged   =
      beforeKey.normalized != afterKey.normalized || beforeKey.year != afterKey.year
    // Compare the RESOLVER original-title set (TMDB + IMDb + Filmweb), not the
    // TMDB-only display `originalTitle`: a Filmweb-supplied original title is a new
    // search term the TMDB/IMDb lookups mine, so it must re-kick them the same way
    // a TMDB one does. See MovieRecord.resolverOriginalTitles.
    val originalTitleChanged = before.resolverOriginalTitles != after.resolverOriginalTitles
    val directorChanged      = before.director != after.director
    val tmdbIdChanged        = before.tmdbId != after.tmdbId
    val tmdbNoMatchChanged   = before.tmdbNoMatch != after.tmdbNoMatch
    val imdbIdChanged        = before.imdbId != after.imdbId
    val searchTitleChanged   = before.searchTitle != after.searchTitle
    // The inputs the search-title-driven ratings (Filmweb/RT/Metacritic) resolve
    // from. tmdbId isn't a query input for them, but a tmdbId change means the
    // row's film identity firmed up, so a re-fetch under the now-known title is
    // warranted.
    val ratingInputChanged   = titleOrYearChanged || searchTitleChanged || tmdbIdChanged

    val builder = Set.newBuilder[RetriggerKind]
    if ((tmdbIdChanged || tmdbNoMatchChanged || searchTitleChanged || titleOrYearChanged || directorChanged || originalTitleChanged)
        && (after.tmdbId.isDefined || after.tmdbNoMatch) && after.imdbId.isEmpty)
      builder += RetriggerKind.ResolveImdbId
    // A row with NO TMDB id holds an IMDb id only as good as the facts it was searched by: looked up before
    // a sibling's year or director merged in, US "Volcanoes" took IMDb's first "Volcanoes" (2009), after
    // it the 2018 film — the order the listings arrived in decided. So such a row searches again when a
    // fact the search reads grows (`ImdbIdResolver` writes only a different answer).
    val reportedYearsChanged = before.cinemaData.values.flatMap(_.releaseYear).toSet != after.cinemaData.values.flatMap(_.releaseYear).toSet
    if (after.tmdbId.isEmpty && after.tmdbNoMatch && after.imdbId.isDefined && (directorChanged || originalTitleChanged || reportedYearsChanged))
      builder += RetriggerKind.ResolveImdbId
    if (imdbIdChanged && after.imdbId.isDefined)
      builder += RetriggerKind.ImdbRating
    // Search-title ratings only for a TMDB-CONCLUDED row (resolved), matching the
    // normal pipeline that enqueues ratings off the resolution event — an
    // unresolved row has no firm film to rate yet.
    if (ratingInputChanged && after.tmdbId.isDefined)
      builder ++= RetriggerKind.titleRatings
    builder.result()
  }
}
