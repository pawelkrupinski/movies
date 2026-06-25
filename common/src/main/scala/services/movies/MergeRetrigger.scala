package services.movies

import models.MovieRecord

/** One enrichment a merge can invalidate by changing the input field(s) it
 *  resolves from. Mapped to a concrete worker task + freshness kind by the
 *  worker-side [[EnrichmentRetrigger]] impl. */
sealed trait RetriggerKind
object RetriggerKind {
  case object ResolveTmdb   extends RetriggerKind
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
 * title the merge has since corrected). The one exception is TMDB resolution:
 * it only re-fires for an UNRESOLVED (or no-match) row, because re-resolving a
 * row that already carries a tmdbId merely because the merge re-spelled its
 * canonical title risks RE-POINTING it to a different same-title film.
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
    // rating spike (`CanonicalizeRetriggerFlapSpec`). Only a genuine title change
    // (different `sanitize`) or a year change re-kicks now.
    val titleOrYearChanged   =
      TitleNormalizer.sanitize(beforeKey.cleanTitle) != TitleNormalizer.sanitize(afterKey.cleanTitle) ||
      beforeKey.year != afterKey.year
    val originalTitleChanged = before.originalTitle != after.originalTitle
    val directorChanged      = before.director != after.director
    val tmdbIdChanged        = before.tmdbId != after.tmdbId
    val imdbIdChanged        = before.imdbId != after.imdbId
    val searchTitleChanged   = before.searchTitle != after.searchTitle
    // The inputs the search-title-driven ratings (Filmweb/RT/Metacritic) resolve
    // from. tmdbId isn't a query input for them, but a tmdbId change means the
    // row's film identity firmed up, so a re-fetch under the now-known title is
    // warranted.
    val ratingInputChanged   = titleOrYearChanged || searchTitleChanged || tmdbIdChanged

    val builder = Set.newBuilder[RetriggerKind]
    if ((titleOrYearChanged || originalTitleChanged || directorChanged) && after.tmdbId.isEmpty && !after.tmdbNoMatch)
      builder += RetriggerKind.ResolveTmdb
    if ((tmdbIdChanged || searchTitleChanged || titleOrYearChanged || directorChanged || originalTitleChanged)
        && after.tmdbId.isDefined && after.imdbId.isEmpty)
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
