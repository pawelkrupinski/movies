package services.movies

import models.MovieRecord

/** One enrichment a merge can invalidate by changing the input field(s) it
 *  resolves from. Mapped to a concrete worker task + freshness kind by the
 *  worker-side [[EnrichmentRetrigger]] impl. */
sealed abstract class RetriggerKind(
  /** The record fields this enrichment writes, named: what moves when it is asked again and answers otherwise. */
  val writes: MovieRecord => Seq[(String, Any)])
object RetriggerKind {
  case object ResolveImdbId extends RetriggerKind(r => Seq("imdbId" -> r.imdbId))
  case object ImdbRating    extends RetriggerKind(r => Seq("imdbRating" -> r.imdbRating))
  case object FilmwebRating extends RetriggerKind(r => Seq("filmwebUrl" -> r.filmwebUrl, "filmwebRating" -> r.filmwebRating))
  case object RtRating      extends RetriggerKind(r => Seq("rottenTomatoesUrl" -> r.rottenTomatoesUrl, "rottenTomatoes" -> r.rottenTomatoes))
  case object McRating      extends RetriggerKind(r => Seq("metacriticUrl" -> r.metacriticUrl, "metascore" -> r.metascore))

  lazy val all: Seq[RetriggerKind] = Seq(ResolveImdbId, ImdbRating, FilmwebRating, RtRating, McRating)

  /** The title/tmdbId-driven ratings (queried by search title, not imdbId). */
  val titleRatings: Set[RetriggerKind] = Set(FilmwebRating, RtRating, McRating)
}

/** Sink the cache calls after a merge to re-kick the enrichments whose inputs
 *  changed. A narrow port (DIP): the cache stays unaware of the task queue +
 *  freshness store; the wiring binds the real `QueueEnrichmentRetrigger`,
 *  tests a capturing fake. Default no-op for the unit tests that drive the
 *  cache directly. */
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

  /** One input an enrichment resolves from, named, and read off a (record, key) pair: what
   *  [[changedInputs]] compares, and what a diagnosis prints the before / after of. */
  final case class Input(name: String, read: (MovieRecord, CacheKey) => LatestTitleYear ?=> Any)

  object Input {
    // Read each key's OWN normalised form rather than re-sanitizing its title: a
    // CacheKey carries the identity its builder computed, so this compares what the
    // corpus actually keyed the rows under. The SANITIZED title, not the raw spelling:
    // a pure case/punctuation re-spelling is not a real change of the film's identity
    // (every lookup folds the key to `sanitize`, and Filmweb/RT/Metacritic match case-
    // and diacritic-insensitively). Comparing the RAW spelling re-kicked all three
    // title-ratings on EVERY boot for any row whose hydrate key differed only in
    // case/punctuation from the settle's `minSpelling` canonical — e.g. "Federico
    // Fellini: Słodkie życie" vs "…SŁODKIE ŻYCIE", the ~83-retrigger/boot spike.
    val TitleOrYear = Input("title/year", (_, k) => (k.normalized, k.year))
    // The RESOLVER original-title set (TMDB + IMDb + Filmweb), not the TMDB-only
    // display `originalTitle`: a Filmweb-supplied original title is a new search term
    // the TMDB/IMDb lookups mine. See MovieRecord.resolverOriginalTitles.
    val ResolverOriginalTitles = Input("resolver original titles", (r, _) => r.resolverOriginalTitles)
    val Director     = Input("director", (r, _) => r.director)
    val TmdbId       = Input("tmdbId", (r, _) => r.tmdbId)
    val TmdbNoMatch  = Input("tmdbNoMatch", (r, _) => r.tmdbNoMatch)
    val ImdbId       = Input("imdbId", (r, _) => r.imdbId)
    val SearchTitle  = Input("searchTitle", (r, _) => r.searchTitle)
    val ReportedYears = Input("cinema-reported years", (r, _) => r.cinemaData.values.flatMap(_.releaseYear).toSet)
    val OriginalTitle = Input("originalTitle", (r, _) => r.originalTitle)
    val CinemaDirectors      = Input("cinema directors", (r, _) => r.evidence.directors)
    val CinemaOriginalTitles = Input("cinema original titles", (r, _) => r.evidence.originalTitles)
    val CinemaYears          = Input("cinema years", (r, _) => r.evidence.years)
  }
  import Input._

  def changedEnrichments(
    before:    MovieRecord, beforeKey: CacheKey,
    after:     MovieRecord, afterKey:  CacheKey
  )(using LatestTitleYear): Set[RetriggerKind] = changedInputs(before, beforeKey, after, afterKey).keySet

  /** Each enrichment the change invalidates, with the inputs of it that changed: the one table of which
   *  enrichment reads which fields, read by the merge path ([[changedEnrichments]]) and by the convergence
   *  harness's next-day diagnosis (which enrichment a day-one write should have re-asked, and why). */
  def changedInputs(
    before:    MovieRecord, beforeKey: CacheKey,
    after:     MovieRecord, afterKey:  CacheKey
  )(using LatestTitleYear): Map[RetriggerKind, Set[Input]] = {
    val changed: Input => Boolean = { val memo = scala.collection.mutable.HashMap.empty[Input, Boolean]
      input => memo.getOrElseUpdate(input, input.read(before, beforeKey) != input.read(after, afterKey)) }
    val builder = scala.collection.mutable.HashMap.empty[RetriggerKind, Set[Input]]
    def kick(kinds: Iterable[RetriggerKind], inputs: Seq[Input], when: Boolean): Unit = {
      val moved = inputs.filter(changed)
      if (moved.nonEmpty && when) kinds.foreach(kind => builder.update(kind, builder.getOrElse(kind, Set.empty) ++ moved))
    }
    kick(Seq(RetriggerKind.ResolveImdbId), Seq(TmdbId, TmdbNoMatch, SearchTitle, TitleOrYear, Director, ResolverOriginalTitles),
      (after.tmdbId.isDefined || after.tmdbNoMatch) && after.imdbId.isEmpty)
    // A row with NO TMDB id holds an IMDb id only as good as the facts it was searched by: looked up before
    // a sibling's year or director merged in, US "Volcanoes" took IMDb's first "Volcanoes" (2009), after
    // it the 2018 film — the order the listings arrived in decided. So such a row searches again when a
    // fact the search reads grows (`ImdbIdResolver` writes only a different answer).
    kick(Seq(RetriggerKind.ResolveImdbId), Seq(Director, ResolverOriginalTitles, ReportedYears),
      after.tmdbId.isEmpty && after.tmdbNoMatch && after.imdbId.isDefined)
    kick(Seq(RetriggerKind.ImdbRating), Seq(ImdbId), after.imdbId.isDefined)
    // The inputs the search-title-driven ratings (Filmweb/RT/Metacritic) resolve from, only for a
    // TMDB-CONCLUDED row (resolved), matching the normal pipeline that enqueues ratings off the resolution
    // event — an unresolved row has no firm film to rate yet. tmdbId isn't a query input for them, but a
    // tmdbId change means the row's film identity firmed up, so a re-fetch under the now-known title is
    // warranted.
    kick(RetriggerKind.titleRatings, Seq(TitleOrYear, SearchTitle, TmdbId), after.tmdbId.isDefined)
    // Filmweb's search reads two inputs more (FilmwebRatings.resolveWithTmdbHints, its link-cache key): the original
    // title it falls back to and the directors the cinemas credit, which confirm a page whose year is off by one.
    // A miss is remembered under the key it was asked by, so a film asked before a cinema's director reached it kept
    // the miss while a whole resolve, all of it there from the start, took the page: Verify scrape recording #23's
    // "Cienie zapomnianych przodków" (Filmweb 1964, TMDB 1965) and "Plemię", asked under TMDB's spellings alone,
    // found once Filmweb's own ("Siergiej Paradżanow") had come in.
    kick(Seq(RetriggerKind.FilmwebRating), Seq(OriginalTitle, CinemaDirectors), after.tmdbId.isDefined)
    // A film TMDB has no record of is searched on every rating site by what its cinemas publish alone
    // (services.enrichment.TmdbLessRatingLinks: their original titles, directors and years), never cached, and asked
    // once: Verify scrape recording #26's "Afrykanska Przygoda" (2007, no TMDB film), asked before those facts were all
    // in, missed, and only the next day's pass, asking again, found Filmweb's "Afrykańska przygoda 3D - safari nad
    // Okavango". So each site is asked again when one of them changes.
    kick(RetriggerKind.titleRatings, Seq(CinemaDirectors, CinemaOriginalTitles, CinemaYears),
      after.tmdbId.isEmpty && TmdbLessEligibility.eligible(after))
    builder.toMap
  }
}
