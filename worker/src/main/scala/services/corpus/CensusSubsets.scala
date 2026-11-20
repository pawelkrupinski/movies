package services.corpus

import models.MovieRecord
import services.metrics.{WorkerCorpusMetrics, WorkerSourceFilmsMetrics}
import services.movies.LatestTitleYear

/** The census's series names: the corpus subsets of the [[CorpusCensus]] — those that do not move with the clock
 *  ([[StaticNames]]) in the order of their bits in [[FilmCensus.subsets]] — and the scopes a city's films are served in. */
object CensusSubsets {
  /** Every scope a city's films are served in, and the two a tally counts. */
  val ServedScopes: Seq[String] = WorkerSourceFilmsMetrics.Scope.all
  val ServedAll: String         = WorkerSourceFilmsMetrics.Scope.All
  val ServedTomorrow: String    = WorkerSourceFilmsMetrics.Scope.Tomorrow

  /** The corpus subsets that do not move with the clock, in the order of their bits in [[FilmCensus.subsets]]; what
   *  each counts is [[StaticCounts]]. */
  val StaticNames: IndexedSeq[String] = {
    import WorkerCorpusMetrics.Subset._
    IndexedSeq(Total, WithAnyRating, WithTmdbId, WithImdbId, ImdbRating, RtRating, McRating, FwRating, Misresolved)
  }

  /** What each of [[StaticNames]] counts, in its order: the bits of [[FilmCensus.subsets]]. */
  val StaticCounts: IndexedSeq[MovieRecord => LatestTitleYear ?=> Boolean] = {
    val counts: IndexedSeq[MovieRecord => LatestTitleYear ?=> Boolean] = IndexedSeq(
      _ => true,
      r => r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined,
      _.tmdbId.isDefined,
      _.imdbId.isDefined,
      _.imdbRating.isDefined,
      _.rottenTomatoes.isDefined,
      _.metascore.isDefined,
      _.filmwebRating.isDefined,
      r => CinemaCorroboration.contradicts(r))
    require(counts.size == StaticNames.size, "a static subset without its name, or a name without its count")
    counts
  }

  /** Every corpus subset's count, in `WorkerCorpusMetrics.Subset.all`'s order: the static ones' (`static`, in
   *  [[StaticNames]]' order) and the unresolved films still screening. */
  def corpus(static: Seq[Long], unresolvedScreening: Int): Seq[(String, Int)] = {
    val counted = StaticNames.indices.map(i => StaticNames(i) -> static(i).toInt) :+
      (WorkerCorpusMetrics.Subset.UnresolvedWithShowtimes -> unresolvedScreening)
    WorkerCorpusMetrics.Subset.all.map(name => name -> counted.collectFirst { case (`name`, n) => n }.getOrElse(0))
  }

  /** Each static subset's count over `subsets`' bits, film by film. */
  def counted(subsets: Iterable[Int]): IndexedSeq[Long] =
    StaticNames.indices.map(i => subsets.count(bits => (bits & (1 << i)) != 0).toLong)
}
