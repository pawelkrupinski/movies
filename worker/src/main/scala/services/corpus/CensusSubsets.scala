package services.corpus

import models.MovieRecord
import services.metrics.{WorkerCorpusMetrics, WorkerSourceFilmsMetrics}
import services.movies.LatestTitleYear

/** The census's series names: the corpus subsets of the [[CorpusCensus]] — those that do not move with the clock
 *  ([[Static]]) in the order of their bits in [[FilmCensus.subsets]] — and the scopes a city's films are served in. */
object CensusSubsets {
  /** Every scope a city's films are served in, and the two a tally counts. */
  val ServedScopes: Seq[String] = WorkerSourceFilmsMetrics.Scope.all
  val ServedAll: String         = WorkerSourceFilmsMetrics.Scope.All
  val ServedTomorrow: String    = WorkerSourceFilmsMetrics.Scope.Tomorrow

  val Static: IndexedSeq[(String, MovieRecord => LatestTitleYear ?=> Boolean)] = {
    import WorkerCorpusMetrics.Subset._
    IndexedSeq(
      Total         -> (_ => true),
      WithAnyRating -> (r => r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined),
      WithTmdbId    -> (_.tmdbId.isDefined),
      WithImdbId    -> (_.imdbId.isDefined),
      ImdbRating    -> (_.imdbRating.isDefined),
      RtRating      -> (_.rottenTomatoes.isDefined),
      McRating      -> (_.metascore.isDefined),
      FwRating      -> (_.filmwebRating.isDefined),
      Misresolved   -> (r => CinemaCorroboration.contradicts(r)))
  }

  /** Every corpus subset's count, in `WorkerCorpusMetrics.Subset.all`'s order: the static ones' (`static`, in
   *  [[Static]]'s order) and the unresolved films still screening. */
  def corpus(static: Seq[Long], unresolvedScreening: Int): Seq[(String, Int)] = {
    val counted = Static.indices.map(i => Static(i)._1 -> static(i).toInt) :+
      (WorkerCorpusMetrics.Subset.UnresolvedWithShowtimes -> unresolvedScreening)
    WorkerCorpusMetrics.Subset.all.map(name => name -> counted.collectFirst { case (`name`, n) => n }.getOrElse(0))
  }

  /** Each static subset's count over `subsets`' bits, film by film. */
  def counted(subsets: Iterable[Int]): IndexedSeq[Long] =
    Static.indices.map(i => subsets.count(bits => (bits & (1 << i)) != 0).toLong)
}
