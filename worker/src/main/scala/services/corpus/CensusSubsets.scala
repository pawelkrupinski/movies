package services.corpus

import models.MovieRecord
import services.metrics.WorkerCorpusMetrics
import services.movies.LatestTitleYear

/** The corpus subsets of the [[CorpusCensus]] that do not move with the clock, in the order of their bits in
 *  [[FilmCensus.subsets]]. */
object CensusSubsets {
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
