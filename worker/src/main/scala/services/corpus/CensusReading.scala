package services.corpus

import models.City

import java.time.Clock

/** The census at one instant: each corpus subset, films served by (city, scope), upcoming showtimes by city, and
 *  the widest film's cinema slots. */
final case class CensusReading(corpus: Seq[(String, Int)], served: Map[(String, String), Int], showtimes: Map[String, Int], widest: Int) {
  def subset(name: String): Int = corpus.collectFirst { case (`name`, n) => n }.getOrElse(0)
}

object CensusReading {

  /** The census of `films`, their static subsets counted already (`static`, in [[CensusSubsets.StaticNames]]' order). */
  private[corpus] def tally(films: Iterator[FilmCensus], static: Seq[Long], cities: Seq[City], clock: Clock): CensusReading = {
    val tally = CensusTally.at(cities, clock.instant())
    films.foreach(tally.add)
    CensusReading(CensusSubsets.corpus(static, tally.unresolvedScreening), tally.served.toMap, tally.upcoming.toMap, tally.widest)
  }
}
