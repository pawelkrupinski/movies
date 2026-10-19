package clients.tools

import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import tools.RealHttpFetch
import services.cinemas.pl.CinemaCityClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteCinemaCity {
  def main(args: Array[String]): Unit = {
    val kinepolis = new CinemaCityClient(new RecordingHttpFetch("cinema-city-kinepolis", new RealHttpFetch()), titles = titleNormalizer)
    kinepolis.fetch("1081", CinemaCityKinepolis).foreach(println)

    val plaza = new CinemaCityClient(new RecordingHttpFetch("cinema-city-plaza", new RealHttpFetch()), titles = titleNormalizer)
    plaza.fetch("1078", CinemaCityPoznanPlaza).foreach(println)
  }
}
