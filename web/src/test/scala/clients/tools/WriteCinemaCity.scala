package clients.tools

import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import services.cinemas.CinemaCityClient
import tools.RealHttpFetch

object WriteCinemaCity {
  def main(args: Array[String]): Unit = {
    val kinepolis = new CinemaCityClient(new RecordingHttpFetch("cinema-city-kinepolis", new RealHttpFetch()))
    kinepolis.fetch("1081", CinemaCityKinepolis).foreach(println)

    val plaza = new CinemaCityClient(new RecordingHttpFetch("cinema-city-plaza", new RealHttpFetch()))
    plaza.fetch("1078", CinemaCityPoznanPlaza).foreach(println)
  }
}
