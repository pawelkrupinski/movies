package clients.tools

import clients.CinemaCityClient
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import tools.RealHttpFetch

object WriteCinemaCity extends App {
  private val kinepolis = new CinemaCityClient(new RecordingHttpFetch("cinema-city-kinepolis", new RealHttpFetch()))
  kinepolis.fetch("1081", CinemaCityKinepolis).foreach(println)

  private val plaza = new CinemaCityClient(new RecordingHttpFetch("cinema-city-plaza", new RealHttpFetch()))
  plaza.fetch("1078", CinemaCityPoznanPlaza).foreach(println)
}
