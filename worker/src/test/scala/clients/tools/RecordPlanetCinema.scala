package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.pl.MsiClient
import scala.util.Try

/**
 * Records the Planet Cinema chain venues' fixtures. Every branch runs its own
 * city-subdomain install of the MSI ticketing portal (`<city>.planetcinema.pl`),
 * so all of them are served by `MsiClient` with no bespoke code. Pass venue keys
 * to record only those — re-recording a venue changes the fixture its spec row
 * pins — or none to record all:
 *
 *   sbt 'worker/Test/runMain clients.tools.RecordPlanetCinema elk'
 */
object RecordPlanetCinema {
  private val venues: Seq[(String, String, String, Cinema)] = Seq(
    ("zabrze",    "planet-cinema-zabrze",    "https://zabrze.planetcinema.pl",    KinoPlanetCinemaZabrze),
    ("zawiercie", "planet-cinema-zawiercie", "https://zawiercie.planetcinema.pl", KinoPlanetCinemaZawiercie),
    ("elk",       "planet-cinema-elk",       "https://elk.planetcinema.pl",       KinoPlanetCinemaElk),
  )

  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    venues.filter { case (key, _, _, _) => args.isEmpty || args.contains(key) }.foreach { case (key, directory, baseUrl, cinema) =>
      val n = Try(new MsiClient(new RecordingHttpFetch(directory, real), baseUrl, cinema).fetch().size)
      println(f"$key%-10s ${n.fold(e => s"FAIL ${e.getClass.getSimpleName} ${e.getMessage}", x => s"$x films")}")
    }
  }
}
