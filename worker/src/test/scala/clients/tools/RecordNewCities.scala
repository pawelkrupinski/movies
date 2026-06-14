package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the chain scrapes (Cinema City, Multikino, Helios) for the eight
 * smaller cities added on top of the 11-city base — Częstochowa, Radom,
 * Sosnowiec, Toruń, Kielce, Rzeszów, Gliwice, Zabrze — into the `new-cities`
 * fixture corpus, plus Kino Zorza (Rzeszów's bespoke art-house) into its own
 * dir. Replayed by `clients.newcities.NewCitiesChainScrapeSpec` and
 * `clients.kino_zorza.KinoZorzaClientSpec`. Scrape-only (no enrichment). Run:
 *
 *   sbt 'worker/Test/runMain clients.tools.RecordNewCities'
 *
 * Helios bakes the capture date into its REST URLs, so the chain spec pins
 * `today` to the date this recorder ran.
 */
object RecordNewCities {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    val rec  = new RecordingHttpFetch("new-cities", real)
    val cc   = new CinemaCityClient(rec)

    def report(label: String)(n: => Int): Unit =
      println(s"$label: ${Try(n).fold(e => s"FAIL ${e.getMessage}", x => s"$x films")}")

    val cinemaCity = Seq(
      "1089" -> CinemaCityCzestochowaJurajska, "1075" -> CinemaCityCzestochowaWolnosc,
      "1083" -> CinemaCitySosnowiec,
      "1077" -> CinemaCityTorunCzerwonaDroga, "1093" -> CinemaCityTorunPlaza,
      "1085" -> CinemaCityGliwice,
    )
    cinemaCity.foreach { case (id, c) =>
      report(s"Cinema City ${c.displayName} ($id)")(cc.fetch(id, c).size)
    }

    val multikino = Seq(
      "0026" -> MultikinoRadom, "0029" -> MultikinoKielce,
      "0028" -> MultikinoRzeszow, "0003" -> MultikinoZabrze,
    )
    multikino.foreach { case (id, c) =>
      report(s"Multikino ${c.displayName} ($id)")(new MultikinoClient(rec, id, c).fetch().size)
    }

    Seq(HeliosNuxt.Radom, HeliosNuxt.Sosnowiec, HeliosNuxt.Kielce, HeliosNuxt.Rzeszow).foreach { config =>
      report(s"Helios ${config.cinema.displayName}")(new HeliosClient(rec, config).fetch().size)
    }

    report("Kino Zorza (Rzeszów)")(
      new KinoZorzaClient(new RecordingHttpFetch("kino-zorza", real), KinoZorza).fetch().size
    )
  }
}
