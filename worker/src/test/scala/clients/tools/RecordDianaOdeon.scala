package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import java.time.LocalDate
import scala.util.Try

/** One-shot: record Kino Diana (Prudnik, bespoke eventCalendar JSON) and Kino
 *  ODEON (Sochaczew, whose Eurobilet backend serves the standard MSI month
 *  page) off Filmweb into the 08-06-2026 whole-corpus fixture set. Run:
 *    sbt 'worker/Test/runMain clients.tools.RecordDianaOdeon'
 *  then delete + regenerate the snapshots. today is pinned to the capture date. */
object RecordDianaOdeon {
  def main(args: Array[String]): Unit = {
    val rec   = new RecordingHttpFetch("08-06-2026", new RealHttpFetch())
    val today = LocalDate.of(2026, 6, 8)
    def rep(label: String)(n: => Int): Unit =
      println(f"$label%-16s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName}: ${e.getMessage}", x => s"$x films")}")
    rep("KinoDiana")(new KinoDianaClient(rec, KinoDiana).fetch().size)
    rep("KinoODEON")(new MsiClient(rec, "https://kinoodeon.eurobilet.pl", KinoODEON, today).fetch().size)
  }
}
