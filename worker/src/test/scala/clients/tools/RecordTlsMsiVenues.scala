package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import java.time.LocalDate
import scala.util.Try

/** One-shot: record the MSI own-site responses of the five nazwa.pl venues newly
 *  unblocked by bundling `Certum EC-384 CA` (Kryterium Koszalin, Kozienice, Świt
 *  Zwoleń, and Chemik+Twierdza which share bilety.mok.com.pl) into the
 *  08-06-2026 whole-corpus fixture set, so the e2e + page snapshots render their
 *  real films off Filmweb. Run:
 *    sbt 'worker/Test/runMain clients.tools.RecordTlsMsiVenues'
 *  then delete + regenerate the snapshots. today is pinned to the capture date. */
object RecordTlsMsiVenues {
  def main(args: Array[String]): Unit = {
    val rec   = new RecordingHttpFetch("08-06-2026", new RealHttpFetch())
    val today = LocalDate.of(2026, 6, 8)
    def rep(label: String)(n: => Int): Unit =
      println(f"$label%-26s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName}: ${e.getMessage}", x => s"$x films")}")
    rep("KinoKryterium")(new MsiClient(rec, "https://bilety.ck105.koszalin.pl", KinoKryterium, today).fetch().size)
    rep("KinoKozienickiDomKultury")(new MsiClient(rec, "https://bilety.dkkozienice.pl", KinoKozienickiDomKultury, today).fetch().size)
    rep("KinoSwitZwolen")(new MsiClient(rec, "https://bilety.switzwolen.pl", KinoSwitZwolen, today).fetch().size)
    rep("KinoChemik")(new MsiClient(rec, "https://bilety.mok.com.pl", KinoChemik, today, titlePrefix = Some("Chemik")).fetch().size)
    rep("KinoTwierdza")(new MsiClient(rec, "https://bilety.mok.com.pl", KinoTwierdza, today, titlePrefix = Some("TWIERDZA")).fetch().size)
  }
}
