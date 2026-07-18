package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas._
import services.cinemas.pl._

import java.time.LocalDate
import scala.util.Try

/** Records the cinemas switched off Filmweb in the "replace-all" batch into the
 *  08-06-2026 whole-corpus fixture set (so e2e + page snapshots render their
 *  films), plus a few per-client fixtures for the new specs. Verifies each
 *  switch produces films through the REAL client (catches TLS/skin failures).
 *  Multikino is recorded by the refresh-fixtures workflow (needs the Zyte seam).
 *  Run: sbt 'worker/Test/runMain clients.tools.RecordReplaceBatch' */
object RecordReplaceBatch {
  def main(args: Array[String]): Unit = {
    val real  = new RealHttpFetch()
    def record(directory: String) = new RecordingHttpFetch(directory, real)
    val today = LocalDate.of(2026, 6, 8)
    def rep(label: String)(n: => Int): Unit =
      println(f"$label%-30s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName}: ${e.getMessage}", x => s"$x films")}")

    def corpus = record("08-06-2026")

    // ── Chains: Helios (native REST) + Cinema City ──
    Seq(HeliosNuxt.Starachowice, HeliosNuxt.Krosno, HeliosNuxt.Tczew, HeliosNuxt.Zory,
        HeliosNuxt.Lubin, HeliosNuxt.OstrowWielkopolski, HeliosNuxt.KedzierzynKozle).foreach { config =>
      rep(config.cinema.displayName)(new HeliosClient(corpus, config, today).fetch().size)
    }
    rep("Cinema City Ruda Śląska")(
      new CinemaCityScraper(new CinemaCityClient(corpus), "1062", CinemaCity).fetch().size)

    // ── Ekobilet (5) ──
    val eko: Seq[(String, Cinema)] = Seq(
      "opolskielamy" -> KinoMeduza, "kinorejs" -> KinoRejs, "mokis-bielawa" -> KinoMOKiS,
      "kino-jaworzyna" -> KinoJaworzyna, "kino-centrum-jastrzebiezdrj" -> KinoCentrum)
    eko.foreach { case (slug, c) => rep(c.displayName)(new EkobiletClient(corpus, slug, c, today).fetch().size) }

    // ── systembiletowy alt-skin (3) ──
    val sb: Seq[(String, Cinema)] = Seq(
      "https://bilety.pckul.pl" -> KinoPckulKino, "https://bilety.mok.zory.pl" -> KinoNaStarowce,
      "https://ock.systembiletowy.pl" -> KinoNaszeKino)
    sb.foreach { case (base, c) => rep(c.displayName)(new SystemBiletowyClient(corpus, base, c).fetch().size) }


    // ── Existing-client one-offs ──
    rep("Kinoteatr Rondo")(new BiletynaClient(corpus, "https://biletyna.pl/Chelmno/Kinoteatr-Rondo", KinoRondo).fetch().size)
    rep("Forum (Bolesławiec)")(new Bilety24OrganizerClient(corpus,
      "https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-kino-forum-1586",
      KinoForumBoleslawiec).fetch().size)

    // ── Per-client fixtures for the new specs ──
    println("--- spec fixtures ---")
    rep("ekobilet spec (Meduza)")(new EkobiletClient(record("kino-meduza"), "opolskielamy", KinoMeduza, today).fetch().size)
    rep("systembiletowy-alt spec")(new SystemBiletowyClient(record("kino-pckul"), "https://bilety.pckul.pl", KinoPckulKino).fetch().size)
  }
}
