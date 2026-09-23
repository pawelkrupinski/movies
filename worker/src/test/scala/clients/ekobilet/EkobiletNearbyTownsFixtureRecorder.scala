package clients.ekobilet

import tools.RealHttpFetch
import models.{Cinema, KinoCKiTIlza, KinoDKGora, KinoMilenium, KinoOpolanka, KinoTon, KinoZaciszeWasosz}
import clients.tools.RecordingHttpFetch
import services.cinemas.pl.EkobiletClient

import java.time.LocalDate

/** One-shot recorder for the five ekobilet venues found in the 2026-09-23
 *  nearby-towns sweep (see `scraper_brief.md`), each captured into its OWN
 *  fixture directory (`ekobilet-<slug>`) so `EkobiletNearbyTownsClientSpec` can
 *  replay them offline. Two of the five (Wąsosz, Milejów, Opole Lubelskie) are
 *  on the "chrono-row" landing skin `EkobiletClient` gained to serve them — the
 *  fixture proves that skin parses real showtimes, not just the synthetic HTML
 *  in a unit test. Re-run with `CaptureDate` pinned to the capture date when the
 *  repertoire goes stale.
 *
 *  Run: `sbt 'worker/Test/runMain clients.ekobilet.EkobiletNearbyTownsFixtureRecorder'` */
object EkobiletNearbyTownsFixtureRecorder {
  val CaptureDate: LocalDate = LocalDate.of(2026, 9, 23)

  private val venues: Seq[(String, String, Cinema)] = Seq(
    ("ekobilet-gora",           "dom-kultury-w-gorze-7114", KinoDKGora),
    ("ekobilet-wasosz",         "zpkwasosz",                KinoZaciszeWasosz),
    ("ekobilet-milejow",        "kino-milenium",            KinoMilenium),
    ("ekobilet-opole-lubelskie", "ock-opolelubelskie",      KinoOpolanka),
    ("ekobilet-zuromin",        "kinoton",                  KinoTon),
    ("ekobilet-ilza",           "centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza),
  )

  /** Pass fixture directories to record only those — re-recording a venue
   *  changes the fixture its spec pins — or none to record all. */
  def main(args: Array[String]): Unit =
    venues.filter { case (dir, _, _) => args.isEmpty || args.contains(dir) }.foreach { case (fixtureDir, slug, cinema) =>
      val http  = new RecordingHttpFetch(fixtureDir, new RealHttpFetch())
      val films = new EkobiletClient(http, slug, cinema, CaptureDate).fetch()
      println(s"Recorded ${films.size} film(s) for ${cinema.displayName} ($fixtureDir):")
      films.foreach(f => println(s"  ${f.movie.title} — ${f.showtimes.size} showtime(s)"))
    }
}
