package clients.ekobilet

import tools.RealHttpFetch
import models.KinoJaworzyna
import clients.tools.RecordingHttpFetch
import services.cinemas.pl.EkobiletClient

import java.time.LocalDate

/** One-shot recorder for the ekobilet per-day sweep. Runs the real client for
 *  Kino Jaworzyna against live ekobilet.pl and writes every fetched page (the
 *  bare landing, each `?date=` listing, each film detail page) under
 *  `test/resources/fixtures/ekobilet-jaworzyna/` so `EkobiletClientSpec` can
 *  replay the date-strip sweep offline. Re-run with `today` pinned to the
 *  capture date when the repertoire goes stale.
 *
 *  Run: `sbt 'worker/Test/runMain clients.ekobilet.EkobiletFixtureRecorder'` */
object EkobiletFixtureRecorder {
  val CaptureDate: LocalDate = LocalDate.of(2026, 6, 11)

  def main(args: Array[String]): Unit = {
    val http = new RecordingHttpFetch("ekobilet-jaworzyna", new RealHttpFetch())
    val films = new EkobiletClient(http, "kino-jaworzyna", KinoJaworzyna, CaptureDate).fetch()
    println(s"Recorded ${films.size} film(s) for Kino Jaworzyna:")
    films.foreach(f => println(s"  ${f.movie.title} — ${f.showtimes.size} showtime(s)"))
  }
}
