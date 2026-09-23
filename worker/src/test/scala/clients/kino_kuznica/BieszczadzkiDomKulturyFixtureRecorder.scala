package clients.kino_kuznica

import tools.RealHttpFetch
import models.KinoBieszczadzkiDK
import clients.tools.RecordingHttpFetch
import services.cinemas.pl.SystemBiletowyClient
import services.movies.SingleCountryNormalizer.titleNormalizer

/** One-shot recorder for the Bieszczadzki Dom Kultury (Lesko) systembiletowy
 *  instance, found in the 2026-09-23 nearby-towns sweep (see
 *  `scraper_brief.md`). Captures `bdk.systembiletowy.pl/index.php` into
 *  `test/resources/fixtures/bdk-systembiletowy/` so `SystemBiletowyClientSpec`
 *  can replay it offline. Re-run when the repertoire goes stale.
 *
 *  Run: `sbt 'worker/Test/runMain clients.kino_kuznica.BieszczadzkiDomKulturyFixtureRecorder'` */
object BieszczadzkiDomKulturyFixtureRecorder {
  def main(args: Array[String]): Unit = {
    val http  = new RecordingHttpFetch("bdk-systembiletowy", new RealHttpFetch())
    val movies = new SystemBiletowyClient(http, "https://bdk.systembiletowy.pl", KinoBieszczadzkiDK, titles = titleNormalizer).fetch()
    println(s"Recorded ${movies.size} film(s) for Bieszczadzki Dom Kultury:")
    movies.foreach(m => println(s"  ${m.movie.title} — ${m.showtimes.size} showtime(s)"))
  }
}
