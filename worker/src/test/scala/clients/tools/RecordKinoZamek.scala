package clients.tools

import models.KinoZamekSzczecin
import tools.RealHttpFetch
import services.cinemas.pl.KinoZamekClient

import java.time.LocalDate

/** One-shot: capture the two shapes the castle's own kino pages come in.
 *    sbt 'worker/Test/runMain clients.tools.RecordKinoZamek'
 *
 *  `kino-zamek`        — today's listing, whose only entries are the summer
 *                        CYCLE page (ten films on one page) and a non-film.
 *  `kino-zamek-season` — the in-season listing (seeded from the 08-06-2026
 *                        corpus capture), whose entries are one page PER FILM.
 *                        Recorded with `RecordMissingFetch` so the seeded
 *                        listing is replayed from disk and only the event pages
 *                        it names are fetched live. */
object RecordKinoZamek {
  def main(args: Array[String]): Unit = {
    val cycle = new KinoZamekClient(
      new RecordingHttpFetch("kino-zamek", new RealHttpFetch()),
      KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8))
    println(s"kino-zamek        ${cycle.fetch().size} films")

    val season = new KinoZamekClient(
      new RecordMissingFetch("kino-zamek-season", Set("zamek.szczecin.pl"), new RealHttpFetch()),
      KinoZamekSzczecin, today = LocalDate.of(2026, 6, 8))
    println(s"kino-zamek-season ${season.fetch().size} films")
  }
}
