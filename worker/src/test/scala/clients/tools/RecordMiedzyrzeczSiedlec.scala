package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.pl.{KinoZaRogiemSiedlecClient, MsiClient}

import java.time.LocalDate

/**
 * One-shot: capture the two new 2026-09-23 nearby-towns venues.
 *   sbt 'worker/Test/runMain clients.tools.RecordMiedzyrzeczSiedlec'
 *
 * `msi` (Międzyrzecz) — reuses the shared MSI ticketing platform, so its
 * fixture just needs the current + next month page, like every other MSI
 * venue's own directory.
 * `kzr-siedlec` — the bespoke WooCommerce scraper; records both product-list
 * pages (`?product-page=1` implicit + `?product-page=2`).
 */
object RecordMiedzyrzeczSiedlec {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def record(directory: String) = new RecordingHttpFetch(directory, real)

    val today = LocalDate.of(2026, 9, 23)

    val msi = new MsiClient(record("kino-mok-miedzyrzecz"), "https://bilety.mokmiedzyrzecz.pl",
      KinoMOKMiedzyrzecz, today = today).fetch()
    println(s"Kino MOK Międzyrzecz  ${msi.size} films")

    val siedlec = new KinoZaRogiemSiedlecClient(record("kzr-siedlec"), KinoZaRogiemSiedlec, today = today).fetch()
    println(s"Kino za Rogiem Siedlec  ${siedlec.size} films")
  }
}
