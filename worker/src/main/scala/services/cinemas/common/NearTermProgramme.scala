package services.cinemas.common

import models.{Cinema, CinemaMovie, City}

import java.time.Clock
import scala.concurrent.duration._

/**
 * Does a scrape that DID return screenings hold none in the next [[Window]]?
 *
 * The white bar only catches a scrape that returned nothing. Kino Polonez
 * (Skierniewice, 2026-09-26) returned something: Filmweb 320 held four "Lalka"
 * pre-sale slots 4.7 days out and nothing before them, while the venue sold 50
 * screenings that week on biletyna. Its bar was green for days. Seven of the
 * nine Filmweb venues showing that shape that day had the same desync. The
 * other two genuinely had nothing near.
 *
 * So this is a fact about the scrape, not a verdict on the scraper: a cultural
 * centre whose next film is a fortnight away trips it too (53 of 517 green PL
 * venues on 2026-09-26, most of them that kind). It marks the uptime bucket
 * `thin` — the bar stays green — as a triage cue, the way `fallback` marks a
 * bar the aggregator served.
 */
object NearTermProgramme {

  /** Three days: every desynced venue measured on 2026-09-26 had its first
   *  screening 109–137h out, and a venue that screens at all normally has
   *  something inside a long weekend. */
  val Window: FiniteDuration = 72.hours

  /** Screenings exist, but none from now (on the venue's own clock) until
   *  [[Window]] from now. An empty listing is not thin — the white bar already
   *  says it. */
  def isThin(cinema: Cinema, movies: Seq[CinemaMovie], clock: Clock): Boolean = {
    val now   = City.localNow(cinema, clock)
    val until = now.plusSeconds(Window.toSeconds)
    val times = movies.iterator.flatMap(_.showtimes).map(_.dateTime).toSeq
    times.nonEmpty && !times.exists(t => !t.isBefore(now) && t.isBefore(until))
  }
}
