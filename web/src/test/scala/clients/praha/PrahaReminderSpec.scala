package clients.praha

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, ZoneId}

/**
 * Time-bomb reminder. Kino Praha (Mazowiecki Teatr Muzyczny, Warszawa) was
 * closed 18 May – 9 June 2026, so there was nothing to scrape when the other
 * Warszawa cinemas were added. This spec deliberately fails once the cinema has
 * reopened, so CI nags us to implement `PrahaClient` (analysis is already
 * captured) and then delete this guard.
 */
class PrahaReminderSpec extends AnyFlatSpec with Matchers {

  private val reopens = LocalDate.of(2026, 6, 9)

  "Kino Praha" should "be implemented now that it has reopened (remove this guard once PrahaClient exists)" in {
    val today = LocalDate.now(ZoneId.of("Europe/Warsaw"))
    if (today.isAfter(reopens))
      fail(s"Kino Praha reopened on $reopens (today is $today). " +
        "Implement services.cinemas.PrahaClient off https://www.mteatr.pl/pl/repertuar-kino-praha, " +
        "model the cinema, wire + uptime it, and delete PrahaReminderSpec.")
    else
      succeed
  }
}
