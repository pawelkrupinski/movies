package services.cinemas.pl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** The clients whose pages print a date without its year, read through the shared
 *  [[services.cinemas.common.ScraperParse.upcomingDate]] rather than a year rule of their own.
 *  The hand-rolled "roll forward when long past" rules got both year-boundary cases wrong: a
 *  29 lutego listed in a non-leap December built this year's 29 February and was dropped, and a
 *  leftover late-December row seen in early January was put eleven months into the future. */
class YearlessPageDateSpec extends AnyFlatSpec with Matchers {

  "Kino IKM's day label" should "read a late-December row still listed in early January as last December's" in {
    KinoIkmClient.parseDate("30.12", LocalDate.of(2027, 1, 5)) shouldBe Some(LocalDate.of(2026, 12, 30))
  }

  it should "place a 29 lutego listed in a non-leap December on next year's leap day" in {
    KinoIkmClient.parseDate("29.02", LocalDate.of(2027, 12, 10)) shouldBe Some(LocalDate.of(2028, 2, 29))
  }
}
