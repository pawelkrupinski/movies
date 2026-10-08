package services.movies

import models.SourceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}

/** The cap on a year read out of a title comes from the clock its caller was given, never the
 *  machine's: a slot titled "(2027)" names a release year only once 2027 is at most next year, and
 *  a "2027" after a film's title is an instalment number until then. */
class LatestTitleYearSpec extends AnyFlatSpec with Matchers {

  private def in(year: Int): LatestTitleYear =
    LatestTitleYear(Clock.fixed(Instant.parse(s"$year-06-01T00:00:00Z"), ZoneOffset.UTC))

  "a slot's bracketed year" should "be read only when the caller's clock makes it at most next year" in {
    val slot = SourceData(title = Some("Avatar: Fire and Ash (2027)"))
    ScrapeListing.yearOf(slot)(using in(2025)) shouldBe None
    ScrapeListing.yearOf(slot)(using in(2026)) shouldBe Some(2027)
  }

  "a four-digit number after a title" should "name another instalment until the caller's clock makes it a plausible year" in {
    val base  = TitleContainment.tokens("Odyssey")
    val whole = TitleContainment.tokens("Odyssey 2027")
    TitleContainment.decorates(base, whole, in(2025).value) shouldBe false
    TitleContainment.decorates(base, whole, in(2026).value) shouldBe true
  }
}
