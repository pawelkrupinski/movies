package services.movies

import models.{CinemaMovie, Country, KinoApollo, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CinemaSlotBuilderSpec extends AnyFlatSpec with Matchers {

  private val slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool)

  private def runtimeOf(minutes: Int, prior: Option[Int] = None): Option[Int] =
    slots.build(CinemaMovie(Movie("Film", runtimeMinutes = Some(minutes)), KinoApollo, None, None, None, Nil, Nil, Nil),
      "Film", prior.map(m => models.SourceData(runtimeMinutes = Some(m)))).runtimeMinutes

  // Filmtheater Bleicherode bills "flüstern & SCHREIEN" (1989) at 6000 minutes (recorded DE corpus, 2026-10-04).
  "CinemaSlotBuilder.build" should "read a runtime no screened film has as unpublished, like a zero" in {
    runtimeOf(6000) shouldBe None
    runtimeOf(0) shouldBe None
    runtimeOf(6000, prior = Some(98)) shouldBe Some(98)
    runtimeOf(432) shouldBe Some(432)
  }
}
