package services.identity

import java.time.LocalDateTime
import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** CinemaxX's Vista film code read off its booking URLs ([[CatalogueSources.CinemaxX]]): at its own venues only, never
 *  off a multi-film bill, and joining only beside an agreeing title. */
class CinemaxXCodeSpec extends AnyFlatSpec with Matchers {

  private def venue(name: String): Cinema = Cinema.all.find(_.displayName == name).get
  private def film(cinema: Cinema, title: String, codes: String*): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil,
      codes.zipWithIndex.map { case (code, i) =>
        Showtime(LocalDateTime.of(2026, 10, 17, 11 + i, 20), Some(s"https://www.cinemaxx.de/buchtickets/zusammenfassung/1861/$code/4038$i"))
      })

  private val own     = venue("CinemaxX Bremen")
  private val partner = venue("Kino am Raschplatz")

  "A CinemaxX venue's listing" should "carry its booking URLs' one code as a catalogue id" in {
    CatalogueId.of(own, film(own, "Sinn und Sinnlichkeit", "HO00001648", "HO00001648")) should contain(CatalogueId("cinemaxx", "HO00001648"))
  }

  it should "carry none when its URLs name two codes, or it bills several films" in {
    CatalogueSources.CinemaxX.code(own, film(own, "Sinn und Sinnlichkeit", "HO00001648", "HO00001649")) shouldBe None
    CatalogueSources.CinemaxX.code(own, film(own, "Lord Of The Rings Triple (Extended Version - English)", "HO00001039")) shouldBe None
  }

  "A partner booking through cinemaxx.de" should "carry no code" in {
    CatalogueSources.CinemaxX.code(partner, film(partner, "Sinn und Sinnlichkeit", "HO00001648")) shouldBe None
  }

  "The code" should "be title-gated, as a feed's id" in {
    CatalogueSources.TitleGated should contain(CatalogueSources.CinemaxX.source)
  }
}
