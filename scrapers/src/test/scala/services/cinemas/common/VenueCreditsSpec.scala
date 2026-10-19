package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The line shapes [[VenueCredits]] reads, each copied from a real venue's event page (the recorded
 *  pages themselves are replayed in Bilety24EventPageCreditsSpec and KinoKreskaClientSpec). */
class VenueCreditsSpec extends AnyFlatSpec with Matchers {

  "VenueCredits" should "read credits a venue joined with semicolons (CK Lublin)" in {
    val detail = VenueCredits.parse(Seq(
      "reżyseria: Łukasz Witt-Michałowski; scenariusz: Dariusz Jeż, Grzegorz Kondrasiuk; obsada: Przemysław Buksiński, Dariusz Jeż, Jarosław Tomica"))
    detail.director shouldBe Seq("Łukasz Witt-Michałowski")
    detail.cast shouldBe Seq("Przemysław Buksiński", "Dariusz Jeż", "Jarosław Tomica")
  }

  it should "not read a 'reż.' inside a sentence as a credit" in {
    VenueCredits.parse(Seq("Klasyka kina w reż. Andrzeja Wajdy, pokazywana w cyklu Re-wizje.")) shouldBe FilmDetail()
  }

  it should "read a bare 'reż.' line at the head of the description" in {
    VenueCredits.parse(Seq("reż. Maciej Kawalski")).director shouldBe Seq("Maciej Kawalski")
  }

  it should "read hours-and-minutes and apostrophe running times" in {
    VenueCredits.parse(Seq("Czas trwania: 2 godz. 42 min.")).runtimeMinutes shouldBe Some(162)
    VenueCredits.parse(Seq("Czas trwania: 119’")).runtimeMinutes shouldBe Some(119)
  }

  it should "take a production year from the production line, never from a premiere date" in {
    val detail = VenueCredits.parse(Seq("premiera: 11 września 2026 (Polska)", "produkcja: Polska"))
    detail.countries shouldBe Seq("Polska")
    detail.releaseYear shouldBe None
  }

  it should "state nothing when two lines credit different directors" in {
    VenueCredits.parse(Seq(
      "reż. Billy Wilder | USA | 1959 | 122 min | komedia",
      "reż. Joshua Logan | USA | 1956 | 94 min | komedia/dramat/romans")) shouldBe FilmDetail()
  }

  it should "pass prose through without a fact" in {
    VenueCredits.parse(Seq("Warszawa, lata 60. Kalina Jędrusik zachwyca miliony Polaków talentem.")) shouldBe FilmDetail()
    VenueCredits.statesFacts("Warszawa, lata 60.") shouldBe false
    VenueCredits.statesFacts("reż. Natxo Leuza, Hiszpania 2025, 85'") shouldBe true
  }
}
