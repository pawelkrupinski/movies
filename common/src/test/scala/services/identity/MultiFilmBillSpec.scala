package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.Film

/** The words that bill several films as one programme, as prod's matched listings carry them ([[MultiFilmBill]]). */
class MultiFilmBillSpec extends AnyFlatSpec with Matchers {

  private def marker(title: String) = MultiFilmBill.marker(Seq(title))

  "A title billing several films" should "carry a marker, in every language a venue bills one in" in {
    // prod, 2026-10-06: each matched one of its films
    marker("Triple Feature: Lord of the Rings") shouldBe Some("triple feature")
    marker("The Dark Knight Trilogy") shouldBe Some("trilogy")
    marker("Maraton Horrorów") shouldBe Some("maraton")
    marker("MARATON HALLOWEEN") shouldBe Some("maraton")
    marker("Inna Mamusia - maraton") shouldBe Some("maraton")
    marker("Sylwestrowa noc przebojów filmowych 2026") shouldBe defined
    // U-jazdowski's block of three shorts, its card's subtitle beside its name
    marker("Obcy w domu | Blok filmów krótkometrażowych") shouldBe defined
    Seq("Double Bill: Alien + Aliens", "Doppelvorstellung: Matrix", "Sesión doble: Alien", "Programa doble Kubrick",
      "Podwójny seans: Shrek", "Trylogia Kieślowskiego", "Die Herr der Ringe-Trilogie", "Trilogía del dólar", "Kill Bill Vol. 1 & 2",
      "Der Pate Teil 1 & 2", "Back to Back: Before Sunrise", "Shorts Programme: Animation", "Kurzfilmprogramm: Animation",
      "Programa de cortos: Animación", "Pokaz shortów: Nowe kino").foreach(title => withClue(title)(marker(title) shouldBe defined))
  }

  it should "not be read into a film's own words" in {
    // the marker alone is a film's whole title, and "&", "Part Two", "2D" bill one film
    Seq("Marathon", "Double Feature", "Trilogy", "Fast & Furious", "Romeo & Juliet", "Dune: Part Two", "Wicked: Part 1 2D",
      "Avatar 2 (2022)", "Double Indemnity", "Body Double", "The Double Life of Veronique", "Krótki film o miłości",
      "The Night Is Short, Walk on Girl").foreach(title => withClue(title)(marker(title) shouldBe None))
  }

  "A film whose own title carries the marker" should "be no bill of others" in {
    MultiFilmBill.billsBeside(Seq("Marathon Man"), Film("Marathon Man", year = Some(1976))) shouldBe false
    MultiFilmBill.billsBeside(Seq("Trilogy of Terror"), Film("Trilogy of Terror", year = Some(1975))) shouldBe false
    MultiFilmBill.billsBeside(Seq("The Lehman Trilogy"), Film("National Theatre Live: The Lehman Trilogy")) shouldBe false
    MultiFilmBill.billsBeside(Seq("SB19 Wakas at Simula: The Trilogy Concert Finale"),
      Film("SB19 Wakas at Simula: The Trilogy Concert Finale")) shouldBe false
    // an alternative title is not the film's own: "Whistle" is filed under "Maraton horrorów"
    MultiFilmBill.billsBeside(Seq("Maraton Horrorów"), Film("Dźwięk śmierci", Some("Whistle"), Seq("Maraton horrorów"))) shouldBe true
    MultiFilmBill.billsBeside(Seq("Triple Feature: Lord of the Rings"), Film("The Lord of the Rings: The Return of the King")) shouldBe true
    MultiFilmBill.billsBeside(Seq("The Dark Knight Trilogy"), Film("The Dark Knight")) shouldBe true
  }
}
