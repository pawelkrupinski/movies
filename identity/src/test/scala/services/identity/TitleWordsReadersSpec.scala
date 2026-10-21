package services.identity

import services.billing.DecorationWords
import models.{Country, KinoMuza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.NonMovieEventClassifier
import services.identity.agreement.NonFilmEvents
import services.movies.TitleNormalizer

/** Each reader of the one word table ([[services.movies.TitleWords]]) reads a concept in every language a country bills
 *  in — the words each rule's own list lacked before the table: a German anniversary, a German ballet house, a German
 *  surprise film, a Spanish sneak, a Spanish audience note. */
class TitleWordsReadersSpec extends AnyFlatSpec with Matchers {

  private def titled(title: String): Listing = FilmTable.listing(KinoMuza, title)

  "An edition" should "be read from an anniversary in German as in English" in {
    // DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)": the same re-release as "Dances with Wolves 35th Anniversary"
    DecorationWords.billsAnEdition(Seq("Der mit dem Wolf tanzt (35-jähriges Jubiläum)"), Seq("Der mit dem Wolf tanzt")) shouldBe true
    DecorationWords.billsAnEdition(Seq("Dances with Wolves 35th Anniversary"), Seq("Dances with Wolves")) shouldBe true
    DecorationWords.billsAnEdition(Seq("Der mit dem Wolf tanzt"), Seq("Der mit dem Wolf tanzt")) shouldBe false
  }

  "A house" should "be billed by a German ballet as by an English one" in {
    ListingShape.billsAHouse(titled("Wiener Staatsballett: Schwanensee")) shouldBe false
    ListingShape.billsAHouse(titled("Ballett im Kino: Schwanensee")) shouldBe true
    ListingShape.billsAHouse(titled("Ballet in Cinema: Swan Lake")) shouldBe true
  }

  "A secret screening" should "be an event in German as in English" in {
    NonFilmEvents.of(titled("ÜBERRASCHUNGSFILM")) shouldBe Some("mystery screening")
    NonFilmEvents.of(titled("Überraschungspremiere")) shouldBe Some("mystery screening")
    NonFilmEvents.of(titled("Secret Screening - 12th October")) shouldBe Some("mystery screening")
  }

  "A sneak" should "be a mystery screening in Spanish as in German" in {
    NonMovieEventClassifier.isMysteryScreening("Sesión sorpresa") shouldBe true
    NonMovieEventClassifier.isMysteryScreening("OV Sneak-Preview") shouldBe true
    NonMovieEventClassifier.isMysteryScreening("Seans w ciemno") shouldBe true
  }

  "An audience note" should "leave the search query in Spanish as in English" in {
    val es = TitleNormalizer.forCountry(Country.byCode("es").get)
    es.searchQuery("Wicked: Parte II - Sesión relajada") shouldBe "Wicked: Parte II"
    val uk = TitleNormalizer.forCountry(Country.byCode("uk").get)
    uk.searchQuery("Wicked: For Good - Relaxed Screening") shouldBe "Wicked: For Good"
  }
}
