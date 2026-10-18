package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The facts venues bill inside a title (recording 38713274617's billings), read per country. */
class TitleFactsSpec extends AnyFlatSpec with Matchers {

  private def peel(country: Country, title: String) = country.titleFacts.peel(title)

  "a director credited in a title" should "be read into the listing's director and taken off the title" in {
    peel(Country.Poland, "Lalka, reż. Maciej Kawalski (2026)") shouldBe
      TitleFacts.Peeled("Lalka (2026)", TitleFacts.Facts(directors = Seq("Maciej Kawalski")))
    peel(Country.Poland, "Fonomo 26 - Nova '78 reż. Rodrigo Areias, Aaron Brookner").facts.directors shouldBe Seq("Rodrigo Areias", "Aaron Brookner")
    peel(Country.Poland, "PLAC ZABAW PUTINA reż. Konrad Szołajski | Październik z DKF \"Bariera\"") shouldBe
      TitleFacts.Peeled("PLAC ZABAW PUTINA | Październik z DKF \"Bariera\"", TitleFacts.Facts(directors = Seq("Konrad Szołajski")))
    // a bracket holding the credit goes whole
    peel(Country.Poland, "Obcy (reż. Ozon)") shouldBe TitleFacts.Peeled("Obcy", TitleFacts.Facts(directors = Seq("Ozon")))
    peel(Country.Germany, "Alpha (dir. Ducournau)") shouldBe TitleFacts.Peeled("Alpha", TitleFacts.Facts(directors = Seq("Ducournau")))
    peel(Country.Germany, "Alte Liebe (Der neue Film von Andreas Dresen mit Dagmar Manzel und Joachim Król)") shouldBe
      TitleFacts.Peeled("Alte Liebe", TitleFacts.Facts(directors = Seq("Andreas Dresen")))
  }

  it should "credit nobody without a capitalised name after the word, or in a language the country does not read" in {
    peel(Country.Germany, "LEICA - EIN JAHRHUNDERT IN BILDERN - am 15.11. mit Regie-Besuch!").facts shouldBe TitleFacts.Facts.empty
    peel(Country.Poland, "Wykład dr Olgi Wesołowskiej").facts shouldBe TitleFacts.Facts.empty
    // a name after a comma is a second director only when it is a whole name
    peel(Country.Poland, "Lalka, reż. Maciej Kawalski, Polska").facts.directors shouldBe Seq("Maciej Kawalski")
    // German "Regie" is no Polish credit word
    peel(Country.Poland, "Kino Regie Jan Kowalski").facts shouldBe TitleFacts.Facts.empty
  }

  "an original title labelled in a title" should "be read into the listing's original title" in {
    peel(Country.Germany, "Cinema Francais - Couscous und Geheimnisse (Originaltitel: La petite cuisine de Mehdi)") shouldBe
      TitleFacts.Peeled("Cinema Francais - Couscous und Geheimnisse", TitleFacts.Facts(originalTitle = Some("La petite cuisine de Mehdi")))
  }

  "an age rating billed in a title" should "be read in the country's own system" in {
    peel(Country.Germany, "Jüdische Filmwoche: Das siebte Kreuz D, ab 12") shouldBe
      TitleFacts.Peeled("Jüdische Filmwoche: Das siebte Kreuz", TitleFacts.Facts(ageRating = Some("FSK 12")))
    // the release pending: the note goes, no rating
    peel(Country.Germany, "DUNE: PART THREE D, FSK folgt") shouldBe TitleFacts.Peeled("DUNE: PART THREE", TitleFacts.Facts.empty)
    peel(Country.Germany, "HIN UND WEG D, ab 12 / ab 6 mit Begleitung").title shouldBe "HIN UND WEG"
    // kinoheld's note means nothing in the UK
    peel(Country.UnitedKingdom, "Das siebte Kreuz D, ab 12").title shouldBe "Das siebte Kreuz D, ab 12"
  }

  it should "read the certificate the format peel takes off only where it is the country's rating system" in {
    val format = FormatTags.peel("Wicker (15) 105 mins")
    format shouldBe FormatTags.Peeled("Wicker", Nil, Some("15"), Some(105))
    TitleFacts.of(Country.UnitedKingdom.titleFacts, TitleFacts.Facts.empty, format) shouldBe
      TitleFacts.Facts(runtimeMinutes = Some(105), ageRating = Some("15"))
    TitleFacts.of(Country.Germany.titleFacts, TitleFacts.Facts.empty, format) shouldBe TitleFacts.Facts(runtimeMinutes = Some(105))
    FormatTags.peel("Screening: NTL - Hay Fever (12A TBC) (160 mins TBC)") shouldBe
      FormatTags.Peeled("Screening: NTL - Hay Fever", Nil, Some("12A"), Some(160))
    FormatTags.peel("Bad Apples (15) (2026) 100 mins") shouldBe FormatTags.Peeled("Bad Apples (2026)", Nil, Some("15"), Some(100))
    // awaited: no certificate
    FormatTags.peel("EOS - Monet (Cert. TBC)").certificate shouldBe None
  }
}
