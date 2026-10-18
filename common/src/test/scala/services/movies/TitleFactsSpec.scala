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

  it should "read every spelling the search strips it replaces took off, and end a name at its sentence's full stop" in {
    // Kino Kinematograf's billings (its client read them by its own pattern before), and the unaccented and colon forms
    peel(Country.Poland, "Znaki Pana Śliwki (2025), reż. Urszula Morga, Bartosz Mikołajczyk").facts.directors shouldBe
      Seq("Urszula Morga", "Bartosz Mikołajczyk")
    peel(Country.Poland, "Mały Kinematograf: Baczne oczka reż. Katarzyna Agopsowicz, (2026)") shouldBe
      TitleFacts.Peeled("Mały Kinematograf: Baczne oczka (2026)", TitleFacts.Facts(directors = Seq("Katarzyna Agopsowicz")))
    peel(Country.Poland, "DKF Człowiek w Zagrożeniu: Pociągi (2024), reż. Maciej Drygas. • Spotkanie z autorem") shouldBe
      TitleFacts.Peeled("DKF Człowiek w Zagrożeniu: Pociągi (2024) • Spotkanie z autorem", TitleFacts.Facts(directors = Seq("Maciej Drygas")))
    peel(Country.Poland, "Perfect Days, rez. Wim Wenders") shouldBe TitleFacts.Peeled("Perfect Days", TitleFacts.Facts(directors = Seq("Wim Wenders")))
    peel(Country.Poland, "Za duży na bajki 3 reż: Kristoffer Rus").facts.directors shouldBe Seq("Kristoffer Rus")
    peel(Country.Poland, "Droga do Vermiglio, rezyseria: Maura Delpero").facts.directors shouldBe Seq("Maura Delpero")
    peel(Country.Poland, "Film, reż. J. Kowalski").facts.directors shouldBe Seq("J. Kowalski")
    // the word typed twice (Kino Kinematograf)
    peel(Country.Poland, "Ale kosmos! reż. reż. Elżbieta Wąsik, Jarosław Szyszko (2026)") shouldBe
      TitleFacts.Peeled("Ale kosmos! (2026)", TitleFacts.Facts(directors = Seq("Elżbieta Wąsik", "Jarosław Szyszko")))
    peel(Country.Poland, "Przekleństwa niewinności, reż. Sofia Coppola (2021)") shouldBe
      TitleFacts.Peeled("Przekleństwa niewinności (2021)", TitleFacts.Facts(directors = Seq("Sofia Coppola")))
    peel(Country.Poland, "„WAJDA: re-wizje” 2026 / KRAJOBRAZ PO BITWIE (1970) reż. Andrzej Wajda") shouldBe
      TitleFacts.Peeled("„WAJDA: re-wizje” 2026 / KRAJOBRAZ PO BITWIE (1970)", TitleFacts.Facts(directors = Seq("Andrzej Wajda")))
  }

  it should "credit nobody without a capitalised name after the word, or in a language the country does not read" in {
    peel(Country.Germany, "LEICA - EIN JAHRHUNDERT IN BILDERN - am 15.11. mit Regie-Besuch!").facts shouldBe TitleFacts.Facts.empty
    peel(Country.Poland, "Wykład dr Olgi Wesołowskiej").facts shouldBe TitleFacts.Facts.empty
    peel(Country.Poland, "Mały Kinematograf: premiera animacji i spotkanie z reżyserką Aleksandrą Chrapowicką").facts shouldBe TitleFacts.Facts.empty
    // a name after a comma is a second director only when it is a whole name
    peel(Country.Poland, "Lalka, reż. Maciej Kawalski, Polska") shouldBe
      TitleFacts.Peeled("Lalka, Polska", TitleFacts.Facts(directors = Seq("Maciej Kawalski")))
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
