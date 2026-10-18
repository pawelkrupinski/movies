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
    // a credit word's ż reads as z, typed or not
    peel(Country.Poland, "Perfect Days, rez. Wim Wenders").facts.directors shouldBe Seq("Wim Wenders")
    peel(Country.Poland, "Film, reż. J. Kowalski").facts.directors shouldBe Seq("J. Kowalski")
    // Kino Fenomen's (biletyna's) pipe metadata, which its client read by its own pattern before
    peel(Country.Poland, "„Milcząca przyjaciółka” | reżyseria: Ildikó Enyedi | Francja, Niemcy, Węgry 2025 (2D/napisy)") shouldBe
      TitleFacts.Peeled("„Milcząca przyjaciółka” | Francja, Niemcy, Węgry 2025 (2D/napisy)", TitleFacts.Facts(directors = Seq("Ildikó Enyedi")))
    peel(Country.Poland, "Film | reżyseria: A. Kowalski, B. Nowak | Polska 2024").facts.directors shouldBe Seq("A. Kowalski", "B. Nowak")
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
    // the credit takes what it runs on to before the billing goes on (the search strip it replaced did, for search)
    peel(Country.Poland, "Lalka, reż. Maciej Kawalski, Polska") shouldBe
      TitleFacts.Peeled("Lalka", TitleFacts.Facts(directors = Seq("Maciej Kawalski")))
    // German "Regie" is no Polish credit word
    peel(Country.Poland, "Kino Regie Jan Kowalski").facts shouldBe TitleFacts.Facts.empty
  }

  "a credit beside other notes" should "go alone, its sentence ending where the billing goes on" in {
    // inside a bracket, only its comma-separated piece goes: the badge or year beside it stays
    peel(Country.Germany, "Film (OmU, Regie: Jan Kowalski)") shouldBe TitleFacts.Peeled("Film (OmU)", TitleFacts.Facts(directors = Seq("Jan Kowalski")))
    peel(Country.Poland, "Lalka (2026, reż. Maciej Kawalski)") shouldBe TitleFacts.Peeled("Lalka (2026)", TitleFacts.Facts(directors = Seq("Maciej Kawalski")))
    // a full stop ends the name; the next sentence is no name of it
    peel(Country.Poland, "Pociągi, reż. Maciej Drygas. Spotkanie z reżyserem").facts.directors shouldBe Seq("Maciej Drygas")
    // a comma joins one more whole name only
    peel(Country.Poland, "Perfect Days, reż. Wim Wenders, Dyskusyjny Klub Filmowy") shouldBe
      TitleFacts.Peeled("Perfect Days", TitleFacts.Facts(directors = Seq("Wim Wenders")))
    // an unbalanced bracket or a credit opening the title leaves nothing dangling
    peel(Country.Germany, "Film (Regie: Jan Kowalski") shouldBe TitleFacts.Peeled("Film", TitleFacts.Facts(directors = Seq("Jan Kowalski")))
    peel(Country.Poland, "Reż. Jan Kowalski: Film") shouldBe TitleFacts.Peeled("Film", TitleFacts.Facts(directors = Seq("Jan Kowalski")))
  }

  it should "credit no capitalised noun: a lone word only after a marked credit word" in {
    peel(Country.Germany, "Heimat (Regie Gespräch im Anschluss)").facts shouldBe TitleFacts.Facts.empty
    peel(Country.Germany, "Kurzfilmabend: Film von Studierenden der Filmhochschule").facts shouldBe TitleFacts.Facts.empty
    peel(Country.Germany, "Phoenix (Regie: Petzold)").facts.directors shouldBe Seq("Petzold")
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
    // PL: Kino w Kadrze's closing age bracket (recording 38713274617); an audience's "60+" is none
    peel(Country.Poland, "Kandydaci śmierci: Kadr Non-Fiction (16+)") shouldBe
      TitleFacts.Peeled("Kandydaci śmierci: Kadr Non-Fiction", TitleFacts.Facts(ageRating = Some("16+")))
    peel(Country.Poland, "MISTYCZKA - \"SENIOR W KINIE\" Cykl filmowy dla widzów 60+").facts shouldBe TitleFacts.Facts.empty
    peel(Country.UnitedKingdom, "Kandydaci śmierci (16+)").facts shouldBe TitleFacts.Facts.empty
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
    // an unconfirmed time behind a certificate is no fact: the title keeps it for the search strip
    FormatTags.peel("NTL: The Motive and the Cue (Encore) (15) 180 mins TBC").runtimeMinutes shouldBe None
    // Watershed Bristol's "certificate to be announced", inside the bracket or after it: the expected certificate read
    FormatTags.peel("The Store (18 CTBA)") shouldBe FormatTags.Peeled("The Store", Nil, Some("18"), None)
    FormatTags.peel("Golden Boy (18) CTBA") shouldBe FormatTags.Peeled("Golden Boy", Nil, Some("18"), None)
    // awaited: no certificate
    FormatTags.peel("EOS - Monet (Cert. TBC)").certificate shouldBe None
  }
}
