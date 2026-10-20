package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** Premiere words, sensory and toddler programmes and a full-stop DKF tag are stripped for SEARCH only
 *  (`apiQuery`): the film is looked up bare, and the screening keeps its own title and row. The titles
 *  are venues' own, from the PL and UK corpora of recording 36807940234. */
class SearchOnlyAffixesSpec extends AnyFlatSpec with Matchers {

  private val n = SingleCountryNormalizer.titleNormalizer

  "A listing title" should "be looked up without its premiere, sensory, toddler or DKF affix" in {
    n.apiQuery("Lalka | PREMIERA") shouldBe "Lalka"
    n.apiQuery("BARANEK SHAUN I KUDŁATA BESTIA (DUBBING) (PREMIERA)") shouldBe "BARANEK SHAUN I KUDŁATA BESTIA"
    n.apiQuery("Czas, który nie nadszedł - uroczysta premiera") shouldBe "Czas, który nie nadszedł"
    n.apiQuery("Vincent. Legenda Oceanu - Kino sensoryczne") shouldBe "Vincent. Legenda Oceanu"
    n.apiQuery("Kino przyjazne sensorycznie: Baranek Shaun i kudłata bestia") shouldBe "Baranek Shaun i kudłata bestia"
    // any audience a Polish screening is billed as friendly to (KINOkawiarnia Stacja Falenica, recording 38498856157)
    n.apiQuery("Pokaz przyjazny osobom z ADHD: Kandydaci śmierci") shouldBe "Kandydaci śmierci"
    n.apiQuery("Seans Przyjazny Sensorycznie: Willow i tajemniczy las") shouldBe "Willow i tajemniczy las"
    n.apiQuery("Pokaz przyjazny dla osób w spektrum autyzmu: Lalka") shouldBe "Lalka"
    n.apiQuery("Toddler Club: Pip and Posy and Friends") shouldBe "Pip and Posy and Friends"
    n.apiQuery("Róża. DKF") shouldBe "Róża"
    // US Showcase's Met broadcasts (recording 37071880312): the encore is the screening's, its year the film's
    n.apiQuery("The Metropolitan Opera: Così fan tutte ENCORE (2026)") shouldBe "The Metropolitan Opera: Così fan tutte (2026)"
    n.apiQuery("Encore") shouldBe "Encore"
  }

  it should "keep its own row: the merge key still carries the affix" in {
    Seq("Lalka | PREMIERA", "Vincent. Legenda Oceanu - Kino sensoryczne", "Toddler Club: Pip and Posy and Friends", "Róża. DKF")
      .foreach(t => n.sanitize(t) should not be n.sanitize(n.apiQuery(t)))
  }

  "A screening billed as a film's showing" should "be looked up as that film" in {
    // PL Kino Orzeł, Kino Stary Młyn, Kino w Ratuszu, Kino Orzeł's Fonomo, Kino w Kadrze (2026-10 corpus).
    n.apiQuery("Sytuacje Relacje. Pokaz filmu Hamnet") shouldBe "Hamnet"
    n.apiQuery("Spotkanie z aktorką Kamilą Urzędowską. Pokaz filmu \"Lalka\"") shouldBe "Lalka"
    // a venue code glued after the closing quote (Kino w Ratuszu, recording 38498856157)
    n.apiQuery("Spotkanie z Sandrą Drzymalską i przedpremierowy pokaz filmu \"Violetta Villas\".FMZ") shouldBe "Violetta Villas"
    n.apiQuery("DKF ZAPRASZA 19 CZERWCA NA PREMIEROWY POKAZ FILMU „OJCZYZNA” (REŻYSERA „IDY” ORAZ „ZIMNEJ WOJNY”)") shouldBe "OJCZYZNA"
    n.apiQuery("Kandydaci śmierci: Kadr Non-Fiction (16+)") shouldBe "Kandydaci śmierci"
    // the screening keeps its own row
    n.sanitize("Sytuacje Relacje. Pokaz filmu Hamnet") should not be n.sanitize("Hamnet")
  }

  "A distributor's \". Film\" suffix" should "come off the title it shows and the key it merges by" in {
    // https://kinowo.net/poznan/movie/rolling-loud-film — the Polish distributor bills "Rolling Loud. Film"
    // (and Jaworzyna "Rolling Loud. Film 2026"); the film is "Rolling Loud".
    n.preferredDisplay(Seq("Rolling Loud. Film")) shouldBe Some("Rolling Loud")
    n.sanitize("Rolling Loud. Film 2026") shouldBe n.sanitize("Rolling Loud")
    // A title merely ending in the word keeps it.
    n.preferredDisplay(Seq("Straszny film")) shouldBe Some("Straszny film")
  }

  "A film titled by the word" should "keep it" in {
    // A title that is only the word is the film's own, however it reads.
    n.apiQuery("Premiera") shouldBe "Premiera"
    // A premiere marker after a bare space may be the title's own word.
    n.apiQuery("Ostatnia Premiera") shouldBe "Ostatnia Premiera"
    // Nor is a pre-premiere's word a premiere's: "przedpremiera" keeps its own rule.
    n.apiQuery("Straszny film – przedpremiera") shouldBe "Straszny film"
  }

  // PL strands billed with no separator: docs/investigations/2026-10-20-pl-unmatched-real-films.md (prod mirror 10-20)
  "A Polish strand billed with no separator" should "be looked up as its film" in {
    // Kino Studyjne Centrum's debut strand
    n.apiQuery("Debiut Klarnet") shouldBe "Klarnet"
    n.apiQuery("Debiut Violetta Villas") shouldBe "Violetta Villas"
    // Na Starówce's trailing strands
    n.apiQuery("Lalka święto kina") shouldBe "Lalka"
    n.apiQuery("Jak zyć, żeby nie zwariować czas kobiet") shouldBe "Jak zyć, żeby nie zwariować"
    // Terminal Kultury Gocław's education strand, and the cycle and town after it
    n.apiQuery("Lydia i Władca Burz Edukacja Młode Horyzonty") shouldBe "Lydia i Władca Burz"
    n.apiQuery("Drogi Panie Dyktatorze Edukacja Młoda Horyzonty") shouldBe "Drogi Panie Dyktatorze"
    n.apiQuery("SONIA Edukacja Młode Horyzonty, cykl \"Filmowe sekrety\", Warszawa") shouldBe "SONIA"
    // CK Nowy Pafawag's narrated screenings: the quotes name the film
    n.apiQuery("Festiwal Korelacje: „Zemsta” z autorską narracją Artura Andrusa") shouldBe "Zemsta"
    n.apiQuery("ZAMKNIĘCIE FESTIWALU KORELACJE: „Zaklęte rewiry” z autorską  narracją Malcolma XD + spotkanie z autorem") shouldBe "Zaklęte rewiry"
    // a film club's banner with no colon, the film quoted
    n.apiQuery("Klub Filmowy Perspektywy film „Oczy szeroko zamknięte”") shouldBe "Oczy szeroko zamknięte"
    n.apiQuery("Klub Filmowy WCK \"Odlot\"") shouldBe "Odlot"
    // a re-release marker spelled with a space after the hyphen (Kino Światowid)
    n.apiQuery("Inni (re- release)") shouldBe "Inni"
  }

  it should "leave a title that only opens or ends on the strand's words" in {
    n.apiQuery("Debiut") shouldBe "Debiut"
    n.apiQuery("Edukacja Młode Horyzonty") shouldBe "Edukacja Młode Horyzonty"
    n.apiQuery("Czas kobiet") shouldBe "Czas kobiet"
    // the screening keeps its own row
    n.sanitize("Debiut Klarnet") should not be n.sanitize("Klarnet")
  }
}
