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
    n.apiQuery("Toddler Club: Pip and Posy and Friends") shouldBe "Pip and Posy and Friends"
    n.apiQuery("Róża. DKF") shouldBe "Róża"
  }

  it should "keep its own row: the merge key still carries the affix" in {
    Seq("Lalka | PREMIERA", "Vincent. Legenda Oceanu - Kino sensoryczne", "Toddler Club: Pip and Posy and Friends", "Róża. DKF")
      .foreach(t => n.sanitize(t) should not be n.sanitize(n.apiQuery(t)))
  }

  "A screening billed as a film's showing" should "be looked up as that film" in {
    // PL Kino Orzeł, Kino Stary Młyn, Kino w Ratuszu, Kino Orzeł's Fonomo, Kino w Kadrze (2026-10 corpus).
    n.apiQuery("Sytuacje Relacje. Pokaz filmu Hamnet") shouldBe "Hamnet"
    n.apiQuery("Spotkanie z aktorką Kamilą Urzędowską. Pokaz filmu \"Lalka\"") shouldBe "Lalka"
    n.apiQuery("Fonomo 26 - Tajemnica śpiewających ptaków reż. Antoine Lanciaux") shouldBe "Fonomo 26 - Tajemnica śpiewających ptaków"
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
}
