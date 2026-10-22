package services.billing

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/** The billing's WORK ([[BillingParse.work]]): the piece a strand, a programme or a club bills it under off, read by the
 *  one billing parse rather than by a title rule per banner; and the lookup query reading it ([[TitleNormalizer.apiQuery]]). */
class BillingWorkSpec extends AnyFlatSpec with Matchers {

  private def work(text: String) = BillingParse(text).work

  "A billing under a strand's banner" should "read as its work, the banner off" in {
    // no title rule names these banners: the strand's word does
    work("Kino Kaarst: Das gewisse Etwas") shouldBe "Das gewisse Etwas"
    work("Japanese Film Club: Linda Linda Linda") shouldBe "Linda Linda Linda"
    work("Festiwal Pięć Smaków: Morze żółte") shouldBe "Morze żółte"
    work("Senior Screening: Sense and Sensibility") shouldBe "Sense and Sensibility"
    work("Kino dla Seniora | Lalka") shouldBe "Lalka"
    work("Cykl Ale Kino - Drugie życie") shouldBe "Drugie życie"
    // a strand's piece closing the billing, opening on the strand's word
    work("Tony | Kino Dostępne") shouldBe "Tony"
    work("Solo Mio - Kino für Junggebliebene") shouldBe "Solo Mio"
    work("Lalka - seans dla grupy zorg. (SDS)") shouldBe "Lalka"
    // banners stacked one after another
    work("Kino Konesera: DKF Kropka: Lalka") shouldBe "Lalka"
  }

  it should "keep the work's own delimiters" in {
    work("Grosses Kino: Der Pate - Teil II") shouldBe "Der Pate - Teil II"
    work("Kino w Terminalu: Filmowe Zwierzenia: Nigdy w życiu!") shouldBe "Filmowe Zwierzenia: Nigdy w życiu!"
  }

  "A billing" should "be its own work when no strand's piece is cut off it" in {
    // no strand's word
    work("Mission: Impossible") shouldBe "Mission: Impossible"
    work("Kill Bill: Vol. 2") shouldBe "Kill Bill: Vol. 2"
    // the strand's word inside a bracket, or past the closing piece's first word
    work("Godzilla Minus Zero IMAX (specjalny przedpremierowy pokaz nocny) - Event Projekt") shouldBe
      "Godzilla Minus Zero IMAX (specjalny przedpremierowy pokaz nocny) - Event Projekt"
    work("Spatzenkino: Tierisches Fest") shouldBe "Spatzenkino: Tierisches Fest"
    work("Dune: Part Three Originalversion EARLY FAN PREVIEW") shouldBe "Dune: Part Three Originalversion EARLY FAN PREVIEW"
    // a delimiter inside a bracket or a quotation, or a clock's colon, cuts nothing
    work("Lalka (seans dostępny: audiodeskrypcja)") shouldBe "Lalka (seans dostępny: audiodeskrypcja)"
    work("Fjord , Preview am Europäischen Kinotag um 19:45 Uhr") shouldBe "Fjord , Preview am Europäischen Kinotag um 19:45 Uhr"
    // a relay's house or season is the relay's to read
    work("Royal Ballet & Opera im Kino: Manon") shouldBe "Royal Ballet & Opera im Kino: Manon"
    work("RBO Kino Saison 2026-27: Schwanensee") shouldBe "RBO Kino Saison 2026-27: Schwanensee"
    // what would be left is no work's title: a language's note, a date, a programme's slot
    work("Duzhe strashne kino 6 - UA") shouldBe "Duzhe strashne kino 6 - UA"
    work("Secret Screening - 19th October") shouldBe "Secret Screening - 19th October"
    work("Unrestricted View Horror Film Festival 2026: Opening Night") shouldBe "Unrestricted View Horror Film Festival 2026: Opening Night"
    work("Purbeck Film Festival - Shorts Social") shouldBe "Purbeck Film Festival - Shorts Social"
  }

  "Where a strand's banner ends" should "be where the work starts" in {
    BillingParse("Kino Kaarst: Das gewisse Etwas").workStart shouldBe Some("Kino Kaarst: ".length)
    BillingParse("Tony | Kino Dostępne").workStart shouldBe None
  }

  "The lookup query" should "ask for the billing's work, the rules folding what is left" in {
    val pl = TitleNormalizer.forCountry(Country.Poland)
    pl.apiQuery("Kino Kaarst: Das gewisse Etwas") shouldBe "Das gewisse Etwas"
    // the rules' format and event strips still fold the work
    pl.apiQuery("Kino Egzystencjalne: Ojczyzna - seans z dyskusją") shouldBe "Ojczyzna"
    pl.apiQuery("Cult Film Club: Highlander (15)") shouldBe "Highlander"
    // the display cases the banner and the work apart, as it does a banner a rule knows
    pl.recase("KINO KAARST: DAS GEWISSE ETWAS") shouldBe "Kino kaarst: Das gewisse etwas"
  }
}
