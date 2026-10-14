package services.identity

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `scripts/title-trace` on the billings investigated 2026-10-14: each step it names is the production step's own answer.
 */
class TitleTraceSpec extends AnyFlatSpec with Matchers {

  private def trace(raw: String, country: Country, venue: Option[String] = None): Seq[String] =
    TitleTrace.trace(raw, country, TitleTrace.venueOf(venue).fold(sys.error, identity))
  private def line(lines: Seq[String], prefix: String): String =
    lines.map(_.trim).find(_.startsWith(prefix)).getOrElse(fail(s"no line '$prefix' in\n${lines.mkString("\n")}"))

  "a sneak naming no film it bills" should "search its words, which SearchQueryMarkersSpec flags, and be no mystery screening" in {
    val t = trace("Sneak Over", Country.Germany, Some("Filmwerk Gutersloh"))
    t.head should include("venue 'Filmwerk Gutersloh'")
    line(t, "NonMovieEventClassifier:") should include("isBareSneak) no")
    line(t, "SearchQueryMarkersSpec:") shouldBe "SearchQueryMarkersSpec: searches 'Sneak Over' (sneak → the country's title rules (TitleRules / ExtraTitleRules))"
  }

  "a sensory-friendly strand banner" should "be stripped from the search by its named rule, never from the slot" in {
    val t = trace("Pokaz przyjazny osobom z ADHD: Kandydaci śmierci", Country.Poland)
    line(t, "= slot title") shouldBe "= slot title 'Pokaz przyjazny osobom z ADHD: Kandydaci śmierci'"
    line(t, "xtra-pp-przyjazny-audience") should endWith("→ 'Kandydaci śmierci'")
    line(t, "= search query") shouldBe "= search query 'Kandydaci śmierci'"
  }

  "a double bill" should "be read as several films, and so exempt from the marker rule" in {
    val t = trace("Thunderbirds - Double Bill 4k Restoration: Atlantic Inferno & Attack of the Alligators", Country.UnitedKingdom)
    line(t, "MultiFilmBill:") shouldBe "MultiFilmBill: marker 'double bill', billed one none"
    line(t, "ListingShape:") should startWith("ListingShape: billsSeveral YES")
    line(t, "SearchQueryMarkersSpec:") shouldBe "SearchQueryMarkersSpec: exempt (bills several films)"
  }

  "a BBFC certificate" should "be peeled off the slot and the searches" in {
    val t = trace("Obsession (18)", Country.UnitedKingdom)
    line(t, "= slot title") shouldBe "= slot title 'Obsession'"
    line(t, "identity title searches") shouldBe "identity title searches (IdentityMeasures.searchQueries): 'Obsession'"
  }

  "a library-filed title with a re-release mark" should "have its article fronted once the mark is peeled" in {
    val t = trace("Rocky Horror Picture Show, The (WA)", Country.Germany)
    line(t, "format peel") shouldBe
      "format peel (FormatTags.extractFormatTags): 'Rocky Horror Picture Show, The (WA)' → 'The Rocky Horror Picture Show' no tags, filed article fronted"
    line(t, "= search query") shouldBe "= search query 'The Rocky Horror Picture Show'"
  }

  "a leading format banner" should "be peeled into the slot's tags" in {
    val t = trace("ATMOS: Michael", Country.Germany)
    line(t, "format peel") shouldBe "format peel (FormatTags.extractFormatTags): 'ATMOS: Michael' → 'Michael' tags [ATMOS]"
    line(t, "identity title searches") shouldBe "identity title searches (IdentityMeasures.searchQueries): 'ATMOS: Michael', 'Michael'"
  }

  "a meeting with a pre-premiere screening" should "search the quoted film, and name the shape verdicts it draws" in {
    val t = trace("Spotkanie z Ewą Błaszczyk i przedpremierowy pokaz filmu \"Violetta Villas\".FMZ", Country.Poland)
    line(t, "xtra-pokaz-filmu") should endWith("→ 'Violetta Villas'")
    line(t, "= search query") shouldBe "= search query 'Violetta Villas'"
    line(t, "NonMovieEventClassifier:") should endWith("screened broadcast YES")
  }

  "a venue the roster does not hold" should "be traced with no venue rules" in {
    trace("Lalka", Country.Poland, Some("No Such Kino 123")).head should include("not in the roster (no venue rules)")
  }

  "two refs' traces" should "carry nothing but the steps, so they diff to what the code moved" in {
    trace("Lalka", Country.Poland) shouldBe trace("Lalka", Country.Poland)
  }
}
