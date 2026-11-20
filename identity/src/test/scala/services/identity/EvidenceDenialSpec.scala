package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.movies.SingleCountryNormalizer

/** The refusals the listing's own evidence gives ([[CandidateScoring.evidenceDenial]]), each pinned by its text and kind:
 *  the trace names a denial by its text (`refused:…`, the ablation's `veto:<id>`, [[RuleContract]]'s declarations), so a
 *  row's wording is part of the model's output. One case per row, each a pair from the spec of the question it asks. */
class EvidenceDenialSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val scoring = new ResolveStages(Nil, new FilmTable(Nil, normalizer), normalizer, IdentityCalibration.resolver,
    IdentityResolver.Mutation.None, PinConstraints(Nil), TitleDecorations.None).scoring

  private def denial(listing: MeasuredListing, film: Film, country: String = "GB"): Option[(String, Denial.Kind)] =
    scoring.evidenceDenial(listing, film, Map.empty, Some(country)).map(denied => denied.why -> denied.kind)

  "The listing's own evidence" should "deny a screen namesake of the work its stage house's billing names" in {
    denial(MeasuredListing("Paris Opera Ballett: Romeo und Julia"), Film("Romeo und Julia", year = Some(1968))) shouldBe
      Some("a screen namesake of the work its stage house's billing names" -> Denial.Kind.NotTheWork(creditOverrules = true))
  }

  it should "deny a record of another work than its stage house's billing names" in {
    val macbeth = MeasuredListing("MET Opera: Macbeth 2026", originalTitle = Some("The Metropolitan Opera: Eugene Onegin"))
    denial(macbeth, Film("MET Opera: Eugen Onegin", Some("The Metropolitan Opera: Eugene Onegin"))) shouldBe
      Some("a record of another work than its stage house's billing names" -> Denial.Kind.Evidence)
  }

  it should "deny another production of its work than the season its screenings fall in" in {
    val paris = MeasuredListing("Opéra De Paris Ballet - Notre-Dame de Paris", screenedSeason = Some(2026))
    val scala = Film("La Scala Ballet: Notre-Dame de Paris", Some("Roland Petit: Notre-Dame de Paris (Teatro alla Scala)"), year = Some(2013))
    denial(paris, scala) shouldBe Some("another production of its work than the season its screenings fall in" -> Denial.Kind.Evidence)
  }

  it should "deny a catalogue namesake of a bare billing under a film festival's banner" in {
    val billed = MeasuredListing("The more the merrier", Some("Pornfilmfestival: The more the merrier"), screeningsSeason = Some(2026))
    denial(billed, Film("The More the Merrier", year = Some(1943)), "DE") shouldBe
      Some("a catalogue namesake of a bare billing under a film festival's banner" -> Denial.Kind.Evidence)
  }

  it should "deny one work of a \"//\" double bill" in {
    val bill = "SHINJUKUY BOYS // THE FISH WITH ONE SLEEVE"
    denial(MeasuredListing(bill, Some(bill)), Film("片袖の魚", Some("片袖の魚"), Seq("The Fish with One Sleeve")), "DE") shouldBe
      Some("one work of a \"//\" double bill, whose other piece none of its titles holds" -> Denial.Kind.Evidence)
  }

  it should "deny a part its title subtitles otherwise than the film" in {
    val lang = Film("Das indische Grabmal", year = Some(1959), directors = Some(Seq("Fritz Lang")))
    denial(MeasuredListing("DAS INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)"), lang, "DE") shouldBe
      Some("a part its title gives a subtitle none of the film's titles holds" -> Denial.Kind.Evidence)
  }

  it should "deny an instalment its title numbers and its original title subtitles otherwise" in {
    val lotr2  = MeasuredListing("Der Herr der Ringe 2 (Extended Version)", originalTitle = Some("The Lord of the Rings: The Two Towers – Extended"))
    val bakshi = Film("Der Herr der Ringe", originalTitle = Some("The Lord of the Rings"), year = Some(1978))
    denial(lotr2, bakshi, "DE") shouldBe
      Some("an instalment its title numbers, whose subtitle none of the film's titles holds" -> Denial.Kind.Evidence)
  }

  it should "deny another year's edition of the series its title dates" in {
    denial(MeasuredListing("Disney Jr Cinema Club 2026"), Film("Disney Jr. Cinema Club 2025", year = Some(2025))) shouldBe
      Some("another year's edition of the series its title dates" -> Denial.Kind.Evidence)
  }
}
