package tools

import models.{Helios, KinoMuza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{IdentityMeasures, Listing, ResolverDecision, SuspectTakes}
import services.identity.SuspectTakes.Take

/** The suspect-take ratchet ([[SuspectTakesAudit]]) a convergence leg runs over its whole corpus: a NEW suspect fails it, a
 *  judged or tolerated one does not, a take a label calls wrong fails it unless tolerated, and a tolerance no take stands
 *  for any more is named for striking, never a failure. */
class SuspectTakesAuditSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, venue: models.Cinema) =
    Listing(venue, services.movies.ListingKey.Published(venue.displayName, title, None, Nil), title, title, title, None, Nil, None, None, None)
  private def take(title: String, film: String, year: Int, tmdb: Int, venue: models.Cinema = KinoMuza) =
    Take(listing(title, venue), s"tmdb:$tmdb", Some(tmdb), IdentityMeasures.Film(film, year = Some(year)),
      ResolverDecision(Seq(listing(title, venue).key), Some(tmdb), 0.9, ResolverDecision.Basis.OwnMatch, Nil)())
  private def label(title: String, film: String, right: Boolean) = UnmatchedClusters.Label("pl", "*", title, film, right, "spec")

  private val fragment = take("Giorgia Fumo - OUT OF OFFICE", "Out of Office", 2022, 1)
  private val fine     = take("Lalka", "Lalka", 2026, 2)
  private val wrongOne = take("Johnny", "Johnny", 2026, 3)
  private def corpus(takes: Take*) = SuspectTakes.Corpus(takes, SuspectTakes.Billings.of(takes.map(_.listing), takes))

  "the ratchet" should "fail on a suspect no label judges, once per billing however many venues bill it" in {
    val verdict = SuspectTakesAudit.judge("pl", corpus(fragment, fragment.copy(listing = listing(fragment.listing.rawTitle, Helios)), fine), 2026,
      Nil, Set.empty)
    verdict.fresh.map(s => (s.rawTitle, s.film, s.venues.size)) shouldBe Seq(("Giorgia Fumo - OUT OF OFFICE", "tmdb:1", 2))
    verdict.failed shouldBe true
  }

  it should "pass a suspect judged right, or wrong and tolerated, or tolerated undecided" in {
    SuspectTakesAudit.judge("pl", corpus(fragment, fine), 2026, Seq(label(fragment.listing.rawTitle, "tmdb:1", right = true)), Set.empty)
      .failed shouldBe false
    SuspectTakesAudit.judge("pl", corpus(fragment, fine), 2026, Nil, Set(("pl", fragment.listing.rawTitle, "tmdb:1"))).failed shouldBe false
  }

  it should "judge a raw title that breaks a line by the label spelling it as the baseline does, a space for the break" in {
    // PL Kino w Ratuszu bills "„Skrzypek na dachu”\nMusical wszech czasów …": a TSV line holds no line break, so the label
    // spells it as a tolerance's key does, and must still judge the take
    val broken = take("„Out of Office”\nGiorgia Fumo na scenie", "Out of Office", 2022, 1)
    SuspectTakesAudit.judge("pl", corpus(broken, fine), 2026, Seq(label("„Out of Office” Giorgia Fumo na scenie", "tmdb:1", right = true)),
      Set.empty).failed shouldBe false
  }

  it should "judge a raw title by the label and tolerance spelling it alike but for its runs of spaces" in {
    // DE Caligari billed "ES  (ULRICH SCHAMONI)" when it was judged and baselined, "ES (ULRICH SCHAMONI)" a recording later:
    // the same billing, which a venue's spacing does not make a new question
    val respaced = take("Giorgia  Fumo -   OUT OF OFFICE", "Out of Office", 2022, 1)
    SuspectTakesAudit.judge("pl", corpus(respaced, fine), 2026, Seq(label("Giorgia Fumo - OUT OF OFFICE", "tmdb:1", right = true)), Set.empty)
      .failed shouldBe false
    SuspectTakesAudit.judge("pl", corpus(respaced, fine), 2026, Nil, Set(("pl", "Giorgia Fumo - OUT OF OFFICE", "tmdb:1"))).failed shouldBe false
    SuspectTakesAudit.judge("pl", corpus(respaced, fine), 2026, Nil, Set.empty).failed shouldBe true
    val labels = Seq(label("Johnny  ", "tmdb:3", right = false))
    SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set.empty).wrong.map(_.film) shouldBe Seq("tmdb:3")
    SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set(("pl", "Johnny ", "tmdb:3"))).failed shouldBe false
  }

  it should "fail on a take a label calls wrong, suspect or not, unless tolerated" in {
    val labels  = Seq(label("Johnny", "tmdb:3", right = false))
    val verdict = SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set.empty)
    verdict.wrong.map(_.film) shouldBe Seq("tmdb:3")
    SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set(("pl", "Johnny", "tmdb:3"))).failed shouldBe false
  }

  it should "name the tolerances no take stands for any more, without failing" in {
    val verdict = SuspectTakesAudit.judge("pl", corpus(fine), 2026, Nil, Set(("pl", "Gone", "tmdb:9"), ("de", "Other", "tmdb:8")))
    verdict.gone shouldBe Seq(("pl", "Gone", "tmdb:9"))
    verdict.failed shouldBe false
  }
}
