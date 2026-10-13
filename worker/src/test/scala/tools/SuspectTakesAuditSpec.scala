package tools

import models.{Helios, KinoMuza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{IdentityMeasures, Listing, ResolverDecision, SuspectTakes}
import services.identity.SuspectTakes.Take

/** The suspect-take ratchet ([[SuspectTakesAudit]]) a convergence leg runs over its whole corpus: a NEW suspect fails it, a
 *  judged or baselined one does not, a take a label calls wrong fails it unless baselined, and a baseline line no take
 *  stands for any more is named for striking, never a failure. */
class SuspectTakesAuditSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, venue: models.Cinema = KinoMuza) =
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

  it should "pass a suspect judged in labels.tsv, right or wrong-and-baselined, or listed in the baseline" in {
    SuspectTakesAudit.judge("pl", corpus(fragment, fine), 2026, Seq(label(fragment.listing.rawTitle, "tmdb:1", right = true)), Set.empty)
      .failed shouldBe false
    SuspectTakesAudit.judge("pl", corpus(fragment, fine), 2026, Nil, Set(("pl", fragment.listing.rawTitle, "tmdb:1"))).failed shouldBe false
  }

  it should "fail on a take a label calls wrong, suspect or not, unless the baseline lists it" in {
    val labels  = Seq(label("Johnny", "tmdb:3", right = false))
    val verdict = SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set.empty)
    verdict.wrong.map(_.film) shouldBe Seq("tmdb:3")
    SuspectTakesAudit.judge("pl", corpus(fine, wrongOne), 2026, labels, Set(("pl", "Johnny", "tmdb:3"))).failed shouldBe false
  }

  it should "name the baseline's lines no take stands for any more, without failing" in {
    val verdict = SuspectTakesAudit.judge("pl", corpus(fine), 2026, Nil, Set(("pl", "Gone", "tmdb:9"), ("de", "Other", "tmdb:8")))
    verdict.gone shouldBe Seq(("pl", "Gone", "tmdb:9"))
    verdict.failed shouldBe false
  }

  "the baseline" should "only shrink: no more lines than it was armed with, each judged nowhere yet and listed once" in {
    val lines = java.nio.file.Files.readAllLines(SuspectTakesAudit.BaselinePath).toArray.toSeq.map(_.toString).drop(1).filter(_.nonEmpty)
    withClue(s"${SuspectTakesAudit.BaselinePath} grew: judge a new suspect into labels.tsv, never baseline it — and lower " +
      "SuspectTakesAudit.BaselineArmed when the baseline shrinks: ") { lines.size should be <= SuspectTakesAudit.BaselineArmed }
    val rows = lines.map(_.split("\t", -1))
    val keys = rows.map(a => (a(0), a(1), a(2)))
    keys.distinct.size shouldBe keys.size
    keys.map(_._1).toSet.subsetOf(models.Country.all.map(_.code).toSet) shouldBe true
    // a SUSPECT a label has since judged right at every venue ("*") is no question left: strike it. A wrong take stays
    // listed until it is fixed, whatever another venue's label says of the same billing.
    val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val judged = rows.filterNot(_(4) == SuspectTakesAudit.WrongReason).map(a => (a(0), a(1), a(2))).filter { case (cc, raw, film) =>
      labels.exists(l => l.country == cc && l.venue == "*" && l.rawTitle == raw && l.film == film && l.right) }
    withClue("baselined suspects since judged right — strike them from the baseline: ") { judged shouldBe empty }
  }
}
