package services.identity
package adapters

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import settings.CommitSha

/** The drift check names every listing whose live take a whole resolve of the same listings takes otherwise, counts it,
 *  and hands it to its response — but not a listing the whole resolve left waiting, nor one the model moved since. */
class IdentityDriftCheckSpec extends AnyFlatSpec with Matchers {
  private def key(venue: String, title: String) = ListingKey.Published(venue, title, None, Nil)
  private val raices = key("Museum-Lichtspiele", "Eröffnungskonzert - Raíces")
  private val muza   = key("Kino Muza", "Ojciec chrzestny")
  private val helios = key("Helios", "Rose")
  private val waits  = key("Rialto", "Nowy film")
  private val moved  = key("Kino Pod Baranami", "Kos")
  private def decision(keys: Seq[ListingKey], film: Option[Int], basis: ResolverDecision.Basis, unanswered: Int = 0) =
    ResolverDecision(keys, film, 0.5, basis, Seq(s"$basis why"), unanswered = unanswered)()
  private def resolution(ds: ResolverDecision*) =
    Resolution(ds, ds.map(_.members.size).sum, ds.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  private def listing(k: ListingKey) = Listing(models.KinoMuza, k, k.rawTitle, k.rawTitle, k.rawTitle, None, Nil, None, None, None)

  private val live = resolution(
    decision(Seq(raices), Some(122769), ResolverDecision.Basis.Agreed),
    decision(Seq(muza), Some(238), ResolverDecision.Basis.OwnMatch),
    decision(Seq(helios), Some(1176962), ResolverDecision.Basis.OwnMatch),
    decision(Seq(waits), Some(5), ResolverDecision.Basis.Agreed),
    decision(Seq(moved), Some(9), ResolverDecision.Basis.OwnMatch))
  private val whole = resolution(
    decision(Seq(raices), None, ResolverDecision.Basis.BelowThreshold),
    decision(Seq(muza), Some(238), ResolverDecision.Basis.OwnMatch),
    decision(Seq(helios), Some(1712584), ResolverDecision.Basis.OwnMatch),
    decision(Seq(waits), None, ResolverDecision.Basis.BelowThreshold, unanswered = 2),
    decision(Seq(moved), None, ResolverDecision.Basis.NoCandidate))
  private val now = IdentityDriftCheck.Live(live, Seq(raices, muza, helios, waits, moved).map(k => k -> listing(k)).toMap, _ != moved)

  "A drift check" should "name each listing the whole resolve takes otherwise, by what adopting it would do" in {
    val report = IdentityDriftCheck.compare(now, whole)
    report.drifted.map(d => d.listing -> d.kind) shouldBe Seq(helios -> "changed", raices -> "lost")
    (report.listings, report.unsettled) shouldBe ((4, 1))   // the moved listing is not compared; the waiting one is unsettled
    report.byKind shouldBe Map("changed" -> 1, "lost" -> 1)
  }

  it should "log, count and hand over what drifted — and do nothing while the heap is pressed" in {
    var counted   = Option.empty[IdentityDriftCheck.Report]
    var responded = Seq.empty[IdentityDriftCheck.Drift]
    def check(pressed: Boolean) = new IdentityDriftCheck(() => Some(now), _ => whole, new TakeChangeLines(Country.Germany, CommitSha("abc")),
      report => counted = Some(report), drifted => responded = drifted, () => pressed, tools.Stopwatch.System)
    check(pressed = true).run() shouldBe None
    counted shouldBe None
    check(pressed = false).run().map(_.drifted.size) shouldBe Some(2)
    counted.map(_.byKind) shouldBe Some(Map("changed" -> 1, "lost" -> 1))
    responded.map(_.listing) shouldBe Seq(helios, raices)
  }

  it should "check nothing before a projection has taken anything" in {
    new IdentityDriftCheck(() => None, _ => fail("no whole resolve without a projection"), new TakeChangeLines(Country.Germany, CommitSha("abc")),
      IdentityDriftCheck.Metrics.Silent, IdentityDriftCheck.Response.ReportOnly, () => false, tools.Stopwatch.System).run() shouldBe None
  }
}
