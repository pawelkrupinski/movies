package services.identity
package adapters

import models.{Helios, KinoMuza, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

/** How often each rule fires on the live listings: the title rules a listing's title took, the model's rules for it and
 *  a later stage's rule — each listing once per rule — over every listing a written whole-corpus projection decided. */
class RuleFiringCountSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private def listing(cinema: models.Cinema, raw: String): Listing = {
    val (clean, _) = normalizer.listingTitle(cinema, raw)
    Listing(cinema, ListingKey.Published(cinema.displayName, raw, None, Nil), raw, clean, clean, None, Nil, None, None, None)
  }
  private def resolution(decisions: ResolverDecision*) = Resolution(decisions, 0, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  private val programme = listing(Helios, "Pokaz przyjazny osobom z ADHD: Kandydaci śmierci")
  private val lalka     = listing(KinoMuza, "Lalka")
  private val obcy      = listing(Rialto, "Obcy")
  private val undecided = listing(Rialto, "Diuna")

  private val accepted = ResolverDecision(Seq(programme.key, lalka.key), Some(1), 0.9, ResolverDecision.Basis.OwnMatch, Nil)(
    DecisionTrace(None, None, Map(programme.key -> DecisionTrace.Node(Some("sole-work"), Seq("same-title"), Nil),
      lalka.key -> DecisionTrace.Node(Some("sole-work"), Seq("same-title"), Nil))))
  private val filled = ResolverDecision(Seq(obcy.key), Some(2), 0.3, ResolverDecision.Basis.Filled, Nil)(
    DecisionTrace(None, None, Map(obcy.key -> DecisionTrace.Node(None, Nil, Seq("Learned(year.delta >= 3)"),
      refusals = Seq(DecisionTrace.Refusal("sole-work", "below the cut")))), stage = Some("fill:venues.current")))

  "the count" should "count each rule once per listing: its title rules, the model's rules and a later stage's" in {
    val counts = RuleFiringCount.count(resolution(accepted, filled), Seq(programme, lalka, obcy, undecided),
      l => if (l eq programme) Seq("title:a", "title:a", "format:3d") else Nil)
    counts shouldBe Map("title:a" -> 1, "format:3d" -> 1, "accept:sole-work" -> 2, "join:same-title" -> 2,
      "apart:learned-year-delta-3" -> 1, "refused:sole-work:below-the-cut" -> 1, "stage:fill:venues.current" -> 1)
  }

  it should "read each listing's title rules as the title pipeline fires them, and report them with the listings counted" in {
    var reported = Seq.empty[(Map[String, Int], Int)]
    val count = new RuleFiringCount(normalizer, (fired, listings) => reported :+= ((fired, listings)), _.run(), () => false)
    count(resolution(accepted, filled), Seq(programme, lalka, obcy, undecided))
    count.submit(resolution(accepted, filled), Seq(programme, lalka, obcy, undecided))
    reported should have size 2
    val (fired, listings) = reported.head
    listings shouldBe 4
    val titleRules = normalizer.firedRules(Helios, programme.rawTitle)
    withClue(titleRules)(titleRules.exists(_.startsWith("title:")) shouldBe true)
    titleRules.foreach(rule => fired(rule) shouldBe 1)
    fired("accept:sole-work") shouldBe 2
    reported(1) shouldBe reported.head // kept title rules read the same
  }

  it should "count nothing while the heap is near its limit" in {
    var reported = 0
    new RuleFiringCount(normalizer, (_, _) => reported += 1, _.run(), () => true).submit(resolution(accepted), Seq(programme))
    reported shouldBe 0
  }
}
