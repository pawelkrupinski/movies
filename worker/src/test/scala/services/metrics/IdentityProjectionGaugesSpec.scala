package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ResolverDecision, TakeChange, TakeReversals}
import services.movies.ListingKey

/** The take changes by basis and change, and the take reversals per country and per billing: what the take alerts read. */
class IdentityProjectionGaugesSpec extends AnyFlatSpec with Matchers {
  import PrometheusExposition.{render, sample}

  private def change(venue: String, title: String, before: Option[Int], after: Option[Int]) =
    TakeChange(ListingKey.Published(venue, title, None, Nil), Some(s"film-$title"), before, after)

  /** The reversals each projection's changes make, 6 s apart, each listing held by a decision of the round's basis, as
   *  the projection hands them over ([[TakeReversals]]). */
  private def reversing(rounds: (Seq[TakeChange], ResolverDecision.Basis)*): Seq[Seq[TakeReversals.Reversal]] = {
    val clock     = new tools.MutableClock(java.time.Instant.parse("2026-11-05T10:00:00Z"))
    val reversals = new TakeReversals(clock)
    rounds.map { case (changes, basis) =>
      clock.advanceSeconds(6)
      reversals.note(changes, k => Some(ResolverDecision(Seq(k), None, 0.9, basis, Nil)()))
    }
  }
  private val OwnMatch = ResolverDecision.Basis.OwnMatch

  "the take reversals" should "count a billing once a projection however many venues it reversed at, beside the listings" in {
    // UK "RBO Cinema Season 2026-27: Macbeth" at every boot: lost at the boot's projection, taken back at the next
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityProjectionGauges(Nil, registry).forCountry("uk")
    val venues   = Seq("Cineworld Bolton", "Cineworld Ely", "Regent Redcar")
    def macbeth(before: Option[Int], after: Option[Int]) = venues.map(change(_, "RBO Cinema Season 2026-27: Macbeth", before, after))
    reversing(macbeth(Some(1703622), None) -> OwnMatch, macbeth(None, Some(1703622)) -> OwnMatch, macbeth(Some(1703622), None) -> OwnMatch)
      .foreach(metrics.takesReversed)
    val text = render(registry)
    sample(text, "kinowo_worker_identity_take_reversals_total", """country="uk"""") shouldBe Some(6.0)
    sample(text, "kinowo_worker_identity_billing_reversals_total", """billing="rbo cinema season 2026 27 macbeth",country="uk"""") shouldBe Some(2.0)
  }

  "the take flips" should "count each reversal by pattern, class, the stages that dropped and restored it and how long it stood" in {
    // the backtest's commonest: a model-stage loss a fill restores in the next projection, 6 s later
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityProjectionGauges(Seq("de"), registry).forCountry("de")
    def take(title: String, before: Option[Int], after: Option[Int], trigger: String = "scrape") =
      change("Kino Babylon", title, before, after).copy(trigger = Some(trigger))
    reversing(Seq(take("Kaidan", Some(30959), None), take("Boot", Some(5), None, trigger = "deploy")) -> OwnMatch,
      Seq(take("Kaidan", None, Some(30959)), take("Boot", None, Some(5))) -> ResolverDecision.Basis.Filled).foreach(metrics.takesReversed)
    val text = render(registry)
    sample(text, "kinowo_worker_identity_take_flips_total",
      """class="RULE",country="de",dropped_by="model",lasted="1m",pattern="film-none-film",restored_by="fill"""") shouldBe Some(1.0)
    // the deploy's reversal counted apart, as scripts/flip-timeline classes it, and kept out of the time to reverse
    sample(text, "kinowo_worker_identity_take_flips_total",
      """class="DEPLOY/RESTART",country="de",dropped_by="model",lasted="1m",pattern="film-none-film",restored_by="fill"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_take_flip_seconds_count", """country="de",pattern="film-none-film"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_take_flip_seconds_bucket", """country="de",pattern="film-none-film",le="10.0"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_take_flip_seconds_bucket", """country="de",pattern="film-none-film",le="5.0"""") shouldBe Some(0.0)
    // every pattern's time to reverse exported at 0 from boot; the deploy's reversal an oscillation, no flip
    sample(text, "kinowo_worker_identity_take_flip_seconds_count", """country="de",pattern="film-film-film"""") shouldBe Some(0.0)
    sample(text, "kinowo_worker_identity_take_reversals_total", """country="de"""") shouldBe Some(2.0)
  }

  "the take changes" should "count each listing of a projection's change list by the basis holding it after and its kind, a new one decided" in {
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityProjectionGauges(Nil, registry).forCountry("pl")
    val agreed   = Some(ResolverDecision.Basis.Agreed)
    metrics.takesChanged(Seq(change("Kino Muza", "Diuna", Some(1), None).copy(basisAfter = Some(ResolverDecision.Basis.Vetoed)),
      change("Rialto", "Diuna", Some(1), Some(2)).copy(basisAfter = agreed),
      TakeChange(ListingKey.Published("Helios", "Diuna", None, Nil), None, None, Some(2), basisAfter = agreed),
      TakeChange(ListingKey.Published("Amok", "Diuna", None, Nil), None, None, Some(2), basisAfter = agreed)))
    val text = render(registry)
    sample(text, "kinowo_worker_identity_take_changes_total", """basis="Vetoed",country="pl",kind="lost"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_take_changes_total", """basis="Agreed",country="pl",kind="changed"""") shouldBe Some(1.0)
    sample(text, "kinowo_worker_identity_take_changes_total", """basis="Agreed",country="pl",kind="decided"""") shouldBe Some(2.0)
    // every basis and kind at 0 from boot
    sample(text, "kinowo_worker_identity_take_changes_total", """basis="OwnMatch",country="pl",kind="gained"""") shouldBe Some(0.0)
  }

  it should "count each move by its cause, a new listing's first take by none" in {
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityProjectionGauges(Nil, registry).forCountry("pl")
    metrics.takesChanged(Seq(change("Kino Muza", "Diuna", Some(1), None).copy(cause = Some(TakeChange.Cause.Evidence)),
      change("Rialto", "Diuna", Some(1), None).copy(cause = Some(TakeChange.Cause.Evidence)),
      change("Helios", "Lalka", Some(3), None).copy(cause = Some(TakeChange.Cause.Code)),
      TakeChange(ListingKey.Published("Amok", "Diuna", None, Nil), None, None, Some(2))))
    val text = render(registry)
    sample(text, "kinowo_worker_identity_take_causes_total", """cause="evidence",country="pl",kind="lost"""") shouldBe Some(2.0)
    sample(text, "kinowo_worker_identity_take_causes_total", """cause="code",country="pl",kind="lost"""") shouldBe Some(1.0)
    // every move and cause at 0 from boot, and nothing for a listing's first take
    sample(text, "kinowo_worker_identity_take_causes_total", """cause="regrouped",country="pl",kind="gained"""") shouldBe Some(0.0)
    sample(text, "kinowo_worker_identity_take_causes_total", """cause="code",country="pl",kind="decided"""") shouldBe None
  }

  "the rule firings" should "add each rule's listings and the listings counted, a series per rule id" in {
    val registry = new PrometheusRegistry()
    val report   = new IdentityProjectionGauges(Nil, registry).ruleFirings("pl")
    report(Map("title:search-programme-prefix" -> 3, "accept:sole-work" -> 40), 120)
    report(Map("title:search-programme-prefix" -> 2), 118)
    val text = render(registry)
    sample(text, "kinowo_worker_identity_rule_fires_total", """country="pl",rule="title:search-programme-prefix"""") shouldBe Some(5.0)
    sample(text, "kinowo_worker_identity_rule_fires_total", """country="pl",rule="accept:sole-work"""") shouldBe Some(40.0)
    sample(text, "kinowo_worker_identity_rule_counted_listings_total", """country="pl"""") shouldBe Some(238.0)
  }
}
