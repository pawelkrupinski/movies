package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.TakeChange
import services.movies.ListingKey

/** The take reversals, per country and per billing: what the oscillation alerts read. */
class IdentityProjectionGaugesSpec extends AnyFlatSpec with Matchers {
  import PrometheusExposition.{render, sample}

  private def change(venue: String, title: String, before: Option[Int], after: Option[Int]) =
    TakeChange(ListingKey.Published(venue, title, None, Nil), s"film-$title", before, after)

  "the take reversals" should "count a billing once a projection however many venues it reversed at, beside the listings" in {
    // UK "RBO Cinema Season 2026-27: Macbeth" at every boot: lost at the boot's projection, taken back at the next
    val registry = new PrometheusRegistry()
    val metrics  = new IdentityProjectionGauges(registry).forCountry("uk")
    val venues   = Seq("Cineworld Bolton", "Cineworld Ely", "Regent Redcar")
    def macbeth(before: Option[Int], after: Option[Int]) = venues.map(change(_, "RBO Cinema Season 2026-27: Macbeth", before, after))
    metrics.takesChanged(macbeth(Some(1703622), None))
    metrics.takesChanged(macbeth(None, Some(1703622)))
    metrics.takesChanged(macbeth(Some(1703622), None))
    val text = render(registry)
    sample(text, "kinowo_worker_identity_take_reversals_total", """country="uk"""") shouldBe Some(6.0)
    sample(text, "kinowo_worker_identity_billing_reversals_total", """billing="rbo cinema season 2026 27 macbeth",country="uk"""") shouldBe Some(2.0)
  }
}
