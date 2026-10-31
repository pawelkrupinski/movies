package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import tools.IndependentCases

/**
 * The places the incremental model and a whole resolve could come apart by mechanism alone
 * (docs/design/identity-no-drift.md; the 2026-10-31 structure review's divergence list), each held
 * here as a property: whatever reaches the incremental model, in whatever order and grouping, it
 * holds and decides what a whole resolve of the same listings does.
 */
class IncrementalWholeDivergenceSpec extends AnyFlatSpec with Matchers {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  private val Seeds       = 1L to 12L

  /** A batch carrying two listings under one key, each order: the model holds the one a whole resolve keeps (the
   *  smallest by the total order, [[Listing.distinct]]) and decides as that resolve does. */
  private def twinKeyDivergence(seed: Long): Option[String] = {
    val corpus = GeneratedIdentityCorpus.generate(seed, normalizer, films = 8, listings = 24)
    val byFilm = corpus.listings.groupBy(l => corpus.truth(l.key)).values.toSeq.sortBy(_.head.key.toString)
    val (a, b) = (byFilm.head.head, byFilm.last.head)
    // another film's billing under `a`'s key: the same venue's row, read otherwise
    val twin   = b.copy(cinema = a.cinema, key = a.key)
    Seq(Seq(a, twin), Seq(twin, a)).zipWithIndex.flatMap { case (pair, order) =>
      val model = new IncrementalResolver(corpus.lookups, normalizer, calibration, decorations = TitleDecorations.None)
      model.batch(seen = corpus.listings.filterNot(_.key == a.key) ++ pair)
      val all      = corpus.listings.filterNot(_.key == a.key) ++ pair
      val expected = IdentityResolver.resolveWith(all, corpus.lookups, normalizer, calibration, IdentityResolver.Mutation.None)
      val heldOk   = model.held(a.key).contains(Listing.distinct(pair).head)
      Option.when(!heldOk || ResolutionSignature.of(model) != ResolutionSignature.of(expected))(
        s"seed $seed, order $order: held ${model.held(a.key).map(_.rawTitle)}, whole keeps ${Listing.distinct(pair).head.rawTitle}")
    }.headOption
  }

  "the incremental model" should "hold and decide, of two listings under one key in a batch, the one a whole resolve keeps" in {
    IndependentCases.flatMap(Seeds)(twinKeyDivergence) shouldBe empty
  }
}
