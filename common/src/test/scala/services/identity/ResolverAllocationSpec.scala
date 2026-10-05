package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityMeasures.{Film, Listing, ListingFilm, ListingListing}

/**
 * The resolver's hottest helpers answer without allocating per call what they used to: on worker-pl the identity model's
 * thread allocated 727 of the worker's 1,084 MB/min (JFR 2026-10-05), most of it boxes, options, tuples and iterators
 * built per measure and thrown away — enough, in a 125 MB eden, to drag its working set into the old generation. Each
 * check also holds the answer to what the allocating form gave.
 */
class ResolverAllocationSpec extends AnyFlatSpec with Matchers {
  private val model = IdentityCalibration.resolver

  /** Bytes `body` allocates per call on this thread, after warming it up — its answer summed, not boxed. A bound of 32
   *  bytes leaves room for what the JIT has not yet scalar-replaced; each allocating form was hundreds to thousands. */
  private def bytesPerCall(body: => Double): Double = {
    var sink = 0.0
    def run(n: Int): Unit = { var i = 0; while (i < n) { sink += body; i += 1 } }
    run(20000)
    val (_, bytes) = tools.ThreadAllocation.of(run(10000))
    if (sink.isNaN) fail("unreachable")
    bytes / 10000.0
  }

  private val listing = Listing("Samson i Dalila", year = Some(2026), directors = Seq("Darko Tresnjak"), runtime = Some(180))
  private val film    = Film("Samson et Dalila", year = Some(2026), directors = Some(Seq("Darko Tresnjak")), runtime = Some(178))
  private val measures = IdentityMeasures.listingFilm(listing, film, searchRank = Some(1), rivals = 2, corroboratingVenues = 3)
  private val pairMeasures = IdentityMeasures.listingListing(listing, listing.copy(title = "Samson i Dalila (MET Opera)"), sameVenue = false,
    sharedChainId = None)

  "a calibrated score" should "be summed without a box, option or tuple per signal, to the same log-odds" in {
    Seq(ListingFilm -> measures, ListingListing -> pairMeasures).foreach { case (scope, m) =>
      model.logOdds(scope, m) shouldBe (model.scopes(scope).prior + model.contributions(scope, m).map(_._2).sum)
      model.contributionSum(scope, m) shouldBe model.contributions(scope, m).map(_._2).sum
      withClue(s"$scope: ")(bytesPerCall(model.logOdds(scope, m)) should be < 32.0)
      withClue(s"$scope: ")(bytesPerCall(model.contributionSum(scope, m)) should be < 32.0)
    }
  }
}
