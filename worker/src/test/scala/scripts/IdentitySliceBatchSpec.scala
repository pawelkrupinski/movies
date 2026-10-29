package scripts

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentitySlicing
import services.movies.ListingKey

/** identity-slice --batch: selectors whose seeds cut the same cluster are ONE slice ([[IdentitySlice.sameCluster]]) — the
 *  UK session stats re-sliced CBeebies' 219-listing cluster once for each of its 210 venues. */
class IdentitySliceBatchSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private def key(venue: String, title: String) = ListingKey.Published(venue, title, None, Nil)
  private val pantoA = key("A", "Panto")
  private val pantoB = key("B", "Panto")
  private val heat   = key("C", "Heat")
  private val index = IdentitySlicing.Index(Nil, familyOf = Map(pantoA -> 1, pantoB -> 1, heat -> 2),
    nodeOf = Map(pantoA -> "n1", pantoB -> "n2", heat -> "n3"),
    keysOf = Map("n1" -> Set("t:panto"), "n2" -> Set("t:panto"), "n3" -> Set("t:heat")),
    nodeListings = Map("n1" -> Seq(pantoA), "n2" -> Seq(pantoB), "n3" -> Seq(heat)))
  private def selector(k: ListingKey) = IdentitySlice.Selector(Some(k.venue), Some(k.rawTitle), exact = true)

  "A batch" should "slice the selectors of one cluster once, every seed of them in it, in order" in {
    val none = IdentitySlice.Selector(Some("Z"), Some("Nothing"), exact = true)
    IdentitySlice.sameCluster(Seq(selector(pantoA) -> Set(pantoA), selector(heat) -> Set(heat), selector(pantoB) -> Set(pantoB),
      none -> Set.empty), index, 0) shouldBe Seq(selector(pantoA) -> Set(pantoA, pantoB), selector(heat) -> Set(heat), none -> Set.empty)
  }

  "Many slices" should "be decided at once, and handed on in the batch's order whichever finishes first" in {
    val running = new java.util.concurrent.atomic.AtomicInteger()
    val most    = new java.util.concurrent.atomic.AtomicInteger()
    val handed  = scala.collection.mutable.ListBuffer.empty[Int]
    // The first three slices hold until all three run, so "at once" is proven, not timed.
    val allThree = new java.util.concurrent.CountDownLatch(3)
    IdentitySlice.inOrder(Seq(5, 4, 3, 2, 1), threads = 3) { n =>
      most.accumulateAndGet(running.incrementAndGet(), math.max)
      allThree.countDown()
      allThree.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
      running.decrementAndGet()
      n
    }(handed += _)
    handed.toList shouldBe List(5, 4, 3, 2, 1)
    most.get should be > 1
  }

  "A slice's result" should "be decided once and read back from its directory after" in {
    val dir = tempDirectory("results").resolve("slice")
    var decided = 0
    def decide = { decided += 1; IdentitySlice.SliceResult("## report\n", Seq("a\tb"), Nil) }
    val first = IdentitySlice.kept(Some(dir))(decide)
    IdentitySlice.kept(Some(dir))(decide) shouldBe first
    decided shouldBe 1
    IdentitySlice.kept(None)(decide)
    decided shouldBe 2
  }

  it should "be kept under the labels about its listings, not every label" in {
    val o = IdentitySlice.Options()
    def label(title: String, film: String) = tools.UnmatchedClusters.Label("pl", "*", title, film, right = true, "")
    def keyOf(labels: Seq[tools.UnmatchedClusters.Label]) = IdentitySlice.resultKey(selector(pantoA), Set(pantoA, pantoB), o, labels, models.Country.Poland)
    val bare = keyOf(Nil)
    keyOf(Seq(label("Heat", "tmdb:1"))) shouldBe bare
    keyOf(Seq(label("Panto", "tmdb:2"))) should not be bare
    IdentitySlice.resultKey(selector(pantoA), Set(pantoA, pantoB), o.copy(widen = 1), Nil, models.Country.Poland) should not be bare
  }
}
