package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentitySlicing
import services.movies.ListingKey

/** identity-slice --batch: selectors whose seeds cut the same cluster are ONE slice ([[IdentitySlice.sameCluster]]) — the
 *  UK session stats re-sliced CBeebies' 219-listing cluster once for each of its 210 venues. */
class IdentitySliceBatchSpec extends AnyFlatSpec with Matchers {

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
}
