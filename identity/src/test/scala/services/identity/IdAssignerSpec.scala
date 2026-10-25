package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** A4: the projection's film ids are [[StableNames]]' (whose spec carries the rules), over its clusters as sets: each
 *  cluster named once, empty ones dropped, the assignment in id order. */
class IdAssignerSpec extends AnyFlatSpec with Matchers {

  private def l(i: Int): ListingKey = ListingKey.Native("Kino", s"page-$i", s"title $i")
  private def set(is: Int*): Set[ListingKey] = is.map(l).toSet

  "IdAssigner" should "give the same clusters the same ids, in id order" in {
    val first = IdAssigner.assign(Seq.empty, Seq(set(1, 2), set(3)), 1L)
    first.ids shouldBe Seq(1L -> set(1, 2), 2L -> set(3))
    IdAssigner.assign(first.ids, Seq(set(3), set(1, 2)), first.nextFresh).ids shouldBe first.ids
  }

  it should "name a cluster given twice once, and drop an empty one" in {
    val assigned = IdAssigner.assign(Seq(7L -> set(1, 2, 3)), Seq(set(4), Set.empty, set(2, 3), set(4), set(1)), 8L)
    assigned.ids shouldBe Seq(7L -> set(2, 3), 8L -> set(1), 9L -> set(4))
    assigned.idOfListing shouldBe Map(l(1) -> 8L, l(2) -> 7L, l(3) -> 7L, l(4) -> 9L)
    assigned.nextFresh shouldBe 10L
  }
}

/** A projection assigns ids to every cluster of the country at once (US: ~2,250 clusters over
 *  ~108,000 listings, 62 of them over 500 listings): keyed by the clusters themselves, each lookup
 *  re-hashed a whole cluster, and the sort of the overlap pairs did that on every comparison —
 *  seconds of every US projection. */
class IdAssignerCostSpec extends AnyFlatSpec with Matchers {

  /** A cluster that counts every time it is hashed — a whole walk of its members, which is what
   *  keying a map by the cluster costs per lookup. Counted, not timed: a 3 s wall-clock ceiling
   *  read 3.2 s in a loaded test run with the code fine. */
  private final class CountedCluster(members: Set[ListingKey], hashes: java.util.concurrent.atomic.AtomicLong)
      extends scala.collection.immutable.AbstractSet[ListingKey] {
    def contains(key: ListingKey): Boolean          = members.contains(key)
    def iterator: Iterator[ListingKey]              = members.iterator
    def incl(key: ListingKey): Set[ListingKey]      = members.incl(key)
    def excl(key: ListingKey): Set[ListingKey]      = members.excl(key)
    override def knownSize: Int                     = members.size
    override def hashCode(): Int                    = { hashes.incrementAndGet(); members.hashCode() }
  }

  "IdAssigner over a country's clusters" should "hash each cluster a bounded number of times, not once per listing" in {
    val hashes = new java.util.concurrent.atomic.AtomicLong(0)
    def cluster(c: Int, size: Int): Set[ListingKey] =
      new CountedCluster((0 until size).map(i => ListingKey.Native("Kino", s"page-$c-$i", s"title $c"): ListingKey).toSet, hashes)
    // A steady day: the same films as before, 150 of them shown at 3,000 venues each — every
    // listing of a film was counted against its cluster, each count hashing the whole cluster.
    val clusters = (0 until 150).map(cluster(_, 3000)) ++ (150 until 2150).map(cluster(_, 20))
    val assigned = IdAssigner.assign(clusters.zipWithIndex.map { case (ls, i) => (i + 1).toLong -> ls }, clusters, clusters.size + 1L)
    assigned.ids.map(_._1) shouldBe (1L to 2150L)
    withClue(s"${hashes.get} cluster hashes for ${clusters.size} clusters over ${clusters.map(_.size).sum} listings: ") {
      hashes.get should be <= 2L * clusters.size
    }
  }
}
