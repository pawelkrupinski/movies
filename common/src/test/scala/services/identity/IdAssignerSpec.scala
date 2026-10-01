package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.util.Random

/** A4: stable ids by overlap — the laws, invariance under input order on random histories, and
 *  the input-order tie-break mutant caught. */
class IdAssignerSpec extends AnyFlatSpec with Matchers {

  private def l(i: Int): ListingKey = ListingKey.Native("Kino", s"page-$i", s"title $i")
  private def set(is: Int*): Set[ListingKey] = is.map(l).toSet

  "IdAssigner" should "give the same clusters the same ids" in {
    val first = IdAssigner.fresh(Seq(set(1, 2), set(3)))
    IdAssigner.assign(first.ids, Seq(set(3), set(1, 2)), first.nextFresh).ids shouldBe first.ids
  }

  it should "keep the id on the larger half of a split" in {
    val prev = Seq(7L -> set(1, 2, 3))
    IdAssigner.assign(prev, Seq(set(1), set(2, 3)), 8L).idOf(set(2, 3)) shouldBe 7L
  }

  it should "keep the id on the half with the smallest listing on an equal split, whatever the input order" in {
    val prev = Seq(7L -> set(1, 2, 3, 4))
    IdAssigner.assign(prev, Seq(set(3, 4), set(1, 2)), 8L).idOf(set(1, 2)) shouldBe 7L
    IdAssigner.assign(prev, Seq(set(1, 2), set(3, 4)), 8L).idOf(set(1, 2)) shouldBe 7L
  }

  it should "keep the OLDER id on a merge, even when the newer side is much larger" in {
    val prev = Seq(3L -> set(1), 9L -> set(2, 3, 4, 5, 6, 7))
    IdAssigner.assign(prev, Seq(set(1, 2, 3, 4, 5, 6, 7)), 10L).ids shouldBe Seq(3L -> set(1, 2, 3, 4, 5, 6, 7))
  }

  it should "mint fresh ids in smallest-listing order and retire an id nothing overlaps" in {
    val prev = Seq(1L -> set(1), 2L -> set(9))
    val next = IdAssigner.assign(prev, Seq(set(5), set(1), set(3)), 3L)
    next.ids shouldBe Seq(1L -> set(1), 3L -> set(3), 4L -> set(5))
    next.nextFresh shouldBe 5L
  }

  private def history(seed: Long): (Seq[(Long, Set[ListingKey])], Seq[Set[ListingKey]]) = {
    val rnd = new Random(seed)
    val n   = 4 + rnd.nextInt(20)
    def partition() = (1 to n).groupBy(_ => rnd.nextInt(1 + n / 3)).values.toSeq.map(g => g.map(l).toSet)
    (IdAssigner.fresh(partition()).ids, partition())
  }

  private def firstVariance(tieBreak: IdAssigner.TieBreak): Option[Long] = (1L to 3000L).find { seed =>
    val (prev, next) = history(seed)
    val reference = IdAssigner.assignWith(prev, next, 1000L, tieBreak)
    val rnd = new Random(seed)
    (1 to 10).exists(_ => IdAssigner.assignWith(rnd.shuffle(prev), rnd.shuffle(next), 1000L, tieBreak) != reference)
  }

  it should "assign identically under 10 input orders of 3,000 random histories" in {
    firstVariance(IdAssigner.TieBreak.Canonical) shouldBe None
  }

  it should "be caught breaking equal splits by input order (mutation)" in {
    firstVariance(IdAssigner.TieBreak.InputOrder) should not be empty
  }

  it should "hand no id to two clusters and drop no cluster" in {
    (1L to 3000L).foreach { seed =>
      val (prev, next) = history(seed)
      val a = IdAssigner.assign(prev, next, 1000L)
      a.ids.map(_._1).distinct.size shouldBe a.ids.size
      a.ids.map(_._2).toSet shouldBe next.filter(_.nonEmpty).toSet
    }
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
