package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.util.Random

/** Stable names by overlap — the rule behind the projection's film ids and the agreement stage's cluster keys: the laws,
 *  invariance under input order on random histories, and the input-order tie-break mutant caught. */
class StableNamesSpec extends AnyFlatSpec with Matchers {

  private def l(i: Int): ListingKey = ListingKey.Native("Kino", s"page-$i", s"title $i")
  private def set(is: Int*): Set[ListingKey] = is.map(l).toSet

  "a cluster seen before" should "keep its name, whatever order the clusters come in" in {
    StableNames.assign(Seq(1L -> set(1, 2), 2L -> set(3)), Vector(set(3), set(1, 2)), 3L) shouldBe
      StableNames.Named(Vector(2L, 1L), Nil, 3L)
  }

  "a merge" should "keep the OLDER name, even when the younger side is much larger, the younger absorbed" in {
    StableNames.assign(Seq(9L -> set(2, 3, 4, 5, 6, 7), 3L -> set(1)), Vector(set(1, 2, 3, 4, 5, 6, 7)), 10L) shouldBe
      StableNames.Named(Vector(3L), Seq(9L -> 0), 10L)
  }

  "a split" should "keep the name on the larger half, the other half fresh" in {
    StableNames.assign(Seq(7L -> set(1, 2, 3)), Vector(set(1), set(2, 3)), 8L) shouldBe StableNames.Named(Vector(8L, 7L), Nil, 9L)
  }

  it should "keep the name on the half with the smallest listing on an equal split, whatever the input order" in {
    StableNames.assign(Seq(7L -> set(1, 2, 3, 4)), Vector(set(3, 4), set(1, 2)), 8L).names shouldBe Vector(8L, 7L)
    StableNames.assign(Seq(7L -> set(1, 2, 3, 4)), Vector(set(1, 2), set(3, 4)), 8L).names shouldBe Vector(7L, 8L)
  }

  "a cluster overlapping none" should "get a fresh name, in order of its smallest listing" in {
    StableNames.assign(Nil, Vector(set(5), set(2, 4), set(3)), 5L) shouldBe StableNames.Named(Vector(7L, 5L, 6L), Nil, 8L)
  }

  "a name no cluster overlaps" should "be retired, absorbed by none and never handed out again" in {
    StableNames.assign(Seq(1L -> set(1), 2L -> set(9)), Vector(set(5), set(9), set(3)), 3L) shouldBe
      StableNames.Named(Vector(4L, 2L, 3L), Nil, 5L)
  }

  "a name a merge retired" should "be absorbed by the cluster holding most of its listings" in {
    // 3 = {1, 2} and 4 = {3, 4} regroup as {1, 3} and {2, 4, 5}, which take 3 and 4; 5 = {5} overlaps only {2, 4, 5},
    // which took an older name: it is absorbed by it
    val named = StableNames.assign(Seq(3L -> set(1, 2), 4L -> set(3, 4), 5L -> set(5)), Vector(set(1, 3), set(2, 4, 5)), 9L)
    named shouldBe StableNames.Named(Vector(3L, 4L), Seq(5L -> 1), 9L)
  }

  private def history(seed: Long): (Seq[(Long, Set[ListingKey])], Vector[Set[ListingKey]]) = {
    val rnd = new Random(seed)
    val n   = 4 + rnd.nextInt(20)
    def partition() = (1 to n).groupBy(_ => rnd.nextInt(1 + n / 3)).values.toVector.map(g => g.map(l).toSet)
    val first = partition()
    (first.zip(StableNames.assign(Nil, first, 1L).names).map(_.swap), partition())
  }

  /** The names, the absorbed and the counter, each by its cluster rather than its position. */
  private def byCluster(previous: Seq[(Long, Set[ListingKey])], next: Vector[Set[ListingKey]],
                        tieBreak: StableNames.TieBreak): (Map[Set[ListingKey], Long], Set[(Long, Set[ListingKey])], Long) = {
    val named = StableNames.assignWith(previous, next, 1000L, tieBreak)
    (next.zip(named.names).toMap, named.absorbed.map { case (name, n) => name -> next(n) }.toSet, named.nextFresh)
  }

  private def firstVariance(tieBreak: StableNames.TieBreak): Option[Long] = (1L to 3000L).find { seed =>
    val (prev, next) = history(seed)
    val reference    = byCluster(prev, next, tieBreak)
    val rnd          = new Random(seed)
    (1 to 10).exists(_ => byCluster(rnd.shuffle(prev), rnd.shuffle(next), tieBreak) != reference)
  }

  "the names" should "be identical under 10 input orders of 3,000 random histories" in {
    firstVariance(StableNames.TieBreak.Canonical) shouldBe None
  }

  it should "be caught breaking equal splits by input order (mutation)" in {
    firstVariance(StableNames.TieBreak.InputOrder) should not be empty
  }

  it should "hand no name to two clusters, every previous name kept or retired for good" in {
    (1L to 3000L).foreach { seed =>
      val (prev, next) = history(seed)
      val named        = StableNames.assign(prev, next, 1000L)
      named.names.distinct.size shouldBe next.size
      prev.map(_._1) should contain allElementsOf named.names.filter(_ < 1000L)
      (named.names.toSet & named.absorbed.map(_._1).toSet) shouldBe empty
      named.names.filter(_ < 1000L).toSet ++ named.absorbed.map(_._1) shouldBe
        prev.collect { case (name, members) if members.exists(next.flatten.toSet) => name }.toSet
    }
  }
}
