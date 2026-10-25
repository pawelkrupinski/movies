package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** The agreement stage's cluster keys by overlap: a merge keeps the older key, a split keeps it on the larger half, a
 *  cluster overlapping none is fresh in order of its smallest listing, and a key no cluster took is retired — absorbed by
 *  the cluster holding most of its listings where one overlaps it. */
class ClusterKeysSpec extends AnyFlatSpec with Matchers {
  private def l(title: String): ListingKey = ListingKey.Published("Kino Muza", title, None, Nil)
  private val Seq(kA, kB, kC, kD, kE) = Seq("A", "B", "C", "D", "E").map(l)

  "a merge" should "keep the older key, the younger absorbed by the merged cluster" in {
    val assigned = ClusterKeys.assign(Seq(7L -> Seq(kC, kD), 3L -> Seq(kA, kB)), Vector(Seq(kA, kB, kC, kD)), nextFresh = 10)
    assigned shouldBe ClusterKeys.Assigned(Vector(3L), Seq(7L -> 0), 10)
  }

  "a split" should "keep the key on the larger half, the other half fresh" in {
    ClusterKeys.assign(Seq(3L -> Seq(kA, kB, kC)), Vector(Seq(kA), Seq(kB, kC)), nextFresh = 10) shouldBe
      ClusterKeys.Assigned(Vector(10L, 3L), Nil, 11)
    // an equal split: the half holding the smallest listing keeps it, whichever order the halves come in
    ClusterKeys.assign(Seq(3L -> Seq(kA, kB, kC, kD)), Vector(Seq(kC, kD), Seq(kA, kB)), nextFresh = 10).keys shouldBe Vector(10L, 3L)
    ClusterKeys.assign(Seq(3L -> Seq(kA, kB, kC, kD)), Vector(Seq(kA, kB), Seq(kC, kD)), nextFresh = 10).keys shouldBe Vector(3L, 10L)
  }

  "a cluster overlapping none" should "get a fresh key, in order of its smallest listing" in {
    ClusterKeys.assign(Nil, Vector(Seq(kE), Seq(kB, kD), Seq(kC)), nextFresh = 5) shouldBe ClusterKeys.Assigned(Vector(7L, 5L, 6L), Nil, 8)
  }

  "a key no cluster overlaps" should "be retired, absorbed by none and never handed out again" in {
    val assigned = ClusterKeys.assign(Seq(3L -> Seq(kA), 4L -> Seq(kB)), Vector(Seq(kB), Seq(kC)), nextFresh = 5)
    assigned shouldBe ClusterKeys.Assigned(Vector(4L, 5L), Nil, 6)
  }

  "a key a merge retired" should "be absorbed by the cluster holding most of its listings" in {
    // 3 = {kA, kB} and 4 = {kC, kD} regroup as {kA, kC} and {kB, kD, kE}, which take 3 and 4; 5 = {kE} overlaps only {kB, kD, kE},
    // which took an older key: it is absorbed by it
    val assigned = ClusterKeys.assign(Seq(3L -> Seq(kA, kB), 4L -> Seq(kC, kD), 5L -> Seq(kE)), Vector(Seq(kA, kC), Seq(kB, kD, kE)), nextFresh = 9)
    assigned.keys shouldBe Vector(3L, 4L)
    assigned.absorbed shouldBe Seq(5L -> 1)
  }
}
