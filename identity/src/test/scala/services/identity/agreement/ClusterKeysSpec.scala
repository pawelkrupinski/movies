package services.identity.agreement

import models.{KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{FilmTable, Listing, Resolution, ResolverDecision, StoredFamily}
import models.ListingKey

import scala.collection.mutable

/** The agreement stage's cluster keys ([[ClusterKeys]]): a decision handed over again keeps its key, a new one takes the
 *  key of the cluster gone it overlaps (a merge naming the key it absorbed) or a fresh one, a restart re-attaches the
 *  stored keys by first listing and resumes the counter past them, and a published apply's keys stay readable beside
 *  the next's. */
class ClusterKeysSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val Seq(muza, multikino, palacowe) = Seq(KinoMuza, Multikino, KinoPalacowe).map(listing(_, "Klondike", year = Some(2022))).sortBy(_.key)(using ListingKey.ordering)

  private def cluster(listings: Listing*) = ResolverDecision(listings.map(_.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()
  private def resolution(decisions: ResolverDecision*) = {
    val keys = decisions.flatMap(_.members)
    Resolution(decisions, keys.size, keys.zipWithIndex.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  }

  /** One apply's keying: what [[ClusterKeys.assign]] gave each decision, and the merges it named, published. */
  private def applied(keys: ClusterKeys, first: Boolean, decisions: ResolverDecision*): (Seq[String], Seq[(String, String)]) = {
    val assigned = new java.util.IdentityHashMap[ResolverDecision, String]()
    val merged   = mutable.ArrayBuffer.empty[(String, String)]
    keys.assign(resolution(decisions*), first, assigned, merged)
    keys.publish(assigned)
    (decisions.map(assigned.get), merged.toSeq)
  }

  "A decision handed over again" should "keep its key, a new one take a fresh key, and one never handed be keyed by its first listing" in {
    val keys = new ClusterKeys(new InMemoryAgreementKeys)
    val (one, two) = (cluster(muza), cluster(multikino, palacowe))
    applied(keys, first = true, one, two)._1 shouldBe Seq("0", "1")
    val three = cluster(muza)
    applied(keys, first = false, one, two)._1 shouldBe Seq("0", "1")
    keys.keyOf(three) shouldBe StoredFamily.idOf(three.members)
  }

  "A merge" should "keep the older key of the clusters it joins, and name the key it absorbed" in {
    val keys = new ClusterKeys(new InMemoryAgreementKeys)
    val (one, two) = (cluster(muza), cluster(multikino, palacowe))
    applied(keys, first = true, one, two)
    val (merged, absorbed) = applied(keys, first = false, cluster(muza, multikino, palacowe))
    merged shouldBe Seq("0")
    absorbed shouldBe Seq("0" -> "1")
  }

  "A restart" should "re-attach each stored key by its cluster's first listing, and hand out no key it stored again" in {
    val store = new InMemoryAgreementKeys
    val before = new ClusterKeys(store)
    applied(before, first = true, cluster(muza), cluster(multikino), cluster(palacowe))
    store.all().next shouldBe 3L
    val after = new ClusterKeys(store)
    // Pałacowe's cluster first now, Multikino's gone: Pałacowe keeps 2, Muza 0, and a new cluster takes 3, never the retired 1
    val (named, _) = applied(after, first = true, cluster(palacowe), cluster(muza), cluster(listing(KinoMuza, "Dune", year = Some(2021))))
    named shouldBe Seq("2", "0", "3")
    store.all().keys.map(_.key).toSet shouldBe Set(0L, 2L, 3L)
  }

  "The last apply's keys" should "stay readable once the next apply published its own" in {
    val keys = new ClusterKeys(new InMemoryAgreementKeys)
    val one  = cluster(muza)
    applied(keys, first = true, one)
    val two  = cluster(muza, multikino)   // the model reshaped the cluster: a new decision object, the key carried by overlap
    applied(keys, first = false, two)
    (keys.keyOf(one), keys.keyOf(two)) shouldBe (("0", "0"))
    applied(keys, first = false, two)
    keys.keyOf(one) shouldBe StoredFamily.idOf(one.members)
  }
}
