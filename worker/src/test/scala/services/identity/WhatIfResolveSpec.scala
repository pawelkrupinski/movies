package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A what-if re-resolves only the families its change can reach — and must still decide each as a whole resolve would:
 *  a re-resolved family that now blocks under another family's key is merged with it, as the whole resolve merges them. */
class WhatIfResolveSpec extends AnyFlatSpec with Matchers {
  import WhatIfResolve.{Family, Resolved, affected}

  private val base = Seq(
    Family("a1", Set("a1", "a2"), Set("title:a")),
    Family("b1", Set("b1"), Set("title:b")),
    Family("c1", Set("c1", "c2"), Set("title:c")),
    Family("d1", Set("d1"), Set("title:d")))

  /** A head whose families are one per group, blocking under the base keys of its members plus `extra`'s. */
  private def head(extra: Map[String, Set[String]], asked: scala.collection.mutable.Buffer[Set[String]]): Seq[Set[String]] => Seq[Seq[Resolved]] =
    groups => groups.map { group =>
      asked += group
      Seq(Resolved(group, base.filter(_.members.exists(group)).flatMap(_.blockKeys).toSet ++ group.flatMap(extra.getOrElse(_, Set.empty))))
    }

  "A what-if's region" should "re-resolve the selected family alone when its families meet no other's key" in {
    val asked  = scala.collection.mutable.Buffer.empty[Set[String]]
    val region = affected(Set("a1"), base, head(Map.empty, asked))
    region.listings shouldBe Set("a1", "a2")
    region.rounds shouldBe 1
    asked.toSeq shouldBe Seq(Set("a1", "a2"))
  }

  it should "join a family outside the selection whose block key a re-resolved family now carries" in {
    val asked  = scala.collection.mutable.Buffer.empty[Set[String]]
    val region = affected(Set("a2"), base, head(Map("a2" -> Set("title:c")), asked))
    region.listings shouldBe Set("a1", "a2", "c1", "c2")
    region.groups shouldBe Seq(Set("a1", "a2", "c1", "c2"))
    region.rounds shouldBe 2
    asked.toSeq shouldBe Seq(Set("a1", "a2"), Set("a1", "a2", "c1", "c2"))
  }

  it should "merge two selected groups whose families now share a block key, and resolve again only what changed" in {
    val asked  = scala.collection.mutable.Buffer.empty[Set[String]]
    val region = affected(Set("a1", "b1", "d1"), base, head(Map("b1" -> Set("title:a")), asked))
    region.groups.toSet shouldBe Set(Set("a1", "a2", "b1"), Set("d1"))
    asked.count(_ == Set("d1")) shouldBe 1
    region.resolved.flatten.flatMap(_.members).toSet shouldBe Set("a1", "a2", "b1", "d1")
  }

  it should "resolve a selected listing no base family holds as a group of its own" in {
    val asked  = scala.collection.mutable.Buffer.empty[Set[String]]
    affected(Set("new"), base, head(Map.empty, asked)).listings shouldBe Set("new")
  }

  "A selection file" should "keep every key whole: a page-less listing with no year or director ends in its separator" in {
    val bare = services.movies.ListingKey.Published("Institute of Contemporary Arts", "Pereda: Fauna", None, Nil)
    val keys = Seq(WhatIfResolve.keyOf(bare), WhatIfResolve.keyOf(services.movies.ListingKey.Native("ICA", "https://ica/1", "Fauna")))
    WhatIfResolve.selection(keys ++ Seq("")) shouldBe keys.toSet
  }
}
