package services.identity

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.FamilyClosure.Edge
import services.movies.TitleNormalizer

/**
 * The family (block closure): a resolve may run family by family only while no constraint edge
 * joins two families, and the closure check must SEE an edge that does.
 */
class FamilyClosureSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.Germany)

  private def keys(title: String, original: Option[String] = None) =
    FamilyClosure.blockKeys(title, original, normalizer)

  // DE convergence, 2026-09-25 — the Faust bug's shape. Murnau's film under two venues'
  // spellings, one resolved; Kulturfabrik Meda's "Zärtlich kreist die Faust" (1990), unresolved.
  private val listings: Map[String, Set[String]] = Map(
    "murnau"    -> keys("Faust - Eine deutsche Volkssage"),
    "schauburg" -> keys("Faust", original = Some("Faust - Eine deutsche Volkssage")),
    "meda"      -> keys("Zärtlich kreist die Faust"))

  "the block closure" should "put listings sharing a title form or an original title in one family" in {
    val family = FamilyClosure.families(listings)
    family("murnau") shouldBe family("schauburg")
    family("meda") should not be family("murnau")
  }

  it should "number families by their smallest listing, whatever order the keys come in" in {
    val reversed = scala.collection.immutable.ListMap(listings.toSeq.reverse*)
    FamilyClosure.families(reversed) shouldBe FamilyClosure.families(listings)
    FamilyClosure.families(listings) shouldBe Map("meda" -> 0, "murnau" -> 1, "schauburg" -> 1)
  }

  it should "not join two listings through an empty title form" in {
    val blank = Map("a" -> keys("—"), "b" -> keys("***"))
    blank.values.foreach(_.filter(_.startsWith("t:")) shouldBe empty)
    FamilyClosure.families(blank).values.toSet should have size 2
  }

  "the closure check" should "pass edges drawn between listings that share a block key" in {
    val edges = Seq(Edge("murnau", "schauburg", must = true, "original-title"), Edge("schauburg", "murnau", must = false, "different-tmdb"))
    FamilyClosure.crossings(FamilyClosure.families(listings), edges) shouldBe empty
  }

  it should "catch a containment edge, whose endpoints share no block key" in {
    // "Zärtlich kreist die Faust" ENDS with "Faust": the containment rule that adopted it drew
    // an edge the closure cannot see. A family-scoped resolve would silently miss it.
    val containment = Edge("meda", "schauburg", must = true, "containment")
    val crossing    = FamilyClosure.crossings(FamilyClosure.families(listings), Seq(Edge("murnau", "schauburg", must = true, "same-tmdb"), containment))
    crossing.map(c => (c.edge, c.familyOfA, c.familyOfB)) shouldBe Seq((containment, Some(0), Some(1)))
  }

  it should "count an edge to a listing outside every family as a crossing" in {
    FamilyClosure.crossings(FamilyClosure.families(listings), Seq(Edge("meda", "nowhere", must = false, "x"))).map(_.familyOfB) shouldBe
      Seq(None)
  }
}
