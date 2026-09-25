package services.identity

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.FamilyClosure.Edge
import services.movies.TitleNormalizer

/**
 * The family (block-closure) check: a resolve may run family by family only while no constraint
 * edge joins two families, and the check must SEE an edge that does.
 */
class FamilyClosureSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.Germany)

  private def keys(title: String, original: Option[String] = None, tmdb: Option[Int] = None) =
    FamilyClosure.blockKeys(title, original, tmdb, normalizer)

  // DE convergence, 2026-09-25 — the Faust bug's shape. Murnau's film under two venues'
  // spellings, one resolved; Kulturfabrik Meda's "Zärtlich kreist die Faust" (1990), unresolved.
  private val listings: Map[String, Set[String]] = Map(
    "murnau"    -> keys("Faust - Eine deutsche Volkssage", tmdb = Some(10728)),
    "schauburg" -> keys("Faust", original = Some("Faust - Eine deutsche Volkssage")),
    "meda"      -> keys("Zärtlich kreist die Faust"))

  private final class Recording extends FamilyClosureMetrics {
    var crossings = Vector.empty[(String, Int, Seq[String])]
    var merges    = Vector.empty[(String, Int)]
    override def recordCrossings(scope: String, count: Int, reasons: Seq[String]): Unit = crossings :+= ((scope, count, reasons))
    override def recordFamilyMerges(scope: String, count: Int): Unit = merges :+= ((scope, count))
  }

  "the block closure" should "put listings sharing a title form, an original title or a TMDB id in one family" in {
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

  "the runtime check" should "pass edges drawn between listings that share a block key" in {
    val metrics = new Recording
    val edges   = Seq(Edge("murnau", "schauburg", must = true, "original-title"),
                      Edge("schauburg", "murnau", must = false, "different-tmdb"))
    FamilyClosure.check("de", listings, edges, metrics) shouldBe Right(FamilyClosure.families(listings))
    metrics.crossings shouldBe Vector(("de", 0, Nil))
  }

  it should "catch a containment edge, whose endpoints share no block key, and report it" in {
    // "Zärtlich kreist die Faust" ENDS with "Faust": the containment rule that adopted it drew
    // an edge the closure cannot see. A family-scoped resolve would silently miss it.
    val metrics     = new Recording
    val containment = Edge("meda", "schauburg", must = true, "containment")
    val result      = FamilyClosure.check("de", listings, Seq(Edge("murnau", "schauburg", must = true, "same-tmdb"), containment), metrics)
    result.left.map(_.map(_.edge)) shouldBe Left(Seq(containment))
    result.left.map(_.map(c => (c.familyOfA, c.familyOfB))) shouldBe Left(Seq((Some(0), Some(1))))
    metrics.crossings shouldBe Vector(("de", 1, Seq("containment")))
  }

  it should "count an edge to a listing outside every family as a crossing" in {
    FamilyClosure.crossings(FamilyClosure.families(listings), Seq(Edge("meda", "nowhere", must = false, "x"))).map(_.familyOfB) shouldBe
      Seq(None)
  }

  it should "count the families that merged since the previous resolve" in {
    val before  = FamilyClosure.families(listings)
    // A new listing whose original title bridges Meda's title and Murnau's joins the families.
    val bridged = listings + ("bridge" -> keys("Zärtlich kreist die Faust", original = Some("Faust")))
    val metrics = new Recording
    FamilyClosure.check("de", bridged, Nil, metrics, previous = before).map(_.values.toSet.size) shouldBe Right(1)
    metrics.merges shouldBe Vector(("de", 1))
    FamilyClosure.merges(before, before) shouldBe 0
  }
}
