package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{MongoCollections, Retention}

import ScalaSourceScan.{MainRoots, codeOf, scalaFiles}

/**
 * Every Mongo collection the code opens is registered ([[MongoCollections]]), declaring how its documents stop being
 * kept: a TTL index, a sweep, or "kept forever" and why. The names are those main sources and the recording's programs
 * spell ([[CollectionNames]]): a collection opened by a name not registered fails here.
 */
class CollectionRetentionSpec extends AnyFlatSpec with Matchers {

  /** How many collections may still be [[Retention.Unswept]]: the backlog may only shrink — lower this
   *  when an entry gets its sweep or TTL. */
  private val UnsweptBacklog = 2

  private lazy val sources: Seq[String] = scalaFiles(MainRoots).map(codeOf)
  private val named                      = CollectionNames.named
  private val declared                   = MongoCollections.all.map(c => c.name -> c.retention).toMap

  "Every collection main sources name" should "declare its retention" in {
    withClue("declare each new collection in services.MongoCollections, its retention " +
      "Ttl(\"expiryField\") | Sweep(\"SweepingClass\") | KeptForever(\"why\") (Unswept is a backlog that may only shrink): ")(
      (named -- declared.keySet) shouldBe empty)
  }

  "The declarations" should "name only collections the sources still name" in {
    withClue("no main source names these collections any more — move them to services.MongoCollections.retired " +
      "(and drop the collection itself in prod if it still holds data): ")(
      (declared.keySet -- named) shouldBe empty)
  }

  "The retired collections" should "be opened by no source" in {
    withClue("a source opens a collection services.MongoCollections.retired names — register it again, or stop opening it: ")(
      (MongoCollections.retired.map(_.name).toSet intersect named) shouldBe empty)
  }

  "The declarations" should "name a sweep class that exists, and a TTL field some source indexes with an expiry" in {
    val classes = """\b(?:class|object)\s+(\w+)""".r
    val defined = sources.flatMap(s => classes.findAllMatchIn(s).map(_.group(1))).toSet
    val expiring = sources.filter(s => s.contains("expireAfter") || s.contains("MongoTtlIndex."))
    val broken = declared.toSeq.collect {
      case (name, Retention.Sweep(by)) if !defined(by) => s"$name: no class $by"
      case (name, Retention.Ttl(field)) if !expiring.exists(_.contains(s"\"$field\"")) => s"$name: no expiring index on $field"
    }
    withClue("point each Sweep at the class that deletes the rows, and each Ttl at the field its expiring index uses: ")(
      broken shouldBe empty)
  }

  they should "keep the unswept backlog from growing" in {
    val unswept = declared.collect { case (name, Retention.Unswept(_)) => name }
    withClue(s"unswept: ${unswept.toSeq.sorted.mkString(", ")} — give the new collection a TTL or a sweep\n")(
      unswept.size should be <= UnsweptBacklog)
  }

  "The name reader" should "find every spelling, and nothing else" in {
    CollectionNames.namesIn(
      """db.getCollection[Document]("a_lit")
        |val MetaCollection = "b_const"
        |class Store(db: Db, collectionName: String = "c_default")
        |MongoIndex.ensure(db, "d_index", Indexes.ascending("id"))
        |lazy val x = resolutionCache("e_resolve")
        |case Film extends TmdbKind("f_kind", None)
        |val title = "not_a_collection"
        |""".stripMargin) shouldBe Set("a_lit", "b_const", "c_default", "d_index", "e_resolve", "f_kind")
  }
}
