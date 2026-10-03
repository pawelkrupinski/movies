package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{CollectionRetention, Retention}

import ScalaSourceScan.{MainRoots, codeOf, scalaFiles}

/**
 * Every Mongo collection the code names declares how its documents stop being kept
 * ([[CollectionRetention]]): a TTL index, a sweep, or "kept forever" and why.
 *
 * Names are read from the shapes main sources spell them in: a literal handed to `getCollection`, a
 * `…Collection…` constant, a `collection`/`collectionName` parameter default, an index helper's
 * collection argument, the resolution caches' and the TMDB store's names.
 */
class CollectionRetentionSpec extends AnyFlatSpec with Matchers {

  private val Spellings = Seq(
    """getCollection(?:\[[^(]*?\])?\(\s*"([^"]+)"""",
    """\bval\s+\w*Collection\w*\s*(?::\s*String)?\s*=\s*"([^"]+)"""",
    """\b(?:collection|collectionName)\s*:\s*String\s*=\s*"([^"]+)"""",
    """\bMongoIndex\s*\.\s*\w+\(\s*\w+\s*,\s*"([^"]+)"""",
    """\bresolutionCache\(\s*"([^"]+)"""",
    """\bextends\s+TmdbKind\(\s*"([^"]+)"""").map(_.r)

  /** How many collections may still be [[Retention.Unswept]]: the backlog may only shrink — lower this
   *  when an entry gets its sweep or TTL. */
  private val UnsweptBacklog = 2

  private[tools] def namesIn(source: String): Set[String] =
    Spellings.flatMap(_.findAllMatchIn(source).map(_.group(1))).toSet

  private lazy val sources: Seq[String] = scalaFiles(MainRoots).map(codeOf)
  private lazy val named: Set[String]   = sources.flatMap(namesIn).toSet

  "Every collection main sources name" should "declare its retention" in {
    (named -- CollectionRetention.Declared.keySet) shouldBe empty
  }

  "The declarations" should "name only collections the sources still name" in {
    (CollectionRetention.Declared.keySet -- named) shouldBe empty
  }

  they should "name a sweep class that exists, and a TTL field some source indexes with an expiry" in {
    val classes = """\b(?:class|object)\s+(\w+)""".r
    val defined = sources.flatMap(s => classes.findAllMatchIn(s).map(_.group(1))).toSet
    val expiring = sources.filter(s => s.contains("expireAfter") || s.contains("MongoTtlIndex."))
    val broken = CollectionRetention.Declared.toSeq.collect {
      case (name, Retention.Sweep(by)) if !defined(by) => s"$name: no class $by"
      case (name, Retention.Ttl(field)) if !expiring.exists(_.contains(s"\"$field\"")) => s"$name: no expiring index on $field"
    }
    broken shouldBe empty
  }

  they should "keep the unswept backlog from growing" in {
    val unswept = CollectionRetention.Declared.collect { case (name, Retention.Unswept(_)) => name }
    withClue(s"unswept: ${unswept.toSeq.sorted.mkString(", ")} — give the new collection a TTL or a sweep\n")(
      unswept.size should be <= UnsweptBacklog)
  }

  "The name reader" should "find every spelling, and nothing else" in {
    namesIn(
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
