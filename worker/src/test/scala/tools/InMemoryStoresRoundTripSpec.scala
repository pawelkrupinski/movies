package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, TestRoots, codeOf, scalaFiles}

/**
 * Every in-memory stand-in for a Mongo-backed store writes through the round-trip base ([[services.BsonRoundTrip]]):
 * each value it is handed is written through the store's own codec to BSON and read back, so a spec against it reads
 * what Mongo would hold and fails where Mongo would refuse. A NUL in an agreement receipt's field name passed every
 * unit spec because the in-memory store kept what it was handed (2026-10-25).
 *
 * An `InMemory*` class stands in for a Mongo-backed store when a `Mongo*` class in main sources extends one of the
 * types it extends. Its body must name `BsonRoundTrip`, or build on `InMemoryKeyedDocStore`, which does.
 */
class InMemoryStoresRoundTripSpec extends AnyFlatSpec with Matchers {

  /** Stand-ins with nothing to round-trip, and why. */
  private val Exempt = Map(
    "InMemoryReviewSource"      -> "read-only: it holds what its spec constructs it with, and nothing writes through it",
    "InMemoryScheduledRunStore" -> "holds only the claimed occurrence ids, never a document")

  private val Header = """(?m)^(?:final\s+|abstract\s+|sealed\s+)*class\s+(\w+)[^{]*""".r
  private val TopLevel = """(?m)^(?:final\s+|abstract\s+|sealed\s+|private\s+|case\s+)*(?:class|object|trait|enum)\s""".r

  /** Each class `source` defines: its name, the types it extends, and its body. */
  private[tools] def classesIn(source: String): Seq[(String, Set[String], String)] =
    Header.findAllMatchIn(source).map { m =>
      val end     = TopLevel.findAllMatchIn(source).map(_.start).find(_ > m.start).getOrElse(source.length)
      val parents = m.matched.split("""\b(?:extends|with)\b""").drop(1).map(_.trim.takeWhile(c => c.isLetterOrDigit || c == '_' || c == '.'))
        .map(_.split('.').last).filter(_.nonEmpty).toSet
      (m.group(1), parents, source.substring(m.start, end))
    }.toSeq

  /** The stand-ins in `sources` that stand in for a type a `Mongo*` class in `mains` extends and do not round-trip. */
  private[tools] def notRoundTripping(sources: Seq[String], mains: Seq[String]): Seq[String] = {
    val mongoBacked = mains.flatMap(classesIn).collect { case (name, parents, _) if name.startsWith("Mongo") => parents }.flatten.toSet
    sources.flatMap(classesIn).collect {
      case (name, parents, body) if name.startsWith("InMemory") && !name.endsWith("Spec") && parents.exists(mongoBacked) &&
          !body.contains("BsonRoundTrip") && !body.contains("InMemoryKeyedDocStore(") && !Exempt.contains(name) => name
    }.sorted
  }

  "Every in-memory stand-in for a Mongo-backed store" should "write each value through the round-trip base" in {
    val mains = scalaFiles(MainRoots).map(codeOf)
    withClue("write each stored value through services.BsonRoundTrip with the Mongo store's own codec (or build on " +
      "InMemoryKeyedDocStore), or exempt the class here with why it holds nothing to round-trip: ")(
      notRoundTripping(scalaFiles(MainRoots ++ TestRoots).map(codeOf), mains) shouldBe empty)
  }

  "The reader" should "flag a stand-in for a Mongo-backed store that keeps what it is handed, and nothing else" in {
    val mains = Seq(
      """trait Store { def put(v: String): Unit }
        |final class MongoStore(db: Db) extends Store with Logging {
        |  def put(v: String): Unit = ()
        |}
        |trait Plain""".stripMargin)
    val sources = Seq(
      """final class InMemoryStore extends Store {
        |  def put(v: String): Unit = kept += v
        |}
        |final class InMemoryRoundTripping extends Store {
        |  def put(v: String): Unit = kept += services.BsonRoundTrip.via(encode, decode)(v)
        |}
        |class InMemoryPlain extends Plain
        |""".stripMargin)
    notRoundTripping(sources, mains) shouldBe Seq("InMemoryStore")
  }
}
