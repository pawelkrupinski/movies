package tools

import java.nio.file.{Files, Path, Paths}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{CollectionDatabase, MongoCollections}

import ScalaSourceScan.{codeOf, scalaFiles}

/**
 * The lists of collections the registry ([[MongoCollections]]) stands for, each held to what the code does: what the
 * nightly recording captures from prod, what the live drift check's resolve reads of those captures, and the fleet's
 * collections the migration doc names.
 */
class RegisteredCollectionListsSpec extends AnyFlatSpec with Matchers {

  private val FixtureRoot = "worker/src/fixtures"
  private val Recorder    = Paths.get("worker/src/fixtures/scala/scripts/RecordCorpusFixture.scala")
  private val LiveDrift   = Paths.get("worker/src/test/scala/scripts/IdentityLiveDrift.scala")

  private lazy val fixtureObjects: Map[String, Path] = scalaFiles(Seq(FixtureRoot)).flatMap { path =>
    """\bobject\s+(\w+)""".r.findAllMatchIn(codeOf(path)).map(_.group(1) -> path)
  }.toMap

  /** `entry` and every recording program file it reaches through the objects it names. */
  private def reached(entry: Path): Seq[Path] = {
    val named = """\b([A-Z]\w*)\b""".r
    Iterator.iterate((Set(entry), Seq(entry))) { case (seen, frontier) =>
      val next = frontier.flatMap(p => named.findAllMatchIn(codeOf(p)).flatMap(m => fixtureObjects.get(m.group(1)))).toSet -- seen
      (seen ++ next, next.toSeq)
    }.dropWhile(_._2.nonEmpty).next()._1.toSeq.sortBy(_.toString)
  }

  "The collections the registry marks recorded" should "be those the nightly recording opens" in {
    CollectionNames.openedFrom(reached(Recorder)) shouldBe MongoCollections.recorded
  }

  /** The captures the live drift check resolves over: every recording program whose capture it reads back (`readFrom`). */
  private def captures(entry: Path): Seq[Path] =
    """\b(\w+)\.readFrom\(""".r.findAllMatchIn(codeOf(entry)).flatMap(m => fixtureObjects.get(m.group(1))).toSeq.distinct.flatMap(reached).distinct

  "The collections the registry marks read by the resolve" should "be those the live drift check's resolve reads the captures of" in {
    CollectionNames.openedFrom(captures(LiveDrift)) shouldBe MongoCollections.readByResolve
  }

  "The reader of what a program opens" should "take a store's collection from its constructor's argument over the store's own" in {
    // ProdListings reads the intake's listings through the scrape archive's store, whose own collection is cinema_scrapes
    CollectionNames.openedFrom(Seq(Paths.get("worker/src/fixtures/scala/tools/ProdListings.scala"))) shouldBe Set("identity_listings")
  }

  "The fleet's collections" should "be those the fleet database migration doc lists" in {
    val doc   = new String(Files.readAllBytes(Paths.get("docs/fleet-database-migration.md")), "UTF-8")
    val table = """(?m)^\| `(\w+)` \|""".r.findAllMatchIn(doc).map(_.group(1)).toSet
    table shouldBe MongoCollections.in(CollectionDatabase.Fleet)
  }
}
