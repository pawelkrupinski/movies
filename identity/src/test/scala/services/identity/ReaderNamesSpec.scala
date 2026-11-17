package services.identity

import kinowo.build.ReaderNames
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which literals name a changed file for the test-impact selection (`project/ReaderNames.scala`): a lint walking a
 *  source tree (ListingConstraintsRoutingSpec walks `worker/src/main` for a direct MixedFilmDetector veto) reads every
 *  source under it, though no class dependency records it, so the narrowed unit tests of a `MovieService.scala` change
 *  must keep it. */
class ReaderNamesSpec extends AnyFlatSpec with Matchers {

  private def reads(source: String, path: String): Boolean =
    ReaderNames.literals(source).exists(ReaderNames.treeNames(path))

  "a tree walker's literal" should "name every file under the tree it walks" in {
    val routing = """private val Roots = Seq("common/src/main", "worker/src/main", "worker/src/fixtures")"""
    reads(routing, "worker/src/main/scala/services/movies/MovieService.scala") shouldBe true
    reads(routing, "worker/src/fixtures/scala/tools/ServedCorpusInvariants.scala") shouldBe true
    reads(routing, "worker/src/test/scala/services/movies/MovieServiceSpec.scala") shouldBe false
    reads("""Paths.get("test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions")""",
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/2026-11-16.html") shouldBe true
    reads("""Path.of("scrapers/src/main/scala/services/cinemas/")""",
      "scrapers/src/main/scala/services/cinemas/pl/HeliosClient.scala") shouldBe true
  }

  it should "name a file by its own path, and no tree by a top-level directory alone" in {
    reads(""""worker/src/main/scala/services/movies/MovieService.scala"""",
      "worker/src/main/scala/services/movies/MovieService.scala") shouldBe true
    reads("""Paths.get("worker")""", "worker/src/main/scala/services/movies/MovieService.scala") shouldBe false
    reads("""Paths.get("worker/src/mainline")""", "worker/src/main/scala/services/movies/MovieService.scala") shouldBe false
  }

  // review round 5: ProvenanceSpec walked a tree built from its parts (`"identity", "src", "main", ...`), which no one
  // literal names, so a
  // change to any identity source but its class dependencies' left it out of the narrowed unit tests
  "a source walking a module's tree" should "name it in one literal, never build it from parts" in {
    // a module and its `src` as two arguments: `"identity", "src"` and `"identity", "src/main"` alike
    val PartsPath = """(?:Path\.of|Paths\.get|new\s+(?:java\.io\.)?File)\(\s*"[^"/]+"\s*,\s*"src(?:"|/)""".r
    val sources = Using.resource(Files.list(Path.of(".")))(_.iterator.asScala.toList).map(_.resolve("src")).filter(Files.isDirectory(_))
      .flatMap(src => Using.resource(Files.walk(src))(_.iterator.asScala.filter(_.toString.endsWith(".scala")).toList))
    sources should not be empty
    sources.filter(file => PartsPath.findFirstIn(Files.readString(file)).isDefined).map(_.toString) shouldBe empty
  }

  private def globReads(source: String, path: String): Boolean =
    ReaderNames.literals(source).exists(ReaderNames.globWalks(_, path))

  // review round 6: the seven lints built on testsupport.ModuleSources walk every module's sources by no module's name,
  // so the narrowed unit tests of a `common`-only change left every one of them out
  "the walker of every module's sources" should "read a change in any module, by its tree glob" in {
    val moduleSources = Files.readString(Path.of("common/src/test/scala/testsupport/ModuleSources.scala"))
    globReads(moduleSources, "common/src/main/scala/clients/TmdbJson.scala") shouldBe true
    globReads(moduleSources, "scrapers/src/test/scala/services/cinemas/AnySpec.scala") shouldBe true
    globReads(moduleSources, "common/src/main/resources/application.conf") shouldBe false
    globReads(moduleSources, "docs/README.md") shouldBe false
  }

  "a tree glob" should "match a directory above the path segment by segment, and no route or package glob" in {
    ReaderNames.globWalks("*/src/*/scala", "web/src/main/scala/A.scala") shouldBe true
    ReaderNames.globWalks("*/src/*/scala", "web/src/main/resources/a.conf") shouldBe false
    ReaderNames.globWalks("test/**/flicks", "test/resources/fixtures/flicks/a.html") shouldBe true
    ReaderNames.globWalks("worker/src/main/scala/*.scala", "worker/src/main/scala/A.scala") shouldBe true
    ReaderNames.globWalks("/*/movies", "pl/movies") shouldBe false
    ReaderNames.globWalks("*", "web/src/main/scala/A.scala") shouldBe false
    ReaderNames.globWalks("services.identity.*", "services.identity.X") shouldBe false
  }

  "a tree walker's literal" should "never make a file named by its one-file tokens" in {
    // a tree's walker never says which file it reads: a fixture under a walked tree that no literal opens by name still
    // has an unknown reader, so the selection runs every test
    ReaderNames.tokens("test/resources/fixtures/flicks/x/day.html") should not contain "test/resources/fixtures/flicks"
  }
}
