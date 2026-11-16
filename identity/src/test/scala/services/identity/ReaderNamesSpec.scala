package services.identity

import kinowo.build.ReaderNames
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

  it should "never make a file named by its one-file tokens" in {
    // a tree's walker never says which file it reads: a fixture under a walked tree that no literal opens by name still
    // has an unknown reader, so the selection runs every test
    ReaderNames.tokens("test/resources/fixtures/flicks/x/day.html") should not contain "test/resources/fixtures/flicks"
  }
}
