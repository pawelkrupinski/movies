package services.identity

import kinowo.build.ReaderNames
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/** Which sources read a changed file for the test-impact selection (`project/ReaderNames.scala`): a lint walking a
 *  source tree (ListingConstraintsRoutingSpec walks `worker/src/main` for a direct MixedFilmDetector veto) reads every
 *  source under it, though no class dependency records it, so the narrowed unit tests of a `MovieService.scala` change
 *  must keep it. Under a module's source tree the walk is the reader's DECLARATION (testsupport.SourceTrees); that every
 *  declarer in the repository is selected so is SourceTreeWalksLintSpec's (this spec declares no tree, so the
 *  selection never adds identity's test JVM to a change elsewhere). */
class ReaderNamesSpec extends AnyFlatSpec with Matchers {

  private def walks(text: String, path: String): Boolean =
    ReaderNames.walkers(path, Seq(ReaderNames.source("worker/src/test/scala/A.scala", text))).nonEmpty

  "a source-tree declaration" should "be the literals of a SourceTrees call, never any other literal" in {
    ReaderNames.declaredTrees("""private val Roots = SourceTrees("common/src/main", "worker/src/fixtures/")""") shouldBe
      Seq("common/src/main", "worker/src/fixtures")
    ReaderNames.declaredTrees("""SourceTrees.apply("web/src/main/twirl").path.resolve("x")""") shouldBe Seq("web/src/main/twirl")
    ReaderNames.declaredTrees("""val p = Paths.get("worker/src/main"); new SourceTrees(Seq("x"))""") shouldBe empty
    ReaderNames.declaredTrees("""// SourceTrees("worker/src/main")""") shouldBe empty
    ReaderNames.declaredTrees(""" val s = "SourceTrees(\"worker/src/main\")"; """) shouldBe empty
  }

  it should "walk every file under a tree it declares, by path or by glob" in {
    val routing = """private val Roots = SourceTrees("common/src/main", "worker/src/main", "worker/src/fixtures")"""
    walks(routing, "worker/src/main/scala/services/movies/MovieService.scala") shouldBe true
    walks(routing, "worker/src/fixtures/scala/tools/ServedCorpusInvariants.scala") shouldBe true
    walks(routing, "worker/src/test/scala/services/movies/MovieServiceSpec.scala") shouldBe false
    walks(routing, "worker/src/mainline/A.scala") shouldBe false
    val glob = """val Trees = SourceTrees("*/src/*/scala")"""
    walks(glob, "common/src/main/scala/clients/TmdbJson.scala") shouldBe true
    walks(glob, "scrapers/src/test/scala/services/cinemas/AnySpec.scala") shouldBe true
    walks(glob, "common/src/main/resources/application.conf") shouldBe false
  }

  // what the declaration displaced: a source tree no SourceTrees call declares is read by no literal naming it
  // (SourceTreeWalksLintSpec refuses one), so only a file's own path still names it there
  it should "read no undeclared literal as a walk of a source tree, but a file's own path as its read" in {
    walks("""Paths.get("worker/src/main")""", "worker/src/main/scala/services/movies/MovieService.scala") shouldBe false
    walks(""""worker/src/main/scala/services/movies/MovieService.scala";""",
      "worker/src/main/scala/services/movies/MovieService.scala") shouldBe true
  }

  "a walker of any other tree" should "name it in a literal: the path or a directory above it, never a top-level one" in {
    walks("""Paths.get("test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions")""",
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/2026-11-16.html") shouldBe true
    walks("""Paths.get("infra/kubernetes/worker")""", "infra/kubernetes/worker/deployment.yaml") shouldBe true
    walks("""Paths.get("infra")""", "infra/kubernetes/worker/deployment.yaml") shouldBe false
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

  "a bookkeeping file" should "be the ratchets' and the build's own tables and the AOT class list, never identity data" in {
    // the coupling and quality baselines and tables, the worker's AOT class list: regenerated or read only by tools
    // outside the identity specs, so a change to one seeds no spec and never falls back to every test (2026-11-19: 46
    // of 100 common/ commits ran every identity spec for a re-recorded coupling-baseline.tsv or aot-classes.txt)
    Seq("test/resources/coupling-baseline.tsv", "test/resources/coupling-fanout-refs.tsv", "test/resources/coupling-exemptions.tsv",
      "test/resources/coupling-layers.tsv", "test/resources/complexity-baseline.tsv", "test/resources/length-baseline.tsv",
      "test/resources/duplication-baseline.tsv", "worker/src/main/resources/aot-classes.txt")
      .filterNot(ReaderNames.isBookkeeping) shouldBe empty
    Seq("test/resources/fixtures/identity-judgements.tsv", "test/resources/fixtures/identity-unmatched/expected-matches.tsv",
      "common/src/main/resources/title-rules.tsv", "test/resources/fixtures/corpus/hard-clusters-seeds.tsv",
      "test/resources/expected-schedules.txt", "worker/src/main/resources/aot-classes.txt.bak")
      .filter(ReaderNames.isBookkeeping) shouldBe empty
  }

  it should "include the what-if records the stamp carry's rows stand on, never the carry table the build reads" in {
    // scripts/identity_stamp_carry.py alone reads a record; the table moves the stamps the identity specs read
    ReaderNames.isBookkeeping(s"test/resources/identity-what-if-verdicts/${"a" * 40}-${"b" * 40}.tsv") shouldBe true
    ReaderNames.isBookkeeping("test/resources/identity-stamp-carry.tsv") shouldBe false
  }
}
