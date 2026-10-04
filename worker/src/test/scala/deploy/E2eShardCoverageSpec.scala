package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards that every spec in the `e2e` module still runs somewhere after the
 * shards were re-cut.
 *
 * The module is split across CI runners by TAG: a spec carrying `@CorpusReplay`
 * gets a shard of its own and is named in a `build.sbt` alias, and everything
 * else rides `e2eRest`, which selects by tag EXCLUSION so a new spec can never
 * be silently dropped. ReScrapeIdempotencySpec moved from the first group to the
 * second — it had a whole runner to itself while finishing ~2 min inside the
 * build's long pole, and ci.yml is at GitHub's 20-runner cap, so the slot bought
 * more as an eighth WebKit page-test shard.
 *
 * The failure this guards is the halfway state: a spec untagged but still named
 * in an alias (run twice), or tagged with no shard to run it (run never, and
 * excluded from `e2eRest` too — invisible).
 */
class E2eShardCoverageSpec extends AnyFlatSpec with Matchers {
  private lazy val buildSbt = RepoFile.read("build.sbt")
  private lazy val ciYml    = RepoFile.read(".github/workflows/ci.yml")
  private lazy val e2eJob   = RepoFile.block(ciYml, "e2e")

  private lazy val specs: Vector[(String, String)] =
    RepoFile.listed("e2e/src/test/scala/services/movies")(_.getName.endsWith("Spec.scala"))
      .map(f => f.getName.stripSuffix(".scala") -> RepoFile.read(f.getPath))
      .toVector

  /** Tagged specs run by name; @CountryScoped ones belong to the convergence workflow, not ci.yml. */
  private lazy val ownShardSpecs: Vector[String] =
    specs.collect { case (name, src) if src.contains("@CorpusReplay") && !src.contains("@CountryScoped") => name }

  "the e2e module" should "have found its specs at all (guards this spec's own reader)" in {
    specs should not be empty
  }

  it should "give every own-shard spec a build.sbt alias that names it" in {
    ownShardSpecs should not be empty
    ownShardSpecs.foreach { name =>
      withClue(s"$name is tagged @CorpusReplay but no alias runs it: ") {
        buildSbt should include(s"e2e/Test/testOnly services.movies.$name")
      }
    }
  }

  it should "give every own-shard alias a runner in the ci matrix" in {
    val AliasName = """addCommandAlias\("(e2e[A-Za-z]+)",\s+"e2e/Test/testOnly services\.movies\.""".r
    val aliases   = AliasName.findAllMatchIn(buildSbt).map(_.group(1)).toVector
    aliases should have size ownShardSpecs.size
    aliases.foreach { alias =>
      withClue(s"$alias has no shard in the e2e matrix: ")(e2eJob should include(s"cmd: $alias"))
    }
  }

  /**
   * The other direction: a spec that gave up its shard must ALSO have given up
   * its tag, or `e2eRest`'s tag-exclusion drops it and nothing runs it at all.
   *
   * @CountryScoped specs are neither — they belong to identity-model-convergence.yml,
   * which names each one in its own alias, and `e2eRest` excludes that tag too.
   *
   * A convergence sample alias may name a rest-shard spec narrowed to its country's recorded
   * corpus (`-- -z recorded:<cc>`): the corpus-shape specs over the corpus only that leg restores.
   */
  it should "not name any spec that rides the rest shard" in {
    val ridingRest = specs.collect {
      case (name, src) if !src.contains("@CorpusReplay") && !src.contains("@CountryScoped") => name
    }
    ridingRest should not be empty
    withClue("These ride e2eRest but an alias also runs them by name: ") {
      E2eShardCoverageSpec.runWholeByName(buildSbt, ridingRest.toSet) shouldBe empty
    }
  }

  it should "read each testOnly command on its own: every spec it names, and only its own -z" in {
    val rest = Set("RestSpec")
    E2eShardCoverageSpec.runWholeByName("""e2e/Test/testOnly services.movies.OwnSpec services.movies.RestSpec"""", rest) shouldBe Seq("RestSpec")
    E2eShardCoverageSpec.runWholeByName(
      """"; e2e/Test/testOnly services.movies.RestSpec ; e2e/Test/testOnly services.movies.OwnSpec -- -z recorded:pl"""", rest) shouldBe Seq("RestSpec")
    E2eShardCoverageSpec.runWholeByName(
      """s"; e2e/Test/testOnly services.movies.$spec ; e2e/Test/testOnly services.movies.OwnSpec services.movies.RestSpec -- -z recorded:$code"""", rest) shouldBe empty
  }

  it should "keep the rest shard selecting by exclusion so a new spec can't be dropped" in {
    buildSbt should include("""addCommandAlias("e2eRest",     "e2e/Test/testOnly * -- -l services.movies.CorpusReplay""")
    e2eJob should include("cmd: e2eRest")
  }
}

object E2eShardCoverageSpec {
  private val TestOnly = """e2e/Test/testOnly\s+([^;"]*)""".r

  /** The `rest` specs some `testOnly` command in `buildSbt` runs whole: named among its specs (any position, not only
   *  the first) without its OWN arguments narrowing it to a recorded corpus (`-- -z recorded:<cc>`) — a `-z` on
   *  another command of the same alias or line narrows nothing here. */
  def runWholeByName(buildSbt: String, rest: Set[String]): Seq[String] =
    TestOnly.findAllMatchIn(buildSbt).flatMap { command =>
      val (names, arguments) = command.group(1).split("""\s+--\s+""", 2) match {
        case Array(n, a) => (n, a)
        case Array(n)    => (n, "")
      }
      if (arguments.contains("-z recorded:")) Nil
      else names.split("""\s+""").iterator.map(_.stripPrefix("services.movies.")).filter(rest)
    }.toSeq.distinct.sorted
}
