package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards WHICH directories the sbt `actions/cache` blocks carry.
 *
 * sbt leaves each module's compiled classes and its zinc incremental state in
 * `<module>/target/scala-<v>/`, and the build definition's own compile in
 * `project/target`. The root `target/` holds neither — only `test-reports`,
 * rewritten every run, and `bg-jobs` scratch, measured locally at 235 MB of a
 * 239 MB directory.
 *
 * Every cache block in this repo used to name that root and none of the modules.
 * The effect was invisible because nothing failed: jobs restored a cache, the
 * logs said "Cache restored", and then sbt compiled every module from cold
 * anyway. It is most of what each page-test row's FixtureServerMain boot costs,
 * paid on 13 runners at once.
 *
 * A cache that silently caches the wrong thing has no failing symptom, so this
 * spec is the symptom. It reads EVERY workflow and composite action rather than a
 * list: it once named three files, and the convergence legs' cache — five
 * recorder legs and ten convergence jobs a night (the recorder's own scrape jobs
 * since folded into its legs), each spending ~100s compiling
 * from cold behind a 74 KB "Cache restored" — was in none of them.
 */
class SbtCachePathsSpec extends AnyFlatSpec with Matchers {

  private val SharedCache = ".github/actions/sbt-target-cache/action.yml"
  private lazy val workflows: Seq[String] = RepoFile.ciFiles()

  /** The `path:` block of every cache step that is caching sbt output. */
  private lazy val sbtCachePaths: Seq[(String, Vector[String])] =
    workflows.flatMap { file =>
      val lines = RepoFile.read(file).linesIterator.toVector
      lines.zipWithIndex.collect { case (line, i) if line.trim == "path: |" => (line.takeWhile(_ == ' ').length, i) }
        .map { case (indent, i) =>
          file -> lines.drop(i + 1)
            .takeWhile(l => l.trim.nonEmpty && l.takeWhile(_ == ' ').length > indent)
            .map(_.trim)
        }
        .filter { case (_, paths) => paths.exists(_.startsWith("project/target")) }
    }

  "the sbt caches" should "have been found at all (guards this spec's own reader)" in {
    sbtCachePaths.map(_._1).distinct should contain(SharedCache)
  }

  /** Ten cache blocks once repeated this path list, and copies of a list drift apart. Every job
   *  caches its build through the one composite action. */
  it should "name their paths in one place, the shared action every Scala job uses" in {
    sbtCachePaths.map(_._1).distinct shouldBe Seq(SharedCache)
    Seq(".github/workflows/ci.yml", ".github/actions/run-page-test/action.yml", ".github/actions/convergence-setup/action.yml")
      .foreach(file => RepoFile.read(file) should include("uses: ./.github/actions/sbt-target-cache"))
  }

  it should "carry the module classes and zinc state, which is the only part worth caching" in {
    sbtCachePaths.collect { case (file, paths) if !paths.contains("*/target/scala-*") => file } shouldBe empty
  }

  it should "not carry the root target, which is test reports and sbt scratch" in {
    sbtCachePaths.collect { case (file, paths) if paths.contains("target") => file } shouldBe empty
  }

  /** `hashFiles` walks and reads the workspace to hash the sources, 8.8 s of the US recording's
   *  setup even with its fixture tree staged outside the workspace (run 37111868620); git's index
   *  already holds each source's content hash. */
  "the convergence legs' build cache key" should "come from git's index, not a hashFiles walk of the workspace" in {
    val step = RepoFile.read(".github/actions/convergence-setup/action.yml").linesIterator
      .dropWhile(!_.contains("id: sbt-key")).takeWhile(!_.trim.startsWith("- uses:")).mkString("\n")
    step should include("git ls-files -s")
    step should not include "hashFiles("
  }
}
