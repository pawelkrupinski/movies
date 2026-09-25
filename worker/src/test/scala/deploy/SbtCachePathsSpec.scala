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
 * recorder legs and ten convergence jobs a night, each spending ~100s compiling
 * from cold behind a 74 KB "Cache restored" — was in none of them.
 */
class SbtCachePathsSpec extends AnyFlatSpec with Matchers {

  private lazy val workflows: Seq[String] =
    RepoFile.workflows().map(_.getPath) ++ RepoFile.compositeActions()

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
    sbtCachePaths.map(_._1).distinct should contain allOf (
      ".github/workflows/ci.yml",
      ".github/workflows/record-scrape-fixtures.yml",
      ".github/actions/run-page-test/action.yml",
      ".github/actions/convergence-setup/action.yml",
    )
  }

  it should "carry the module classes and zinc state, which is the only part worth caching" in {
    sbtCachePaths.collect { case (file, paths) if !paths.contains("*/target/scala-*") => file } shouldBe empty
  }

  it should "not carry the root target, which is test reports and sbt scratch" in {
    sbtCachePaths.collect { case (file, paths) if paths.contains("target") => file } shouldBe empty
  }
}
