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
    Seq(".github/actions/setup-sbt-build/action.yml", ".github/actions/run-page-test/action.yml", ".github/actions/convergence-setup/action.yml")
      .foreach(file => RepoFile.read(file) should include("uses: ./.github/actions/sbt-target-cache"))
    RepoFile.read(".github/workflows/ci.yml") should include("uses: ./.github/actions/setup-sbt-build")
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
  private lazy val setup = RepoFile.read(".github/actions/convergence-setup/action.yml")
  private lazy val e2eJob = RepoFile.block(RepoFile.read(".github/workflows/ci.yml"), "e2e")
  private def keyStep(file: String): String =
    file.linesIterator.dropWhile(!_.contains("id: sbt-key")).takeWhile(!_.trim.startsWith("- uses:")).mkString("\n")

  "the convergence legs' build cache key" should "come from git's index, not a hashFiles walk of the workspace" in {
    keyStep(setup) should include("scripts/ci/sbt-sources-key.sh convergence")
    keyStep(setup) should not include "hashFiles("
    RepoFile.read("scripts/ci/sbt-sources-key.sh") should include("git ls-files -s")
  }

  /** Main dispatches the legs once `ci` is done, its e2e job included, which compiles what a leg compiles (e2e/Test)
   *  for the same commit -- where the newest convergence entry is an older commit's, and every leg recompiled the
   *  difference: 20-72 s in each of 14 suite jobs (run 38599662543). So a leg restores that exact entry first, named
   *  by the one script both jobs key with. */
  it should "try Main's e2e compile of the same commit before any older convergence entry" in {
    keyStep(setup) should include("scripts/ci/sbt-sources-key.sh e2e")
    keyStep(e2eJob) should include("scripts/ci/sbt-sources-key.sh e2e")
    e2eJob should include("key: sbt-target-e2e-${{ runner.os }}-${{ steps.sbt-key.outputs.e2e }}")
    restoreKeys.lift(1) shouldBe Some("sbt-target-e2e-${{ runner.os }}-${{ steps.sbt-key.outputs.e2e }}")
    restoreKeys.indexOf("sbt-target-convergence-${{ runner.os }}-") should be > 1
  }

  private lazy val restoreKeys =
    setup.linesIterator.dropWhile(!_.trim.startsWith("restore-keys:")).drop(1).map(_.trim).takeWhile(_.nonEmpty).toSeq
  private lazy val leg = RepoFile.read(".github/workflows/country-convergence-leg.yml")

  /** A leg's takes row compiles worker/Test and worker/It (identity-takes-check.sh), which no other row does, and the
   *  convergence key's entry is whichever row saves first -- a suite row's, without them: every takes row, on main and
   *  in every measurement, compiled all 957 of their sources, ~3 min a row (runs 38693633421, 38703015688). So main's
   *  takes row saves a family of its own once they exist, and a takes row restores that family first. */
  "a leg's takes row" should "restore main's newest takes build first, and only a takes row" in {
    restoreKeys.headOption shouldBe Some("${{ inputs.takes == 'true' && format('sbt-target-takes-{0}-', runner.os) || '' }}")
    leg should include("takes:         ${{ startsWith(matrix.phase, 'takes') }}")
  }

  it should "save its worker test and it classes as that family, on main, once they were compiled" in {
    val save = leg.linesIterator.dropWhile(!_.contains("key: sbt-target-takes-")).take(2).mkString("\n")
    save should include("key: sbt-target-takes-${{ runner.os }}-${{ steps.setup.outputs.sbt-sources }}")
    save should include("save-only: 'true'")
    RepoFile.step(leg, "Did the takes check compile worker's test and it classes?") should include("github.ref == 'refs/heads/main'")
    RepoFile.step(leg, "Did the takes check compile worker's test and it classes?") should include("it-classes")
    keyStep(setup) should include("scripts/ci/sbt-sources-key.sh convergence")
  }

  /** After an eviction no takes entry existed and a takes check that compiled nothing saved none, so every drift row of
   *  five runs in a row recompiled scrapers/Test and worker/Test (~170 s a row; docs/investigations/2026-11-01-twenty-runners.md).
   *  The first drift seed saves the family when what it restored was not a takes entry. */
  it should "also save the first drift seed's worker test classes as that family when no takes entry was restored" in {
    val decide = RepoFile.step(leg, "Did the takes check compile worker's test and it classes?")
    decide should include("(matrix.phase == 'takes' || matrix.phase == 'takes-drift-1')")
    decide should include("RESTORED: ${{ steps.setup.outputs.sbt-target-restored }}")
    decide should include("""elif [[ "$RESTORED" != sbt-target-takes-* ]]""")
    setup should include("value: ${{ steps.sbt-target.outputs.matched-key }}")
    RepoFile.read(".github/actions/sbt-target-cache/action.yml") should include(
      "value: ${{ steps.cache.outputs.cache-matched-key || steps.restore.outputs.cache-matched-key }}")
  }

  /** A branch's cache is restorable only from that branch and main, and a measure branch never runs again: each
   *  measurement saved 82 MB toward the repository's 10 GB cap for no one. */
  "a measurement's legs" should "save no build cache" in {
    setup should include("restore-only: ${{ inputs.replay != 'true' || startsWith(github.ref, 'refs/heads/measure/') }}")
  }
}
