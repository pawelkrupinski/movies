package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards what the page-test matrix actually SELECTS, which no other layer can
 * see: the rows are `--project` / `--grep` / `--shard` strings in YAML, and a
 * typo in one silently runs fewer tests while CI stays green.
 *
 * Two invariants, both of which a careless edit breaks quietly:
 *
 *  1. Shard coverage. A project set split `i/N` covers everything only if every
 *     `i` in `1..N` has a row. Dropping row 5 of 8 drops an eighth of the WebKit
 *     suite and nothing goes red.
 *
 *  2. Where the `@agnostic` specs run. Those ~13 viewport-INDEPENDENT spec
 *     files used to be re-run at every viewport of every phone — 208 of each
 *     Chromium row's 576 tests, 12× what the tag says is needed. They now run
 *     once per ENGINE, and the phone rows exclude them. Both halves have to hold
 *     together: exclude them from the phone rows without a row that runs them,
 *     and they stop running entirely.
 */
class PageTestShardCoverageSpec extends AnyFlatSpec with Matchers {
  private lazy val ciYml = RepoFile.read(".github/workflows/ci.yml")

  /** Every `runs:` record across both page-test jobs: (project, grep, grepInvert, shard). */
  private case class Selection(project: String, grep: String, grepInvert: String, shard: String)

  private lazy val selections: Vector[Selection] = {
    val jobs = Vector("page-tests-chrome", "page-tests-safari").map(RepoFile.block(ciYml, _)).mkString("\n")
    // `runs` takes two YAML shapes — a quoted one-liner (inside a flow mapping
    // or not) and a `|` block of several records — so read the quoted form by
    // regex and the block form by indentation, then treat both as records.
    val Quoted = """runs:\s*'([^']*)'""".r
    val quoted = Quoted.findAllMatchIn(jobs).map(_.group(1)).toVector
    val blocks = {
      val lines = jobs.linesIterator.toVector
      lines.zipWithIndex.collect { case (line, i) if line.trim == "runs: |" => (line.takeWhile(_ == ' ').length, i) }
        .flatMap { case (indent, i) =>
          lines.drop(i + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent)
        }
    }
    (quoted ++ blocks).map(_.trim).filter(_.count(_ == ';') == 3).map { record =>
      val f = record.split(";", -1).map(_.trim)
      Selection(f(0), f(1), f(2), f(3))
    }
  }

  private def shardsOf(projectSet: String): Vector[String] =
    selections.filter(_.project == projectSet).map(_.shard).filter(_.nonEmpty)

  private lazy val phoneSets: Vector[String] =
    selections.map(_.project).filter(_.contains(",")).distinct

  "the page-test matrix" should "have parsed some selections at all (guards this spec's own reader)" in {
    selections should not be empty
    phoneSets should have size 2 // one sharded phone set per engine
  }

  it should "cover every shard of every sharded project set" in {
    phoneSets.foreach { set =>
      val shards = shardsOf(set)
      val total  = shards.head.split("/")(1).toInt
      withClue(s"$set: ") {
        shards.map(_.split("/")(1).toInt).distinct shouldBe Vector(total)
        shards.map(_.split("/")(0).toInt).sorted shouldBe (1 to total).toVector
      }
    }
  }

  it should "run the viewport-independent specs exactly once per engine" in {
    val agnosticRows = selections.filter(_.grep == "@agnostic")
    agnosticRows should have size 2
    agnosticRows.map(_.project).count(_.startsWith("chromium-")) shouldBe 1
    agnosticRows.map(_.project).count(_.startsWith("webkit-")) shouldBe 1
  }

  /**
   * On a MOBILE project, not the desktop one. `card-tap.spec.ts` is @agnostic
   * and asserts on a touch interaction, so a desktop project — no touch, mouse
   * events — would run it in a context it was never about.
   */
  it should "run them in a touch context, not on the desktop project" in {
    selections.filter(_.grep == "@agnostic").foreach { row =>
      withClue(s"${row.project}: ") { row.project should not include "desktop" }
    }
  }

  it should "exclude them everywhere else, so no viewport re-runs the same assertions" in {
    selections.filter(_.grep != "@agnostic").foreach { row =>
      withClue(s"${row.project}: ") { row.grepInvert should include("@agnostic") }
    }
  }

  /** axe-core is informational and has its own step; no blocking row may run it. */
  it should "keep the informational a11y audit out of every blocking row" in {
    selections.foreach { row =>
      withClue(s"${row.project}: ") { row.grepInvert should include("axe-core") }
    }
  }
}
