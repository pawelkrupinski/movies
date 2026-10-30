package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE DEVELOPMENT TOOLING'S OWN TESTS RUN OFF THE GATE (ci-failures-2026-10-30, recommendation 1): the script
 * self-tests and identity-slice's old-ref compile turned Main red 12 times in five days, each about time or process
 * lifecycle on a loaded host, none about a user-visible behaviour. They run in tool-self-tests.yml, on every push to
 * main touching what they test, and before landing in the pre-push hook's `scripttests` and `oldrefs` checks.
 */
class ToolSelfTestsOffGateSpec extends AnyFlatSpec with Matchers {
  private lazy val ci    = RepoFile.withoutComments(RepoFile.read(".github/workflows/ci.yml"))
  private lazy val tools = RepoFile.read(".github/workflows/tool-self-tests.yml")

  /** The `on.push.paths` list of tool-self-tests.yml. */
  private lazy val paths: Seq[String] = {
    val lines = RepoFile.block(RepoFile.block(tools, "on"), "push").linesIterator.toVector
    val start = lines.indexWhere(_.trim == "paths:")
    require(start >= 0, "tool-self-tests.yml's push has no `paths:`")
    lines.drop(start + 1).takeWhile(l => l.trim.isEmpty || l.trim.startsWith("-") || l.trim.startsWith("#"))
      .map(_.trim).filter(_.startsWith("-")).map(_.stripPrefix("-").trim.stripPrefix("'").stripSuffix("'"))
  }

  private def covers(pattern: String, path: String): Boolean =
    if (pattern.endsWith("/**")) path.startsWith(pattern.stripSuffix("**")) else pattern == path

  "Main's CI (ci.yml)" should "run neither the script self-tests nor the old-ref compile" in {
    RepoFile.jobs(ci).keySet should not contain "script-tests"
    ci should not include "scripts/run-script-tests"
    ci should not include "identity-slice-old-ref-test.sh"
  }

  "tool-self-tests.yml" should "run every script self-test and the old-ref compile, on pushes to main" in {
    val jobs = RepoFile.jobs(tools)
    jobs("script-tests") should include("run: scripts/run-script-tests")
    jobs("old-ref-compile") should include("bash scripts/identity-slice-old-ref-test.sh")
    RepoFile.block(tools, "on") should include("branches: [main]")
  }

  it should "run on a change under every root scripts/run-script-tests searches" in {
    val roots = """(?m)^Roots="([^"]*)"""".r.findFirstMatchIn(RepoFile.read("scripts/run-script-tests"))
      .getOrElse(fail("run-script-tests names no Roots")).group(1).split("\\s+").toSeq
    roots should not be empty
    roots.filterNot(r => paths.contains(s"$r/**")) shouldBe empty
  }

  it should "run on a change to every file identity-slice and title-trace drop into an old ref" in {
    val toolFiles = Seq("scripts/identity-slice", "scripts/title-trace").flatMap { tool =>
      """(?s)TOOL_FILES=\(([^)]*)\)""".r.findFirstMatchIn(RepoFile.read(tool)).getOrElse(fail(s"$tool: no TOOL_FILES"))
        .group(1).split("\\s+").filter(_.nonEmpty).toSeq
    }
    toolFiles should not be empty
    toolFiles.filterNot(f => paths.exists(covers(_, f))) shouldBe empty
  }

  "the pre-push hook" should "still run both before a change touching them lands" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("scripts/run-script-tests --changed")
    hook should include("scripts/identity-slice-old-ref-test.sh")
  }
}
