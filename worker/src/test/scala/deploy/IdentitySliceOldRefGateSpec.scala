package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml compiles identity-slice's tool files into a pinned old ref only when a push touches what the tool
 * brings to that ref: the ref is pinned, so nothing else can change the verdict. The gate lists the tool
 * files by path, so a file added to `TOOL_FILES` but not to the gate would ship unchecked — the exact
 * breakage (83b869548: every older ref failed to compile) the check exists to catch.
 */
class IdentitySliceOldRefGateSpec extends AnyFlatSpec with Matchers {
  private lazy val gate = RepoFile.step(RepoFile.read(".github/workflows/ci.yml"),
    "Did this change touch what identity-slice brings to an old ref?")

  /** The `patterns: |` block scalar's lines. */
  private lazy val patterns: Seq[String] = {
    val lines  = gate.linesIterator.toVector
    val start  = lines.indexWhere(_.trim == "patterns: |")
    require(start >= 0, "the gate has no `patterns: |` block")
    val indent = lines(start).takeWhile(_ == ' ').length
    lines.drop(start + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent)
      .map(_.trim).filter(_.nonEmpty)
  }

  private def covers(pattern: String, path: String): Boolean =
    if (pattern.endsWith("/**")) path.startsWith(pattern.stripSuffix("**")) else pattern == path

  private lazy val toolFiles: Seq[String] = {
    val script = RepoFile.read("scripts/identity-slice")
    val list   = """(?s)TOOL_FILES=\(([^)]*)\)""".r.findFirstMatchIn(script)
      .getOrElse(throw new AssertionError("scripts/identity-slice defines no TOOL_FILES=(…)")).group(1)
    list.split("\\s+").filter(_.nonEmpty).toSeq
  }

  "the old-ref compile's gate" should "cover every tool file identity-slice drops into an old ref" in {
    toolFiles should not be empty
    toolFiles.filterNot(f => patterns.exists(covers(_, f))) shouldBe empty
  }

  it should "cover the scripts that lay the ref out and the check itself" in {
    for (f <- Seq("scripts/identity-slice", "scripts/identity-tools.sh", "scripts/tool-conventions.sh",
                  "scripts/identity-slice-old-ref-test.sh", ".github/workflows/ci.yml"))
      withClue(f) { patterns.exists(covers(_, f)) shouldBe true }
  }

  it should "be what the compile step is conditioned on" in {
    RepoFile.step(RepoFile.read(".github/workflows/ci.yml"),
      "identity-slice's tool files still compile into a ref from before them") should include(
      "if: steps.slice-tools.outputs.changed == 'true'")
  }
}
