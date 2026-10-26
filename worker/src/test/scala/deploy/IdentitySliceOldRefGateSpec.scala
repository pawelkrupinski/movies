package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml compiles identity-slice's and title-trace's tool files into a pinned old ref only when a push touches
 * what the tools bring to that ref: the ref is pinned, so nothing else can change the verdict. The gate lists the
 * tool files by path, so a file added to either's `TOOL_FILES` but not to the gate would ship unchecked — the exact
 * breakage (83b869548's `Capture.live`, 2026-10-17/18's `Listing.properTitle` and TitleFacts: every older ref
 * failed to compile) the check exists to catch.
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

  /** The tools that drop their tool files into an old ref, each checked by scripts/identity-slice-old-ref-test.sh. */
  private val tools = Seq("scripts/identity-slice", "scripts/title-trace")

  private lazy val toolFiles: Seq[String] = tools.flatMap { tool =>
    val list = """(?s)TOOL_FILES=\(([^)]*)\)""".r.findFirstMatchIn(RepoFile.read(tool))
      .getOrElse(throw new AssertionError(s"$tool defines no TOOL_FILES=(…)")).group(1)
    list.split("\\s+").filter(_.nonEmpty).toSeq
  }.distinct

  "the old-ref compile's gate" should "cover every tool file identity-slice and title-trace drop into an old ref" in {
    toolFiles should not be empty
    toolFiles.filterNot(f => patterns.exists(covers(_, f))) shouldBe empty
  }

  it should "cover the scripts that lay the ref out and the check itself" in {
    for (f <- tools ++ Seq("scripts/identity-tools.sh", "scripts/tool-conventions.sh",
                  "scripts/identity-slice-old-ref-test.sh", ".github/workflows/ci.yml"))
      withClue(f) { patterns.exists(covers(_, f)) shouldBe true }
  }

  it should "be what the compile's start and its verdict are conditioned on, on the row that computes it" in {
    val ci = RepoFile.read(".github/workflows/ci.yml")
    val condition = "if: matrix.row == 'rest' && steps.slice-tools.outputs.changed == 'true'"
    gate should include("if: matrix.row == 'rest'")
    val start   = RepoFile.step(ci, "Start identity-slice's old-ref compile, in the background")
    val verdict = RepoFile.step(ci, "identity-slice's tool files still compile into a ref from before them")
    start should include(condition)
    start should include("in-background.sh start old-ref-compile bash scripts/identity-slice-old-ref-test.sh")
    verdict should include(condition)
    verdict should include("in-background.sh wait old-ref-compile")
    // Started before itAll, so the tree it lays out copies the restored target/ before sbt writes to it.
    ci.indexOf("Start identity-slice's old-ref compile") should be < ci.indexOf("run: sbt itAll")
  }

  /** The job's checkout is shallow, so a floor the old-ref test compiles into but the start step does not fetch fails
   *  as "no commit … in this SHALLOW clone": 68370c42d added 9bb54c1a4 to the test only, and Main went red (1807). */
  it should "fetch every floor the old-ref test compiles into" in {
    val start  = RepoFile.step(RepoFile.read(".github/workflows/ci.yml"),
      "Start identity-slice's old-ref compile, in the background")
    val floors = """(?m)^floor\(\)(.*)$""".r.findFirstMatchIn(RepoFile.read("scripts/identity-slice-old-ref-test.sh"))
      .map(m => """\b[0-9a-f]{40}\b""".r.findAllIn(m.group(1)).toSeq).getOrElse(fail("the old-ref test defines no floor()"))
    val fetch = start.linesIterator.find(_.trim.startsWith("git fetch")).getOrElse(fail("the start step fetches nothing"))
    floors should not be empty
    floors.filterNot(fetch.contains) shouldBe empty
  }

  /** The compiled old tree is cached by the tool files: one the key missed would restore a tree compiled with its old
   *  copy as an EXACT hit — no save, and no difference for zinc to see beyond the file's own mtime. */
  it should "restore the compiled old ref before the compile, keyed by every tool file" in {
    val ci      = RepoFile.read(".github/workflows/ci.yml")
    val restore = RepoFile.step(ci, "Restore identity-slice's compiled old ref")
    restore should include("if: matrix.row == 'rest' && steps.slice-tools.outputs.changed == 'true'")
    restore should include("path: ${{ runner.temp }}/kinowo-identity/refs")
    ci.indexOf("Restore identity-slice's compiled old ref") should be < ci.indexOf("Start identity-slice's old-ref compile")
    val hashed = """hashFiles\(([^)]*)\)""".r.findFirstMatchIn(restore).map(_.group(1)).getOrElse(fail("the key hashes no files"))
      .split(",").map(_.trim.stripPrefix("'").stripSuffix("'")).toSeq
    (toolFiles ++ tools).filterNot(f => hashed.exists(covers(_, f))) shouldBe empty
  }
}
