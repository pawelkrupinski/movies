package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * A `gh` or `curl` read in CI and fleet scripts is not silenced into "nothing there".
 *
 * `gh release download … 2>/dev/null` inside an `if`, or `gh run list … || true`, reads an auth
 * refusal, a 5xx or a network failure exactly like "no marker yet" / "no earlier run" — the
 * shell twin of the client bug `tools.ReadOutcome` exists for. A download whose target may
 * legitimately be absent goes through `scripts/ci/gh-optional.sh`, which tells the two apart;
 * any other read lets its failure fail the step.
 *
 * A site where carrying on is genuinely right — the failure makes the script do MORE work, or
 * the read is a probe whose failure is the measurement — says why in an `# allow-silenced: …`
 * comment on the line above (or the same line).
 */
class NoSilencedGhCurlSpec extends AnyFlatSpec with Matchers {

  private val Roots = Seq("scripts", ".github", "infra")
  private val Helper = "scripts/ci/gh-optional.sh"
  private val Tool     = """(?:^|[\s;|&($`])(?:gh|curl)\s""".r
  private val Silenced = """2>\s*/dev/null|&>\s*/dev/null|\|\|\s*true\b|\|\|\s*:(?:\s|$)""".r
  private val Allowed  = "allow-silenced:"

  private def files(root: String): Seq[Path] =
    if (!Files.isDirectory(Paths.get(root))) Nil
    else {
      val stream = Files.walk(Paths.get(root))
      try stream.iterator.asScala.filter(Files.isRegularFile(_)).toSeq finally stream.close()
    }

  private def scanned(path: String): Boolean =
    (path.endsWith(".sh") || path.endsWith(".yml") || path.endsWith(".yaml")) &&
      !path.endsWith("-test.sh") && path != Helper && !path.contains("/node_modules/") &&
      !path.startsWith("infra/kubernetes")

  /** Offending (line number, logical line) pairs: `\`-continued lines joined, comments dropped. */
  private[deploy] def offenders(text: String): Seq[(Int, String)] = {
    val lines = text.split("\n", -1).toVector
    var i = 0
    val out = Vector.newBuilder[(Int, String)]
    while (i < lines.length) {
      val start = i
      var logical = lines(i)
      while (logical.stripTrailing.endsWith("\\") && i + 1 < lines.length) {
        i += 1
        logical = logical.stripTrailing.dropRight(1) + " " + lines(i).trim
      }
      val code = if (logical.trim.startsWith("#")) "" else logical.split(" #", 2).head
      val excused = logical.contains(Allowed) || (start > 0 && lines(start - 1).contains(Allowed))
      if (!excused && Tool.findFirstIn(code).isDefined && Silenced.findFirstIn(code).isDefined)
        out += (start + 1 -> code.trim)
      i += 1
    }
    out.result()
  }

  "CI and fleet scripts" should "not silence a gh or curl read into 'nothing there'" in {
    val found = Roots.flatMap(files).map(_.toString).filter(scanned).sorted.flatMap { path =>
      offenders(RepoFile.read(path)).map { case (line, code) => s"$path:$line  $code" }
    }
    withClue(s"Use $Helper for a download that may be absent, let any other read fail, or say why with " +
      s"'# $Allowed …' on the line above:\n  ${found.mkString("\n  ")}\n") {
      found shouldBe empty
    }
  }

  "The lint" should "see silenced gh/curl reads, continued lines included, and honour an allow-silenced reason" in {
    val script =
      """if gh release download t --pattern m 2>/dev/null; then
        |  x=1
        |fi
        |base=$(gh run list --limit 1 \
        |        --json headSha || true)
        |# allow-silenced: a probe
        |status=$(curl -s "$u") || true
        |n=$(curl -sS "$u")
        |grep -q x file 2>/dev/null
        |# gh release download t 2>/dev/null
        |""".stripMargin
    offenders(script).map(_._1) shouldBe Seq(1, 4)
  }
}
