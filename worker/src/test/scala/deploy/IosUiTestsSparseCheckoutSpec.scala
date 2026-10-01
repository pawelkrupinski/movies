package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * ios.yml's `ui-tests` job checks out only the paths its steps read (a sparse checkout, not the
 * 1.2 GB tree). A script it runs that sources a file outside those paths fails with exit 127 on the
 * runner — b85a62cef shipped exactly that: `ios-sim-destination-test.sh` sources `../shell-spec.sh`.
 * Every `scripts/…` file the job runs, and every file those scripts source, must be checked out.
 */
class IosUiTestsSparseCheckoutSpec extends AnyFlatSpec with Matchers {
  private lazy val job = RepoFile.jobs(RepoFile.read(".github/workflows/ios.yml"))("ui-tests")

  private lazy val sparse: Seq[String] = {
    val lines = job.linesIterator.toVector
    val start = lines.indexWhere(_.trim == "sparse-checkout: |")
    withClue("ui-tests has no sparse-checkout list: ")(start should be >= 0)
    val indent = lines(start).takeWhile(_ == ' ').length
    lines.drop(start + 1).takeWhile(l => l.trim.nonEmpty && l.takeWhile(_ == ' ').length > indent).map(_.trim.stripPrefix("/"))
  }

  private def covered(path: String): Boolean =
    sparse.exists(p => if (p.endsWith("/")) path.startsWith(p) else path == p)

  /** The repo-relative files a script sources with `. "$HERE/<rel>"` / `source "$HERE/<rel>"`. */
  private def sourcedBy(script: String): Seq[String] = {
    val dir = Paths.get(script).getParent
    """(?m)^\s*(?:\.|source)\s+"\$HERE/([^"]+)"""".r.findAllMatchIn(RepoFile.read(script))
      .map(m => dir.resolve(m.group(1)).normalize().toString).toSeq
  }

  "the ui-tests job" should "check out every script it runs and every file those scripts source" in {
    val commands = job.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    val scripts  = """\bscripts/[A-Za-z0-9_./-]+\.(?:sh|py)\b""".r.findAllIn(commands).toSeq.distinct
    scripts should not be empty
    val needed = (scripts ++ scripts.filter(_.endsWith(".sh")).flatMap(sourcedBy)).distinct
    needed.foreach(f => withClue(s"$f exists: ")(Files.exists(Paths.get(f)) shouldBe true))
    withClue(s"sparse paths $sparse miss: ")(needed.filterNot(covered) shouldBe empty)
  }
}
