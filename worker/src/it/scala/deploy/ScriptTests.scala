package deploy

import scala.sys.process.*

/**
 * The script self-tests as scripts/run-script-tests finds them — the one list of them, so a spec asking
 * "does CI run this test?" reads the runner's own discovery rather than re-deriving it, or a workflow line.
 */
object ScriptTests {

  /** One discovered test: its repo path, its `# script-test: <tag> <why>` tag (none: the default run), and why. */
  final case class Listed(path: String, tag: Option[String], reason: String)

  lazy val all: Seq[Listed] =
    Process(Seq("bash", "scripts/run-script-tests", "--list", "--all")).!!.linesIterator.filter(_.nonEmpty).map { line =>
      line.split("\t", -1).toSeq match {
        case Seq(path, "-", _)         => Listed(path, None, "")
        case Seq(path, tag, reason)    => Listed(path, Some(tag), reason)
        case other                     => throw new AssertionError(s"run-script-tests --list printed $other")
      }
    }.toSeq

  /** The runner over everything, as a `run:` line: no filter, no tag, at most a pool size or a timeout. */
  private val FullRun = """.*(^|\s)scripts/run-script-tests(\s+--(jobs|timeout)\s+\d+)*\s*""".r

  /** ci.yml runs the runner's full, untagged set. */
  lazy val ciRunsAll: Boolean =
    RepoFile.runSteps(RepoFile.read(".github/workflows/ci.yml")).exists(_.script.linesIterator.exists(FullRun.matches))

  /** CI runs `path` through the runner: discovered, untagged, and ci.yml runs the full set. */
  def runInCi(path: String): Boolean = ciRunsAll && all.exists(test => test.path == path && test.tag.isEmpty)
}
