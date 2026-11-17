package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A test-report step never spends the GitHub API: every `mikepenz/action-junit-report` step is `annotate_only: true`.
 *
 * Without it the action creates a check run (and updates it per 50 annotations) through the repository's
 * GITHUB_TOKEN, whose budget every job of every overlapping run shares. Main #2035's worker-identity row (run
 * 41232892153) ended its report step on "Failed to create checks ... API rate limit exceeded for installation" while
 * two convergence runs had spent that budget. Annotations and the detailed job summary are workflow commands and a
 * file, no API; the suite step's own exit status grades the tests.
 */
class JunitReportNoCheckRunSpec extends AnyFlatSpec with Matchers {

  "every junit report step" should "annotate only, never create a check run through the API" in {
    val offenders = for {
      path <- RepoFile.ciFiles()
      lines = RepoFile.withoutComments(RepoFile.read(path)).linesIterator.toVector
      (line, at) <- lines.zipWithIndex
      if line.contains("uses: mikepenz/action-junit-report@")
      // the step's own keys: as deep as `uses:` or deeper, up to the next step's `- ` (shallower)
      keys = line.indexOf("uses:")
      step = lines.drop(at + 1).takeWhile(l => l.trim.isEmpty || l.indexWhere(!_.isWhitespace) >= keys)
      if !step.exists(_.trim == "annotate_only: true")
    } yield s"$path:${at + 1}"

    RepoFile.ciFiles().exists(p => RepoFile.read(p).contains("mikepenz/action-junit-report@")) shouldBe true
    withClue("a junit report that creates a check run spends the shared API budget and can fail its job on it: ") {
      offenders shouldBe empty
    }
  }
}
