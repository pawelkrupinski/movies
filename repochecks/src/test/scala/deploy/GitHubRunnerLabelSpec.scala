package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * EVERY JOB ASKS FOR A RUNNER GITHUB.COM HAS. GitHub is the only forge, and
 * a job asking for a label no runner carries (the old self-hosted tiers: `measure`, `main-lane`, ...) waits forever
 * instead of failing. Each job's `runs-on:` is read as github.com evaluates it on main (RunnerLabel); one that reads an
 * input or a needed job's output is judged by its fallback, and an input's own default is judged as well.
 */
class GitHubRunnerLabelSpec extends AnyFlatSpec with Matchers {
  private val Hosted = Set("ubuntu-22.04", "ubuntu-24.04", "ubuntu-26.04", "ubuntu-latest", "macos-latest")
  private val OnMain = RunnerLabel.Context(RunnerLabel.GitHub, RunnerLabel.Main)
  private val RunsOn = """(?m)^\s+runs-on:\s*(\$\{\{.*}}|\S+)""".r
  private val RunnerDefault = """(?ms)^\s+(?:runner|lane):\s*\n(?:\s+(?:description|type|required):.*\n)*\s+default:\s*(\S+)""".r

  "every job" should "ask for a runner github.com has" in {
    val stranded = for {
      file  <- RepoFile.workflows().sortBy(_.getName)
      yml    = RepoFile.withoutComments(RepoFile.read(file.getPath))
      label <- RunsOn.findAllMatchIn(yml).map(m => RunnerLabel(m.group(1).trim, OnMain)).toSeq ++
                 RunnerDefault.findAllMatchIn(yml).map(_.group(1).stripPrefix("'").stripSuffix("'")).toSeq
      if label.nonEmpty && !Hosted(label)
    } yield s"${file.getName} asks for `$label`"
    withClue("github.com has no such runner; the job would wait forever: ")(stranded shouldBe empty)
  }
}
