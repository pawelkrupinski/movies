package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The nightly order-independence workflow's `seed` dispatch input is free text, and it
 * reaches shells and the Android emulator runner's `script:`. Interpolated there as
 * `${{ ... }}` it is spliced into the script BEFORE the shell parses it, so a "seed" of
 * `1; curl … | sh` runs. It is checked once, digits only (scripts/ci/order-seed.sh,
 * whose own test is scripts/ci/order-seed-test.sh), and every leg reads the checked
 * value through `env:`.
 */
class OrderIndependenceSeedSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/order-independence.yml")
  private lazy val jobs = RepoFile.block(workflow, "jobs")
  private lazy val code = workflow.linesIterator.filterNot(_.trim.startsWith("#")).toVector

  "the order-independence workflow" should "read the dispatch seed in one place, the job that checks it" in {
    code.count(_.contains("inputs.seed")) shouldBe 1
    val seedJob = RepoFile.block(jobs, "seed")
    seedJob should include("REQUESTED: ${{ inputs.seed }}")
    seedJob should include("scripts/ci/order-seed.sh \"$REQUESTED\"")
  }

  it should "give every shuffled leg the checked seed, not its own reading of the input" in {
    for (job <- Seq("scala", "android", "android-instrumented", "swift"))
      withClue(s"$job: ") { RepoFile.block(jobs, job) should include("needs: seed") }
  }

  it should "interpolate no expression into a run or emulator script body" in {
    // Single-line `run:` / `script:` values; the multi-line `run: |` bodies are the lines
    // indented under them, which never carry an expression either.
    val scripts = code.filter(l => l.trim.startsWith("run:") || l.trim.startsWith("script:"))
    scripts.filter(_.contains("${{")) shouldBe empty
    val bodies = workflow.linesIterator.toVector.zipWithIndex.collect {
      case (l, i) if l.trim == "run: |" => l.takeWhile(_ == ' ').length -> i
    }.flatMap { case (indent, i) =>
      workflow.linesIterator.toVector.drop(i + 1)
        .takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent)
    }
    bodies.filter(_.contains("${{")) shouldBe empty
  }
}
