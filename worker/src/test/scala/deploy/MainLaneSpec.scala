package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE PRODUCTION LANE: off github.com, Main and nix-stage-closures on main run on ci-runner's `main-lane`
 * runner, and nothing else does.
 *
 * Forgejo has no job priority: a runner takes the oldest waiting job its labels match. With every job on
 * the shared `ubuntu-*` runner, Main run #484 (a red-main fix) sat queued 40+ min behind ~39 dispatched
 * identity/convergence jobs on 2026-10-22, and production stayed on the old commit. So production asks for
 * a runner of its own (infra nix/hosts/ci-runner/forgejo.nix `main`, docs/forgejo-actions.md "Lanes").
 *
 * The lane holds only while it stays exclusive: one experiment workflow naming `main-lane` makes it a
 * second shared queue. And every mention is gated on `github.server_url`, because github.com has no such
 * runner and a job asking for it there would wait forever.
 */
class MainLaneSpec extends AnyFlatSpec with Matchers {

  private val Lane       = "main-lane"
  private val Production = Set("main.yml", "nix-stage-closures.yml")
  /** Reusable workflows production calls; their jobs take the caller's `lane` input. */
  private val Called     = Seq(".github/workflows/ci.yml", ".github/workflows/report-timings.yml")

  private def code(path: String) = RepoFile.withoutComments(RepoFile.read(path))
  private val RunsOn = """runs-on:\s*(\$\{\{[^}]*}}|\S+)""".r.unanchored

  "every production job" should s"run on $Lane off github.com" in {
    val off = for {
      file        <- Production.toSeq.sorted
      (name, job) <- RepoFile.jobs(code(s".github/workflows/$file"))
      ok = job match {
             case RunsOn(r) => r.contains(s"'$Lane'")
             case _         => job.linesIterator.exists(l => l.trim.startsWith("lane:") && l.contains(s"'$Lane'"))
           }
      if !ok
    } yield s"$file: $name"
    withClue("production jobs that would queue behind experiments: ")(off shouldBe empty)
  }

  "every job of a workflow production calls" should "take the caller's lane" in {
    val off = for {
      path        <- Called
      (name, job) <- RepoFile.jobs(code(path))
      if !(job match { case RunsOn(r) => r.startsWith("${{ inputs.lane || 'ubuntu-"); case _ => false })
    } yield s"$path: $name"
    withClue("jobs ignoring the lane input: ")(off shouldBe empty)
  }

  s"$Lane" should "be named by production workflows alone" in {
    val others = RepoFile.workflows()
      .filterNot(f => Production(f.getName))
      .filter(f => code(f.getPath).contains(Lane))
      .map(_.getName)
    withClue(s"$Lane is production's; an experiment there turns it back into a shared queue: ")(others shouldBe empty)
  }

  it should "be asked for only off github.com" in {
    val ungated = for {
      file <- Production.toSeq.sorted
      line <- code(s".github/workflows/$file").linesIterator
      if line.contains(s"'$Lane'") && !line.contains("github.server_url != 'https://github.com'") &&
         !line.contains("github.server_url == 'https://github.com' && 'ubuntu-")
    } yield s"$file: ${line.trim}"
    withClue("github.com has no such runner; the job would wait forever: ")(ungated shouldBe empty)
  }
}
