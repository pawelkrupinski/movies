package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Keeps every throwaway mongod CI and the local scripts start on the SAME server
 * release mongo-1 runs.
 *
 * The production server is `roles/mongodb.nix`'s package; the CI replica set and
 * the local convergence runner are `docker run` of a `mongo:<tag>` image. Nothing tied the two
 * together, and they drifted a whole major apart: CI tested against 7.0 for the
 * month prod ran 8.2, so a query, index or aggregation stage that behaves
 * differently across those releases was green in every layer and only met the real
 * server in production. The tag is pinned to the exact patch, not a floating `8`
 * or `8.3`, so a run is reproducible and a bump is one deliberate edit here plus
 * the nix role — which this spec forces to happen together.
 */
class MongoServerVersionParitySpec extends AnyFlatSpec with Matchers {

  private val role = "infra/nix/modules/roles/mongodb.nix"

  private val dockerStarts = Seq(
    "scripts/ci/start-mongo-replset.sh",
    "scripts/convergence-local.sh"
  )

  private lazy val productionVersion: String =
    """mongodbVersion\s*=\s*"([0-9.]+)"""".r
      .findFirstMatchIn(RepoFile.read(role))
      .map(_.group(1))
      .getOrElse(fail(s"$role no longer names `mongodbVersion = \"x.y.z\"`"))

  // Every `mongo:<tag>` image reference, bare or registry-qualified (`mirror.gcr.io/library/mongo:`,
  // `docker.io/library/mongo:`): the CI script names its image in a variable, a mirror first and
  // Docker Hub as the fallback, and both must be the production release.
  private def dockerTags(path: String): Seq[String] =
    """(?<![\w.-])(?:[\w.-]+/)*mongo:([0-9][0-9.]*)""".r.findAllMatchIn(RepoFile.read(path)).map(_.group(1)).toSeq

  "every docker-started mongod" should "run the exact server release production runs" in {
    dockerStarts.foreach { path =>
      withClue(s"$path must start mongo:$productionVersion, the version $role deploys: ") {
        dockerTags(path) should not be empty
        all(dockerTags(path)) shouldBe productionVersion
      }
    }
  }

  // The NATIVE mongod the Forgejo job image bakes (start-mongo-replset.sh's MONGO_NATIVE_HOME, /opt/mongodb/<version>,
  // copied out of the same mongo:<version> image by infra's nix/files/forgejo-job-image): the same release, or the
  // script would start another one wherever the image carries it.
  "the job image's native mongod" should "be the exact server release production runs" in {
    val homes = """/opt/mongodb/([0-9][0-9.]*)""".r.findAllMatchIn(RepoFile.read("scripts/ci/start-mongo-replset.sh")).map(_.group(1)).toSeq
    withClue(s"scripts/ci/start-mongo-replset.sh must start the native mongod $productionVersion: ") {
      homes should not be empty
      all(homes) shouldBe productionVersion
    }
  }

  /** mongod 8.3 refuses to start on Linux 6.19 through 7.0.13 ("Linux kernel versions 6.19 and newer has a
   *  known incompatibility", SERVER-121912; TCMalloc's rseq use), and the ubuntu-26.04 runner boots 7.0 — its first
   *  Main run (38781236870) died there. Since the 2026-10-25 image ubuntu-24.04 boots one too (github.com Main
   *  41182034274: both integration rows). Until production's release starts on that kernel, every job that starts
   *  the pinned mongod runs on ubuntu-22.04 (kernel 6.8) ON GITHUB.COM: its `runs-on:` evaluated there (RunnerLabel),
   *  or, for a job running on `${{ inputs.runner }}`, that input's default and every caller's `runner:` evaluated there.
   *  Off github.com every label -- ubuntu-*, and each tier's (RunnerClassSpec) -- is one job image on ci-runner's
   *  kernel, so `${{ inputs.lane || github.server_url == 'https://github.com' && 'ubuntu-22.04' || 'ubuntu-24.04' }}`
   *  and `... && 'ubuntu-22.04' || 'measure'` pass. */
  private val MongoStarts  = Seq("scripts/ci/start-mongo-replset.sh", "./.github/actions/convergence-setup")
  private val MongoRunner  = "ubuntu-22.04"
  private val OnGitHub     = RunnerLabel.Context(RunnerLabel.GitHub, RunnerLabel.Main)

  /** A `runs-on` that reads its plan job's output (WorkflowPlanSpec) is that output's expression, as the plan declares it. */
  private def planned(jobs: Map[String, String], runner: String): String = runner match {
    case s"$${{ needs.$job.outputs.$key }}" if jobs.contains(job.trim) =>
      RepoFile.block(jobs(job.trim), "outputs").linesIterator.map(_.trim)
        .collectFirst { case line if line.startsWith(s"${key.trim}:") => ForgejoParse.stripComment(line.drop(key.trim.length + 1).trim) }
        .getOrElse(runner)
    case other => other
  }

  "every CI job starting mongod" should s"run on $MongoRunner, whose kernel mongod $productionVersion starts on" in {
    val RunsOn = """runs-on:\s*(\$\{\{[^}]*}}|\S+)""".r.unanchored
    val wrong = for {
      file        <- RepoFile.workflows()
      yml          = RepoFile.withoutComments(RepoFile.read(file.getPath))
      jobs         = RepoFile.jobs(yml)
      (name, job) <- jobs
      if MongoStarts.exists(job.contains)
      runner       = planned(jobs, job match { case RunsOn(r) => r.trim; case _ => "<none>" })
      ok           = if (runner.contains("inputs.runner")) RepoFile.block(yml, "runner").contains(s"default: $MongoRunner")
                     else RunnerLabel(runner, OnGitHub) == MongoRunner
      if !ok
    } yield s"${file.getName}: $name ($runner)"

    // Every caller's `runner:` for a leg, as github.com evaluates it (a dispatch naming none). One that passes a needed
    // job's output on is that job's `runner:` output, a line of its own here.
    val fallbacks = """(?m)^[ \t]+runner:[ \t]+(\$\{\{.*}}|\S+)[ \t]*$""".r
      .findAllMatchIn(RepoFile.workflows().map(f => RepoFile.withoutComments(RepoFile.read(f.getPath))).mkString("\n"))
      .map(_.group(1))
      .filterNot(_.contains("needs."))
      .map(RunnerLabel(_, OnGitHub)).toSeq

    withClue(s"mongod $productionVersion will not start on kernels 6.19-7.0.13 (ubuntu-24.04, ubuntu-26.04): ") {
      wrong shouldBe empty
      fallbacks should not be empty
      all(fallbacks) shouldBe MongoRunner
    }
  }
}
