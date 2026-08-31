package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * FLY.IO HOSTS EXACTLY ONE THING FROM THIS REPOSITORY, AND CI MAY DEPLOY ONLY
 * THAT: the Polish web app `kinowo`, which is what `kinowo.fly.dev` resolves to.
 *
 * This is a standing rule about the platform, not a snapshot of how far the
 * migration got. Everything else — both other countries' sites and all three
 * workers — runs on k3s and is shipped by the GHCR jobs; a Fly deploy of any of
 * them would put a SECOND copy of a live service on a host with none of the
 * traffic, against the same databases. That failure is silent: the leg goes
 * green, the duplicate serves, and the only tell is a stale page on a hostname
 * nobody checks. It was live for a day after the 2026-08-29 cutover before
 * anyone noticed.
 *
 * The matrix flag is one word, which makes it exactly the kind of thing a
 * "restore the UK for a moment" branch flips and forgets. Asserting the roster
 * here means restoring an app takes a deliberate edit in two files — the flip,
 * and the sentence in this spec saying why it is allowed.
 *
 * Tests run with the repo root as CWD, so the workflow paths resolve directly.
 */
class FlyDeployScopeSpec extends AnyFlatSpec with Matchers {

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  /** Every `- app: … enabled: …` row of the deploy matrix, in file order. */
  private lazy val legs: Seq[(String, Boolean)] = {
    val lines = RepoFile.block(mainYml, "deploy").linesIterator.toVector
    lines.zipWithIndex.collect { case (line, at) if line.trim.startsWith("- app:") =>
      val app = line.trim.stripPrefix("- app:").trim
      // The flag is the last key of the row: scan forward to the next `enabled:`
      // before the following row starts.
      val enabled = lines.drop(at + 1).takeWhile(!_.trim.startsWith("- app:"))
        .find(_.trim.startsWith("enabled:")).map(_.trim.stripPrefix("enabled:").trim)
      withClue(s"leg '$app' has no `enabled:` flag: ")(enabled shouldBe defined)
      app -> (enabled.get == "true")
    }
  }

  "the Fly deploy matrix" should "enable the Polish web app and nothing else" in {
    legs.filter(_._2).map(_._1) shouldBe Seq("kinowo")
  }

  // Named individually rather than counted, so adding a seventh app to the
  // matrix fails here instead of passing on an arithmetic coincidence.
  it should "still carry every other app as a disabled row, ready to restore" in {
    legs.map(_._1) should contain theSameElementsAs Seq(
      "kinowo", "showtimes-uk", "showtimes-de", "kinowo-worker", "kinowo-worker-uk", "kinowo-worker-de")
  }

  // The leg releases whatever `fly.toml` describes, and `fly.toml` is where
  // `KINOWO_RETIRED` lives — which is what makes deploying this host on every
  // push safe. A leg pointed at another config would ship the SERVING app onto
  // kinowo.fly.dev and undo the retirement without touching a line of Scala.
  it should "deploy the Polish app from the config that carries its retirement flag" in {
    val row = RepoFile.block(mainYml, "deploy").linesIterator.toVector
      .dropWhile(!_.trim.startsWith("- app: kinowo")).take(5).map(_.trim)
    row should contain ("toml: fly.toml")
    row should contain ("bin: web")
    RepoFile.read("fly.toml") should include ("KINOWO_RETIRED = 'true'")
  }

  // The rule is about the PLATFORM, so it has to hold across every workflow, not
  // just the one with the matrix in it. Two files may reach Fly at all; a third
  // that learns to has to be a deliberate edit here.
  "the workflows that can reach Fly" should "be only the deploy matrix and the Grafana rollback" in {
    val reaching = RepoFile.workflows()
      .filter(f => RepoFile.read(s".github/workflows/${f.getName}").contains("flyctl deploy"))
      .map(_.getName)
    reaching should contain theSameElementsAs Seq("main.yml", "deploy-grafana.yml")
  }

  // `kinowo-grafana` is the migration's rollback, and its machine is STOPPED — a
  // `flyctl deploy` starts it. On a `push` trigger that made every alert-rule
  // tweak boot a second Grafana holding a second copy of the same alerts, which
  // is both a duplicate service and the exact thing the one-app rule forbids.
  // Dispatch-only keeps the rollback without letting a commit fire it.
  it should "leave the Grafana rollback on a manual trigger, never a push" in {
    val grafana = RepoFile.read(".github/workflows/deploy-grafana.yml")
    grafana should include ("workflow_dispatch:")
    withClue("deploy-grafana.yml would redeploy the stopped Fly Grafana on a push: ") {
      grafana.linesIterator.exists(_.trim == "push:") shouldBe false
    }
  }
}
