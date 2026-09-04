package deploy

import java.nio.file.{Files, Paths}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * FLY.IO HOSTS EXACTLY ONE THING FROM THIS REPOSITORY, AND CI MAY DEPLOY ONLY
 * THAT: the Polish web app `kinowo`, which is what `kinowo.fly.dev` resolves to.
 *
 * This is a standing rule about the platform, not a snapshot of how far the
 * migration got. Everything else — both other countries' sites and all five
 * workers — runs on k3s and is shipped by the GHCR jobs; a Fly deploy of any of
 * them would put a SECOND copy of a live service on a host with none of the
 * traffic, against the same databases. That failure is silent: the leg goes
 * green, the duplicate serves, and the only tell is a stale page on a hostname
 * nobody checks. It was live for a day after the 2026-08-29 cutover before
 * anyone noticed.
 *
 * The rule used to be a `enabled: false` flag on each of six matrix rows, which
 * is exactly the kind of thing a "restore the UK for a moment" branch flips and
 * forgets. The rows, their `fly.*.toml` configs and the Fly apps themselves are
 * gone now, so restoring one means writing it from scratch — but the deploy job
 * is still one edit away from growing a matrix again, and these assertions are
 * what make that edit deliberate.
 *
 * Tests run with the repo root as CWD, so the workflow paths resolve directly.
 */
class FlyDeployScopeSpec extends AnyFlatSpec with Matchers {

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")
  private lazy val deployJob = RepoFile.block(mainYml, "deploy")

  "the Fly deploy job" should "deploy the Polish web app and nothing else" in {
    // On the COMMANDS, not the file: the comments above the job name the apps
    // that USED to be deployed while explaining why they are gone, and a spec
    // that forbids saying so would delete the explanation along with the
    // behaviour.
    val commands = deployJob.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    commands.linesIterator.filter(_.contains("flyctl deploy")).toSeq shouldBe
      Seq("              run: flyctl deploy -c fly.toml -a kinowo -i ghcr.io/${{ github.repository_owner }}/movies-web:${{ github.sha }}")
  }

  /**
   * And it must not grow a matrix again. A matrix is a place to add a row, and
   * every row anyone would add names an app that already runs on k3s — so the
   * one-app rule is enforced by there being nowhere to put a second one, not by
   * a flag on each row that a branch can flip.
   */
  it should "name its one app inline rather than iterating a matrix" in {
    deployJob should not include "strategy:"
    deployJob should not include "matrix."
  }

  /**
   * The job releases whatever `fly.toml` describes, and `fly.toml` is where
   * `KINOWO_RETIRED` lives — which is what makes deploying this host on every
   * push safe: it boots `modules.RetiredComponents`, a composition root with no
   * `Wiring` mixed in, so it cannot open a Mongo client. A job pointed at another
   * config would ship the SERVING app onto kinowo.fly.dev and undo the retirement
   * without touching a line of Scala.
   */
  it should "deploy from the config that carries the app's retirement flag" in {
    RepoFile.read("fly.toml") should include ("KINOWO_RETIRED = 'true'")
  }

  /**
   * ONE fly config, because there is one Fly app. The five that went with the
   * cutover — two country frontends and three workers — were kept for months as
   * "restoring one is a word plus a decision", which is how a config that
   * describes nothing running stays in a repo indefinitely, drifting from the
   * overlay that replaced it. `WorkerScrapeCadenceConfigSpec` used to read a
   * country's scrape cadence out of one of them.
   */
  it should "be the only fly config in the repository" in {
    RepoFile.flyTomls().map(_.getName) shouldBe Seq("fly.toml")
    withClue("a fly config outside the repo root would dodge the check above: ") {
      Files.exists(Paths.get("fly")) shouldBe false
    }
  }

  /**
   * The rule is about the PLATFORM, so it has to hold across every workflow, not
   * just the one that deploys. `deploy-grafana.yml` was the second — a
   * dispatch-only rollback for `kinowo-grafana`, kept after Grafana moved to
   * monitoring-1 on the grounds that a rollback you have to reconstruct is not
   * one. It went unrun long enough that its provisioning had drifted from the
   * copy that actually serves, at which point it was a rollback to a Grafana
   * nobody had seen.
   */
  "the workflows that can reach Fly" should "be only the one that deploys the Polish web app" in {
    val reaching = RepoFile.workflows()
      .filter(f => RepoFile.read(s".github/workflows/${f.getName}").contains("flyctl deploy"))
      .map(_.getName)
    reaching shouldBe Seq("main.yml")
  }

  /**
   * And no workflow may reach Fly for anything else either — a `flyctl scale`,
   * `flyctl machines start`, a `flyctl ssh console` against an app that is not
   * `kinowo`. The deploy guard's own probe is the one exception, and it names
   * `kinowo` on the same line.
   */
  it should "run no flyctl command against an app other than kinowo" in {
    val offenders = for {
      file <- RepoFile.workflows()
      line <- RepoFile.read(s".github/workflows/${file.getName}").linesIterator
      command = line.trim
      if !command.startsWith("#") && command.contains("flyctl ") && !command.contains("-a kinowo")
    } yield s"${file.getName}: $command"
    offenders shouldBe empty
  }
}
