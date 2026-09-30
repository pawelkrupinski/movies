package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * AT MOST THREE WORKERS BOOT AT ONCE. All five share k3s-worker-1, and a rollout used to restart
 * them together: the node sat at 7.9 of 8 cores and every boot — cache hydrate, projector prepare,
 * identity take-up — ran several times slower (2026-09-30). The stagger is three files agreeing:
 *
 *  - `flux/gotk-sync.yaml` gives the later workers' Kustomizations a `dependsOn` on the earlier
 *    ones, so Flux applies a wave only once the one before it is Ready;
 *  - `worker/base/all.yaml` makes a worker Ready only once `/ready` says its boot work settled —
 *    without that probe a pod is Ready seconds into its boot and the waves overlap entirely;
 *  - `WorkerMain.ReadinessCap` bounds how long `/ready` can hold a wave, which the Kustomizations'
 *    timeout must outlast, or a slow boot reads as a failed deploy.
 */
class WorkerRolloutStaggerSpec extends AnyFlatSpec with Matchers {

  private lazy val sync   = RepoFile.read("infra/kubernetes/flux/gotk-sync.yaml")
  private lazy val worker = RepoFile.read("infra/kubernetes/worker/base/all.yaml")

  private val MaxBootingAtOnce = 3

  private final case class Rollout(name: String, dependsOn: Set[String], timeoutMinutes: Int)

  /** Every `worker-<cc>-config` Kustomization: its name, the worker Kustomizations it waits on,
   *  and its timeout. Comments stripped, so a commented-out `dependsOn` counts as none. */
  private lazy val workers: Seq[Rollout] =
    sync.split("\n---").toSeq.map(_.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n"))
      .filter(_.contains("kind: Kustomization"))
      .flatMap { doc =>
        """(?m)^  name: (worker-\w+-config)$""".r.findFirstMatchIn(doc).map(_.group(1)).map { name =>
          val waits = """(?m)^  - name: (worker-\w+-config)$""".r.findAllMatchIn(doc).map(_.group(1)).toSet
          val timeout = """(?m)^  timeout: (\d+)m$""".r.findFirstMatchIn(doc).map(_.group(1).toInt).getOrElse(0)
          Rollout(name, waits, timeout)
        }
      }

  /** The workers in the order Flux can apply them: each wave waits only on earlier ones. */
  private lazy val waves: Seq[Set[String]] = {
    val byName = workers.map(w => w.name -> w.dependsOn).toMap
    Iterator.iterate((Seq.empty[Set[String]], byName.keySet)) { case (done, left) =>
      val applied = done.flatten.toSet
      (done :+ left.filter(byName(_).subsetOf(applied)), left.filterNot(byName(_).subsetOf(applied)))
    }.dropWhile { case (done, left) => left.nonEmpty && done.lastOption.forall(_.nonEmpty) }.next()._1.filter(_.nonEmpty)
  }

  "the worker rollout" should "cover all five workers" in {
    workers.map(_.name).toSet shouldBe Set("pl", "uk", "de", "us", "es").map(cc => s"worker-$cc-config")
  }

  it should s"boot at most $MaxBootingAtOnce workers at once, every worker in some wave" in {
    waves.flatten.toSet shouldBe workers.map(_.name).toSet   // no worker left waiting on a cycle
    waves.map(_.size).max should be <= MaxBootingAtOnce
  }

  it should "hold each worker unready until its boot work has settled, or its wave waits on nothing" in {
    val probe = RepoFile.block(worker, "readinessProbe")
    probe should include("path: /ready")
    probe should include("port: health")
  }

  it should "keep a booting worker scrapeable, or every staggered boot pages WorkerDown" in {
    worker should include regex """(?m)^  publishNotReadyAddresses: true$"""
  }

  it should "give a wave longer to become Ready than the worker's own cap on its boot" in {
    val cap = modules.WorkerMain.ReadinessCap.toMinutes
    all(workers.map(_.timeoutMinutes)) should be > cap.toInt
  }
}
