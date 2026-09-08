package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The web tier is the only thing users see, and this locks the handful of fields
 * that decide whether a deploy is visible to them.
 *
 * IT WAS VISIBLE. Measured 2026-09-08 against `showtimes.cc/de/`, polling every
 * ~100ms across one `web-de` rollout: three 502s in a 330ms window, on a tier
 * that had `maxUnavailable: 0` and looked correct.
 *
 * `maxUnavailable: 0` is necessary and was never sufficient, because it only
 * governs when the new pod is CREATED. Retiring the old one starts two things at
 * once and orders neither: the kubelet sends SIGTERM, and — independently — the
 * endpoints controller drops the pod from the Service, which becomes real only
 * once kube-proxy has rewritten the node's iptables. Play begins unbinding on the
 * first while the second is still in flight, so the NodePort keeps forwarding to a
 * socket that has stopped accepting. Caddy fronts these NodePorts with a bare
 * `reverse_proxy` and no retry, so every one of those is a 502 with a person on the
 * other end.
 *
 * The fields below are what closes that, and they are worth naming because each is
 * invisible in `kubectl get` and silently absent by default:
 *
 *   preStop           holds the container open while endpoint removal propagates.
 *                     SIGTERM is not sent until the hook returns, and the app goes
 *                     on serving throughout — that delay IS the mechanism.
 *   minReadySeconds   makes the new pod prove itself before its predecessor dies.
 *                     Without it a JVM that answers /health once and then falls
 *                     over takes the country with it, since nothing else serves it.
 *   PodDisruptionBudget
 *                     the drain path, which the Deployment's strategy never sees.
 *
 * The worker tier deliberately gets NONE of this, and the last test here says so:
 * nothing routes to a worker, it is `Recreate` because two of them would
 * double-write one corpus, and a preStop sleep would buy nothing but a slower
 * restart on every one of the ~15 deploys a day this fleet does.
 */
class WebRolloutAvailabilitySpec extends AnyFlatSpec with Matchers {

  private val WebBase    = RepoFile.read("infra/kubernetes/web/base/all.yaml")
  private val WorkerBase = RepoFile.read("infra/kubernetes/worker/base/all.yaml")

  /** A bare `key: 123` at any indentation, ignoring commented-out copies. */
  private def intField(yaml: String, key: String): Option[Int] =
    yaml.linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"$k: $v" if k == key && v.forall(_.isDigit) && v.nonEmpty => v.toInt }

  /** The seconds a `preStop` `exec` sleeps for, written as a flow sequence. */
  private def preStopSleepSeconds(yaml: String): Option[Int] =
    """command:\s*\[\s*"sleep"\s*,\s*"?(\d+)"?\s*\]""".r.findFirstMatchIn(yaml).map(_.group(1).toInt)

  "the web tier" should "hold the container open while its endpoint removal propagates" in {
    val sleep = preStopSleepSeconds(WebBase)
    withClue(
      "web/base/all.yaml declares no `preStop` sleep. Without one the pod stops accepting while " +
      "kube-proxy is still forwarding to it, which is the 502s measured on 2026-09-08: ") {
      sleep should not be empty
    }
    withClue("a zero-length preStop is the same as having none: ")(sleep.get should be > 0)
  }

  /** Play's own shutdown — unbind, drain in-flight requests, close the Mongo driver — has to
   *  happen INSIDE the grace period too, and it only starts once the hook returns. Ten seconds
   *  is the floor: below that the kubelet SIGKILLs a JVM mid-drain, which turns a deploy from
   *  "invisible" into "cut every in-flight response", the exact opposite of the point. */
  private val MinShutdownHeadroomSeconds = 10

  it should "leave the app time to shut down after the hook returns" in {
    val sleep = preStopSleepSeconds(WebBase).getOrElse(fail("no preStop sleep"))
    val grace = intField(WebBase, "terminationGracePeriodSeconds")
      .getOrElse(fail("web/base declares no terminationGracePeriodSeconds"))
    withClue(
      s"preStop sleeps ${sleep}s of a ${grace}s grace period, leaving ${grace - sleep}s for Play to " +
      s"unbind and drain before the kubelet SIGKILLs it. Raise the grace period in the same commit " +
      s"as the hook, or in-flight requests are cut instead of finished: ") {
      (sleep + MinShutdownHeadroomSeconds) should be <= grace
    }
    info(s"preStop ${sleep}s + ${grace - sleep}s to drain = ${grace}s grace")
  }

  it should "make a new pod prove itself before the old one is retired" in {
    val minReady = intField(WebBase, "minReadySeconds")
    withClue(
      "web/base/all.yaml sets no `minReadySeconds`, so `maxUnavailable: 0` retires the old pod the " +
      "instant the new one passes readiness once — and at one replica there is nothing else " +
      "serving that country if it then falls over: ") {
      minReady should not be empty
    }
    withClue("`minReadySeconds: 0` is the default and buys nothing: ")(minReady.get should be > 0)
  }

  it should "still refuse to retire a pod before its replacement exists" in {
    // The field the two above are useless without, locked so a future edit cannot quietly
    // reintroduce the outage they were added to close.
    val rolling = RepoFile.block(WebBase, "rollingUpdate")
    intField(rolling, "maxUnavailable") shouldBe Some(0)
    intField(rolling, "maxSurge")       shouldBe Some(1)
  }

  it should "declare what a node drain may do to a country" in {
    withClue(
      "web/base/all.yaml declares no PodDisruptionBudget. A rollout is governed by the strategy " +
      "above, but `kubectl drain`, a cordon for a host move and kubelet pressure eviction all " +
      "delete the pod directly and weigh it against nothing: ") {
      WebBase should include ("kind: PodDisruptionBudget")
    }
    // `maxUnavailable`, NOT `minAvailable`. These Deployments run one replica on purpose, so
    // `minAvailable: 1` could never be satisfied and would block every drain forever — trading a
    // brief outage for a permanently stuck host move.
    val pdb = WebBase.split("kind: PodDisruptionBudget").last
    withClue("a single-replica PDB written as `minAvailable` blocks drains rather than shaping them: ") {
      pdb should not include ("minAvailable")
    }
    intField(pdb, "maxUnavailable") shouldBe Some(1)
  }

  // THE CONTRAST, asserted so a sweep that adds these to "all the deployments" has to argue with
  // a test first. Nothing routes to a worker: it has no Service, no readiness probe and no
  // NodePort, so there is no endpoint to drain and a preStop sleep would only make every restart
  // 15s slower. `Recreate` is likewise deliberate — two workers would double-write one corpus.
  "the worker tier" should "not carry the web tier's draining machinery" in {
    withClue("the worker has no endpoints to drain — a preStop sleep here is pure restart latency: ") {
      preStopSleepSeconds(WorkerBase) shouldBe empty
    }
    withClue("`minReadySeconds` on a tier nothing routes to only delays the next scrape: ") {
      intField(WorkerBase, "minReadySeconds") shouldBe empty
    }
    withClue("two workers would double-write one corpus, so this must stay Recreate: ") {
      RepoFile.block(WorkerBase, "strategy") should include ("type: Recreate")
    }
  }
}
