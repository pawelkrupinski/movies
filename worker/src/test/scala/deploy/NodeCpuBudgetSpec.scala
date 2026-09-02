package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the one resource the whole fleet shares and nothing else guards: the CPU
 * a single k3s node can have SPOKEN FOR at once.
 *
 * Every kinowo pod runs on `k3s-worker-1`, and it is the only schedulable node
 * there is — `monitoring-1` carries the `node-role.kubernetes.io/control-plane`
 * taint, so the scheduler never considers it. That node reports
 * `allocatable.cpu: 8` (`kubectl get node k3s-worker-1
 * -o jsonpath='{.status.allocatable.cpu}'`), and the scheduler admits a pod by
 * comparing the SUM OF REQUESTS already placed on a node against that number. It
 * never looks at utilisation.
 *
 * That distinction is the whole reason this spec exists. On 2026-09-02 the sum of
 * requests reached 7760m of 8000m — 97% — while the box itself was running at a
 * 16% median and a 29% p95. Nothing was slow and no alert fired, but only 240m
 * was unspoken for, and a web pod asks for more than that. The symptom was not a
 * pod that would not start: it was a ROLLOUT THAT COULD NOT FINISH. The web
 * Deployments run `maxSurge: 1, maxUnavailable: 0`, so a rollout must place the
 * new pod BEFORE retiring the old one; with no room the new pod sat `Pending`
 * with `FailedScheduling: Insufficient cpu` and the old one kept serving
 * indefinitely. Spain served the `:latest` placeholder image that way until the
 * old pod was deleted by hand.
 *
 * So the invariant is not "the requests fit". It is "the requests fit WITH THE
 * LARGEST SURGE POD ON TOP", which is what a deploy actually needs.
 *
 * The numbers themselves are measured rather than chosen — `max_over_time` of
 * each pod's own `process_cpu_seconds_total` over 24h — and the reasoning for
 * each sits beside it in its overlay.
 */
class NodeCpuBudgetSpec extends AnyFlatSpec with Matchers {

  /** `k3s-worker-1`'s `allocatable.cpu`, in millicores. */
  private val NodeAllocatableMillis = 8000

  /** Left for everything on the node that is NOT a kinowo pod: kube-system's
   *  coredns / metrics-server / local-path-provisioner, and the kubelet's own
   *  slack. Measured at ~10m in practice (most of kube-system runs on the
   *  control-plane node), so this is deliberately an order of magnitude more
   *  than observed — the point is a floor under the arithmetic, not a tight fit. */
  private val SystemReserveMillis = 250

  private val Tiers     = Seq("worker", "web")
  private val Countries = Seq("pl", "de", "uk", "us", "es")

  /** The `requests.cpu` of one tier+country overlay, in millicores.
   *
   *  Reads the FIRST `cpu:` under `requests:`, which is the shape every overlay
   *  has. Deliberately NOT tolerant of a missing value: a country that inherits
   *  the base's request silently would be invisible to the sum below, which is
   *  the one thing this spec must never be. */
  private def requestedMillis(tier: String, cc: String): Int = {
    val path  = s"infra/kubernetes/$tier/overlays/$cc/patch.yaml"
    val lines = RepoFile.read(path).linesIterator.map(_.trim).toList
    val underRequests = lines.dropWhile(_ != "requests:").drop(1)
    val cpu = underRequests.takeWhile(_ != "limits:").collectFirst {
      case l if l.startsWith("cpu:") => l.stripPrefix("cpu:").trim.replace("\"", "").replace("'", "")
    }
    withClue(s"$path declares no cpu request under `requests:`: ")(cpu should not be empty)
    parseMillis(cpu.get)
  }

  /** Kubernetes CPU quantities: `"500m"` is millicores, a bare `"1"` is cores. */
  private def parseMillis(raw: String): Int =
    if (raw.endsWith("m")) raw.dropRight(1).toInt else (raw.toDouble * 1000).toInt

  private lazy val requests: Seq[((String, String), Int)] =
    for (tier <- Tiers; cc <- Countries) yield (tier, cc) -> requestedMillis(tier, cc)

  "every deployed tier and country" should "declare its own CPU request rather than inheriting one" in {
    // `requestedMillis` fails on a missing value; this is the enumeration itself,
    // so a country added to the fleet without an overlay is caught here.
    requests.map(_._1) should contain theSameElementsAs
      (for (tier <- Tiers; cc <- Countries) yield (tier, cc))
    all(requests.map(_._2)) should be > 0
  }

  "the node's CPU request budget" should "leave room for the extra pod a rolling update surges" in {
    val total   = requests.map(_._2).sum
    val surge   = requests.filter(_._1._1 == "web").map(_._2).max   // maxSurge: 1 on the web tier
    val ceiling = NodeAllocatableMillis - SystemReserveMillis

    withClue(
      s"kinowo requests ${total}m + a ${surge}m surge pod = ${total + surge}m against a ${ceiling}m " +
      s"ceiling (${NodeAllocatableMillis}m allocatable - ${SystemReserveMillis}m for system pods). " +
      "Over it, a web rollout hangs Pending forever with maxUnavailable: 0 rather than failing. " +
      "Either right-size the requests against measured peaks or add a node: ") {
      (total + surge) should be <= ceiling
    }

    info(f"requests ${total}m + surge ${surge}m = ${total + surge}m of ${ceiling}m " +
         f"(${100.0 * (total + surge) / ceiling}%.1f%%, ${ceiling - total - surge}m spare)")
  }

  it should "not have drifted back to a uniform round number per tier" in {
    // The failure mode this replaced was not a wrong number, it was the SAME
    // number everywhere: every worker asked for exactly one core when their real
    // peaks span 0.29 to 1.24, and every web for 500m against peaks of 0.19 to
    // 0.59. A sweep that re-rounds them all would rebuild the 97% ledger without
    // changing a single measured fact, so require that the fleet still reflects
    // that spread rather than a default.
    val workers = requests.filter(_._1._1 == "worker").map(_._2).distinct
    val webs    = requests.filter(_._1._1 == "web").map(_._2).distinct
    withClue("every worker asks for the same CPU again — that is the uniform guess, not a measurement: ")(
      workers.size should be > 1)
    withClue("every web asks for the same CPU again — see above: ")(
      webs.size should be > 1)
  }
}
