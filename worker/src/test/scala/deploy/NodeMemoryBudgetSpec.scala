package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The memory half of the ledger [[NodeCpuBudgetSpec]] keeps for CPU, and the one
 * that has actually cost an outage.
 *
 * `web-us` OOM-crash-looped through 2026-08-30..09-02 — 40 restarts in three
 * days, each a 502 window on the public site, because
 * `pekko.jvm-exit-on-fatal-error` makes the JVM exit rather than limp. The
 * obvious remedy is to give it a bigger heap and a bigger limit, and on this node
 * that remedy is a TRAP: `k3s-worker-1` is the only schedulable node
 * (`monitoring-1` carries the control-plane taint), its allocatable memory is
 * 15616Mi, and the kinowo pods already ask for 12800Mi of it. The scheduler
 * admits on REQUESTS, never on usage, and the web tier deploys `maxSurge: 1,
 * maxUnavailable: 0` — so a rollout must place the new pod BEFORE retiring the
 * old one. Raise web-us from 2Gi to 3Gi and the sum still "fits", but the surge
 * pod no longer does: the rollout hangs `Pending` with `Insufficient memory` and
 * the old pod serves forever. That is exactly how Spain served the `:latest`
 * placeholder image off the CPU side of this same ledger.
 *
 * So the invariant is the same one, and for the same reason: the requests must
 * fit WITH THE LARGEST SURGE POD ON TOP. If a country genuinely needs more heap,
 * this spec is the thing that says the memory has to come from somewhere —
 * another pod's over-ask, or another node.
 */
class NodeMemoryBudgetSpec extends AnyFlatSpec with Matchers {

  /** `k3s-worker-1`'s `allocatable.memory`, in mebibytes:
   *  `kubectl get node k3s-worker-1 -o jsonpath='{.status.allocatable.memory}'`
   *  reports 15990952Ki. */
  private val NodeAllocatableMib = 15616

  /** Left for everything on the node that is NOT a kinowo pod. The node's own
   *  ledger shows 12864Mi requested against the 12800Mi these overlays declare,
   *  so kube-system's share is ~64Mi (most of it runs on the control-plane node).
   *  This is deliberately several times that — a floor under the arithmetic, not
   *  a tight fit, and the kubelet needs slack it does not request. */
  private val SystemReserveMib = 512

  private val Tiers     = Seq("worker", "web")
  private val Countries = Seq("pl", "de", "uk", "us", "es")

  /** The `requests.memory` of one tier+country overlay, in mebibytes.
   *
   *  Deliberately NOT tolerant of a missing value, for the same reason the CPU
   *  spec is not: a country silently inheriting the base's request would be
   *  invisible to the sum below, which is the one thing this must never be. */
  private def requestedMib(tier: String, cc: String): Int = {
    val path  = s"infra/kubernetes/$tier/overlays/$cc/patch.yaml"
    val lines = RepoFile.read(path).linesIterator.map(_.trim).toList
    val underRequests = lines.dropWhile(_ != "requests:").drop(1)
    val memory = underRequests.takeWhile(_ != "limits:").collectFirst {
      case l if l.startsWith("memory:") => l.stripPrefix("memory:").trim.replace("\"", "").replace("'", "")
    }
    withClue(s"$path declares no memory request under `requests:`: ")(memory should not be empty)
    parseMib(memory.get)
  }

  /** Kubernetes memory quantities, as the overlays actually spell them: `1Gi`,
   *  `1536Mi`. Binary suffixes only — a decimal `1G` would be a different number
   *  and is not a spelling this repo uses, so reject it rather than round it. */
  private def parseMib(raw: String): Int = raw match {
    case s"${n}Gi" => n.toInt * 1024
    case s"${n}Mi" => n.toInt
    case other     => fail(s"unrecognised memory quantity `$other` — expected a Gi or Mi suffix")
  }

  private lazy val requests: Seq[((String, String), Int)] =
    for (tier <- Tiers; cc <- Countries) yield (tier, cc) -> requestedMib(tier, cc)

  "every deployed tier and country" should "declare its own memory request rather than inheriting one" in {
    requests.map(_._1) should contain theSameElementsAs
      (for (tier <- Tiers; cc <- Countries) yield (tier, cc))
    all(requests.map(_._2)) should be > 0
  }

  "the node's memory request budget" should "leave room for the extra pod a rolling update surges" in {
    val total   = requests.map(_._2).sum
    val surge   = requests.filter(_._1._1 == "web").map(_._2).max   // maxSurge: 1 on the web tier
    val ceiling = NodeAllocatableMib - SystemReserveMib

    withClue(
      s"kinowo requests ${total}Mi + a ${surge}Mi surge pod = ${total + surge}Mi against a ${ceiling}Mi " +
      s"ceiling (${NodeAllocatableMib}Mi allocatable - ${SystemReserveMib}Mi for system pods). " +
      "Over it, a web rollout hangs Pending forever with maxUnavailable: 0 rather than failing. " +
      "Giving a country a bigger heap means taking the memory from another pod or adding a node: ") {
      (total + surge) should be <= ceiling
    }

    info(f"requests ${total}Mi + surge ${surge}Mi = ${total + surge}Mi of ${ceiling}Mi " +
         f"(${100.0 * (total + surge) / ceiling}%.1f%%, ${ceiling - total - surge}Mi spare)")
  }

  /** The JAVA_OPTS a tier+country actually boots with: its overlay's where it
   *  patches one, the base's otherwise. A ConfigMap patch REPLACES the whole key,
   *  so an overlay that names JAVA_OPTS at all is the whole story for that pod —
   *  which is why web-us has to restate the base's non-heap flags verbatim beside
   *  its doubled heap. */
  private def javaOpts(tier: String, cc: String): String = {
    val overlay = RepoFile.read(s"infra/kubernetes/$tier/overlays/$cc/patch.yaml")
    if (overlay.contains("-Xmx")) overlay else RepoFile.read(s"infra/kubernetes/$tier/base/all.yaml")
  }

  /** One `-XX:Name=<size>` / `-Xmx<size>` flag as mebibytes. */
  private def flagMib(opts: String, flag: String, tier: String, cc: String): Int = {
    val matched = s"$flag(\\d+)([mgMG])".r.findFirstMatchIn(opts)
      .getOrElse(fail(s"$tier/$cc's JAVA_OPTS declares no $flag"))
    val size = matched.group(1).toInt
    if (matched.group(2).equalsIgnoreCase("g")) size * 1024 else size
  }

  private def limitMib(tier: String, cc: String): Int = {
    val lines = RepoFile.read(s"infra/kubernetes/$tier/overlays/$cc/patch.yaml").linesIterator.map(_.trim).toList
    val memory = lines.dropWhile(_ != "limits:").drop(1).collectFirst {
      case l if l.startsWith("memory:") => l.stripPrefix("memory:").trim.replace("\"", "").replace("'", "")
    }
    withClue(s"$tier/overlays/$cc declares no memory limit: ")(memory should not be empty)
    parseMib(memory.get)
  }

  /** Thread stacks, GC bookkeeping and native malloc — everything inside the
   *  cgroup that no JVM flag caps. Not a model of the JVM: it is the SMALLEST
   *  margin any deployment currently runs with (worker/pl leaves 288Mi over its
   *  448m heap and 288m of declared non-heap ceilings, and is stable), rounded
   *  down. A change that dips under it is asking a pod to run on less native
   *  headroom than anything here has been observed to need. */
  private val UntrackedNativeMib = 256

  // The heap and the container limit are set in two different documents and
  // nothing has ever checked that they agree. They have to: the JVM sizes itself
  // by -Xmx and the kernel kills by the cgroup limit, so a heap raised without the
  // limit does not OOM with a Java stack trace an alert can read — the container
  // is SIGKILLed, and all Sentry sees is that the pod stopped. `-Xms` equals
  // `-Xmx` everywhere here, so the heap is COMMITTED at boot rather than grown
  // into: the pod either fits from the first second or it does not.
  "every deployment's heap" should "fit inside its own container limit with the non-heap ceilings on top" in {
    Tiers.foreach { tier =>
      Countries.foreach { cc =>
        val opts     = javaOpts(tier, cc)
        val heap     = flagMib(opts, "-Xmx", tier, cc)
        val nonHeap  = Seq("-XX:MaxMetaspaceSize=", "-XX:ReservedCodeCacheSize=", "-XX:MaxDirectMemorySize=")
          .map(flagMib(opts, _, tier, cc)).sum
        val needed   = heap + nonHeap + UntrackedNativeMib
        val limit    = limitMib(tier, cc)

        withClue(
          s"$tier/$cc: -Xmx${heap}Mi + ${nonHeap}Mi declared non-heap + ${UntrackedNativeMib}Mi " +
          s"untracked native = ${needed}Mi against a ${limit}Mi limit. Raise the limit in the same " +
          "commit as the heap, or the kernel kills the container instead of the JVM reporting an " +
          "OutOfMemoryError: ") {
          needed should be <= limit
        }
      }
    }
  }

  /** Floors bought by a measured heap exhaustion, per tier+country. See the test
   *  below for what each one cost. */
  private val HeapFloorsMib = Map(("worker", "us") -> 1280, ("web", "us") -> 1024)

  // The MIRROR of the test above, and the failure it missed. That one stops a heap
  // outgrowing its container; this one stops a heap the container has already paid
  // for going unclaimed. A limit is not headroom a JVM can reach; only -Xmx is, so a
  // pod can die of OutOfMemoryError with hundreds of MB of its cgroup untouched --
  // which means no memory-pressure alert fires and a bigger limit fixes nothing.
  //
  // Both floors here were bought that way, and BOTH pods had slack in the limit at
  // the time:
  //   worker-us  died 2026-09-03T03:40:17 on -Xmx1024m; cgroup peaked at 1.3G of 2Gi.
  //   web-us     G1 Old Gen peaked at 760.5MiB of a 768MiB cap (99% of the heap LIVE)
  //              while RSS peaked at 1515MiB of 2048MiB -- the shape behind its
  //              ~40-restart crash loop of 2026-08-30..09-02.
  // US carries ~5000 venues against Germany's ~1500, and one US "city" (California)
  // renders an 18.9MB page, which is why it is the country that runs out first.
  "the US deployments' heaps" should "claim the container limit they were already given" in {
    HeapFloorsMib.foreach { case ((tier, cc), floor) =>
      val heap  = flagMib(javaOpts(tier, cc), "-Xmx", tier, cc)
      val limit = limitMib(tier, cc)
      withClue(
        s"$tier/$cc boots with -Xmx${heap}Mi against a ${limit}Mi limit, under a ${floor}Mi floor " +
        "that a measured heap exhaustion bought. Lowering it re-opens that crash; raising it " +
        "further is fine only while the test above still passes: ") {
        heap should be >= floor
      }
      info(s"$tier/$cc: -Xmx${heap}Mi of a ${limit}Mi limit")
    }
  }

  /** The heap-dump volume's ceiling, in mebibytes, and how many dumps survive a boot. */
  private def dumpVolumeMib(cc: String): Int = {
    val overlay = RepoFile.read(s"infra/kubernetes/web/overlays/$cc/patch.yaml")
    val source  = if (overlay.contains("sizeLimit:")) overlay else RepoFile.read("infra/kubernetes/web/base/all.yaml")
    val raw = source.linesIterator.map(_.trim).collectFirst {
      case l if l.startsWith("sizeLimit:") => l.stripPrefix("sizeLimit:").trim.replace("\"", "")
    }
    withClue(s"web/$cc has no heap-dump volume sizeLimit: ")(raw should not be empty)
    parseMib(raw.get)
  }

  private def dumpsKept(cc: String): Int = {
    val overlay = RepoFile.read(s"infra/kubernetes/web/overlays/$cc/patch.yaml")
    val source  = if (overlay.contains("HEAPDUMP_KEEP")) overlay else RepoFile.read("infra/kubernetes/web/base/all.yaml")
    val raw = source.linesIterator.map(_.trim).collectFirst {
      case l if l.startsWith("HEAPDUMP_KEEP:") => l.stripPrefix("HEAPDUMP_KEEP:").trim.replace("\"", "")
    }
    withClue(s"web/$cc does not say how many heap dumps to keep: ")(raw should not be empty)
    raw.get.toInt
  }

  // AN emptyDir OVER ITS sizeLimit GETS THE POD EVICTED, so the volume that exists to explain an
  // OOM must not be able to cause an outage of its own -- on the only user-facing tier, that trade
  // is strictly worse than having no dump.
  //
  // A dump cannot exceed the heap that produced it, so `(kept + 1) * -Xmx` is the honest worst
  // case: the dump being written on the way down, beside the one the previous boot kept. Bounding
  // on -Xmx rather than on the measured live set is deliberate -- the live set is what a dump
  // ACTUALLY weighs (~600MiB for web-us's 1024m heap), but it moves with the corpus and this
  // number must hold without anyone re-measuring it.
  //
  // This is the guard for the whole class: raise a heap, raise HEAPDUMP_KEEP, or shrink a volume,
  // and whichever of the three you forget fails here rather than in an eviction.
  "the heap-dump volume" should "hold every dump it is configured to keep, on every web country" in {
    Countries.foreach { cc =>
      val heap    = flagMib(javaOpts("web", cc), "-Xmx", "web", cc)
      val kept    = dumpsKept(cc)
      val ceiling = dumpVolumeMib(cc)
      val worst   = (kept + 1) * heap
      withClue(
        s"web/$cc keeps $kept dump(s) of a ${heap}Mi heap, so the volume can hold ${worst}Mi at " +
        s"once, against a ${ceiling}Mi sizeLimit. Over that limit the kubelet EVICTS the pod: ") {
        ceiling should be >= worst
      }
      info(s"web/$cc: keep=$kept, worst ${worst}Mi <= ${ceiling}Mi sizeLimit")
    }
  }
}
