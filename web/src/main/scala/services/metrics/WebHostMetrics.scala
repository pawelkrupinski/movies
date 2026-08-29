package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.util.Try

/**
 * `kinowo_web_host_memory_*` + `kinowo_web_host_disk_*` — how much RAM and disk
 * the web tier's MACHINE has left, as opposed to what its JVM is using.
 *
 * WHY THIS EXISTS: these were `fly_instance_memory_mem_available` and the
 * `fly_volume_*` family, out of Fly's managed Prometheus. Those tokens are
 * revoked and cannot be reissued, so the question "is the box about to run out"
 * became unanswerable for the web tier — the worker gets it from node_exporter
 * on the Hetzner fleet, but nothing scrapes the Fly machine's host. `/metrics`
 * is scraped directly over the WireGuard peer, so the honest way to get the
 * fact back is for the process to read its own kernel and publish it.
 *
 * It is a genuinely different signal from the heap and RSS panels beside it.
 * `-Xmx384m` inside a 1 GB machine leaves room for exactly one surprise, and
 * this app has already OOM-crash-looped once at a smaller VM size (see the
 * `[[vm]]` note in fly.toml, and the memory limit in
 * infra/kubernetes/web/base/all.yaml, which is the same bet on this platform) —
 * the tell was the free memory in whatever bounds the process, which
 * neither `jvm_memory_used_bytes` nor `process_resident_memory_bytes` can show,
 * because neither counts the page cache, the sidecar processes, or a second
 * machine's share of the same host.
 *
 * MEMORY IS READ FROM `/proc/meminfo`, NOT from `OperatingSystemMXBean`. The
 * bean reports MemFree, which on a healthy Linux box sits near zero — the
 * kernel spends everything it can on page cache — so a panel drawn from it
 * looks like a permanent emergency. `MemAvailable` is the kernel's own estimate
 * of what a new allocation could actually get, and is what Fly's metric
 * reported.
 *
 * ⚠️ BUT THE CGROUP WINS WHERE THERE IS ONE, and on k3s there always is. This
 * was written for a Firecracker microVM, where `/proc/meminfo` describes the
 * machine and `MemTotal` is its configured size. In a container it describes
 * the NODE: all three web pods would publish k3s-worker-1's ~16 GB and its
 * node-wide available memory, identical to each other and unrelated to the 1Gi
 * limit that is what actually OOM-kills them. That is not a smaller version of
 * the truth, it is a different fact wearing the same name — the panel would sit
 * at 90% free while a pod was being killed.
 *
 * So `memory.max` / `memory.current` (cgroup v2) are preferred when readable and
 * numeric, and `/proc/meminfo` is the fallback. `memory.max` reads the literal
 * string `max` for an unlimited cgroup, which is the Fly case and every
 * developer box that has the files at all — that spelling is treated as "no
 * limit", so the fallback still answers for a VM.
 *
 * DISK is the ROOT filesystem, not a volume: the web app mounts none (no
 * `[mounts]` in fly.toml), so the thing that can fill up is the machine's own
 * overlay — logs, the JVM's temp files, a runaway heap dump. Naming it `disk`
 * rather than `volume` keeps the panel honest if a volume is ever added.
 *
 * Both readings are taken at SCRAPE time through callback gauges rather than
 * sampled on a timer: they are two reads of a pseudo-file and a `statfs`, far
 * cheaper than the JVM collectors already running on the same scrape, and a
 * timer would only add a staleness window.
 *
 * When a reading is unavailable the gauge emits NO sample rather than a zero —
 * which is what happens on a developer's macOS box, where `/proc/meminfo` does
 * not exist. A zero would read as "the machine is out of memory", the single
 * most alarming value the panel can draw, and would be wrong.
 *
 * Registered on the SAME registry as the JVM collectors ([[WebJvmMetrics]]),
 * for the same reason [[WebHttpMetrics]] is: one registry, one `/metrics` body,
 * one thing to scrape. `country` is constant for the process and costs no
 * cardinality; it lets these series line up with every other `kinowo_*` series
 * on the shared dashboards.
 */
class WebHostMetrics(
  registry: PrometheusRegistry,
  country: String,
  meminfo: Path = Paths.get("/proc/meminfo"),
  diskRoot: File = new File("/"),
  cgroup: Path = Paths.get("/sys/fs/cgroup")
) {

  private def gauge(name: String, help: String, read: () => Option[Long]): Unit =
    GaugeWithCallback.builder()
      .name(name)
      .help(help)
      .unit(io.prometheus.metrics.model.snapshots.Unit.BYTES)
      .labelNames("country")
      .callback(callback => read().foreach(value => callback.call(value.toDouble, country)))
      .register(registry)

  private def meminfoField(key: String): Option[Long] =
    Try(Files.readString(meminfo)).toOption.flatMap(WebHostMetrics.meminfoBytes(_, key))

  /** One cgroup v2 pseudo-file as a byte count. `None` when the file is absent
   *  (not containerised, or cgroup v1) or holds `max`, the kernel's spelling for
   *  "no limit" — both mean the cgroup has nothing useful to say and the caller
   *  should fall back to the machine's own numbers. */
  private def cgroupBytes(file: String): Option[Long] =
    Try(Files.readString(cgroup.resolve(file))).toOption
      .map(_.trim)
      .filter(_.forall(_.isDigit))
      .flatMap(text => Try(text.toLong).toOption)

  /** What the process can still allocate before something kills it: the cgroup's
   *  headroom where a limit exists, the machine's MemAvailable otherwise. */
  private def memoryAvailable: Option[Long] =
    cgroupBytes("memory.max")
      .flatMap(limit => cgroupBytes("memory.current").map(used => math.max(0L, limit - used)))
      .orElse(meminfoField("MemAvailable"))

  /** `getTotalSpace` answers 0 for a path the process cannot stat, which would
   *  otherwise be published as a machine with no disk at all. */
  private def diskSpace(read: File => Long): Option[Long] =
    if (diskRoot.getTotalSpace > 0L) Some(read(diskRoot)) else None

  gauge(
    "kinowo_web_host_memory_available_bytes",
    "What the web process can still allocate before it is killed: the cgroup's memory.max minus " +
      "memory.current under k3s, MemAvailable from /proc/meminfo on an unlimited machine. The " +
      "replacement for Fly's fly_instance_memory_mem_available.",
    () => memoryAvailable
  )

  gauge(
    "kinowo_web_host_memory_total_bytes",
    "The ceiling the line above is a fraction of: the cgroup's memory.max under k3s (the pod's " +
      "limit, NOT the node's RAM), MemTotal from /proc/meminfo on an unlimited machine.",
    () => cgroupBytes("memory.max").orElse(meminfoField("MemTotal"))
  )

  gauge(
    "kinowo_web_host_disk_free_bytes",
    "Free space on the web machine's root filesystem. The app mounts no volume, so this is the " +
      "overlay that logs, temp files and any heap dump land on.",
    () => diskSpace(_.getUsableSpace)
  )

  gauge(
    "kinowo_web_host_disk_total_bytes",
    "Size of the web machine's root filesystem, so the free line above can be read as a fraction.",
    () => diskSpace(_.getTotalSpace)
  )
}

object WebHostMetrics {

  /** One `/proc/meminfo` field in BYTES, or `None` if the text does not carry
   *  it. Lines are `MemAvailable:   654321 kB` — a fixed name, whitespace, a
   *  decimal count and (for every memory field) the unit `kB`, which is
   *  kibibytes despite the spelling. The handful of unit-less fields
   *  (`HugePages_Total` and friends) are counts, not sizes, and are not
   *  something this reads; requiring the unit is what keeps a future caller
   *  from silently publishing a page count as a byte count. */
  private val Field = """(?m)^(\w+):\s+(\d+)\s+kB$""".r

  def meminfoBytes(meminfo: String, key: String): Option[Long] =
    Field.findAllMatchIn(meminfo).collectFirst { case m if m.group(1) == key => m.group(2).toLong * 1024L }
}
