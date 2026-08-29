package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Files, Path, Paths}

/**
 * Locks the machine-level facts the web tier publishes on its own `/metrics`
 * (`kinowo_web_host_memory_*`, `kinowo_web_host_disk_*`) — the replacement for
 * Fly's `fly_instance_memory_mem_available` and `fly_volume_*`, whose
 * managed-Prometheus token is revoked.
 *
 * Two assertions are load-bearing rather than decorative:
 *
 *  - `MemAvailable`, not `MemFree`. They sit in the same file two lines apart
 *    and differ by the whole page cache: on a healthy 1 GB Fly machine free is
 *    near zero while available is most of the box. Reading the wrong one draws
 *    a permanent false emergency, and no other layer would catch it — so the
 *    fixture carries both, deliberately far apart, and the test names which one
 *    must come out.
 *  - An unreadable source publishes NO sample, not a zero. Zero is the most
 *    alarming value either panel can draw, and it is what a naive
 *    `getOrElse(0)` would report on every scrape from a machine whose procfs or
 *    mount point isn't there.
 *
 * The fixture keeps `/proc/meminfo`'s real shape — fixed-width padding, the `kB`
 * suffix that means kibibytes, and the unit-less page counts mixed in — because
 * that format is a kernel interface and a tidied-up sample is exactly where a
 * units bug hides.
 */
class WebHostMetricsSpec extends AnyFlatSpec with Matchers {

  private val Meminfo =
    """MemTotal:        1009836 kB
      |MemFree:           73048 kB
      |MemAvailable:     651932 kB
      |Buffers:           12704 kB
      |Cached:           501516 kB
      |HugePages_Total:       0
      |Hugepagesize:       2048 kB
      |""".stripMargin

  private val Available = "kinowo_web_host_memory_available_bytes"
  private val TotalMem  = "kinowo_web_host_memory_total_bytes"
  private val DiskFree  = "kinowo_web_host_disk_free_bytes"
  private val DiskTotal = "kinowo_web_host_disk_total_bytes"

  /** A registry carrying only these gauges, rendered as exposition text.
   *  `diskRoot` defaults to a real directory so `getUsableSpace` runs the
   *  production `statfs` path rather than a stub. */
  private def render(meminfo: Path, diskRoot: File = new File("."),
                     cgroup: Path = Paths.get("/nonexistent-cgroup")): String = {
    val registry = new PrometheusRegistry()
    new WebHostMetrics(registry, country = "pl", meminfo = meminfo, diskRoot = diskRoot, cgroup = cgroup)
    PrometheusExposition.render(registry)
  }

  /** A cgroup v2 directory holding the two files the reader consults. `null`
   *  for either writes no file at all, which is the not-containerised shape. */
  private def cgroupDir(max: String, current: String): Path = {
    val dir = Files.createTempDirectory("cgroup")
    if (max != null) Files.writeString(dir.resolve("memory.max"), max + "\n")
    if (current != null) Files.writeString(dir.resolve("memory.current"), current + "\n")
    dir
  }

  // ── The cgroup, where there is one ───────────────────────────────────────────
  // On k3s `/proc/meminfo` describes the NODE, so all three web pods would
  // publish k3s-worker-1's ~16 GB and its node-wide free memory — identical to
  // each other and unrelated to the 1Gi limit that actually kills them. The
  // panel would read 90% free while a pod was being OOM-killed, which is worse
  // than no panel. These four lock the precedence that fixes it.

  "the memory gauges" should "report the CGROUP's headroom and limit when one is set, not the node's" in {
    // 1Gi limit, 700MiB used -> 348160 KiB of headroom, and a total that is the
    // POD's limit rather than the node's 1009836 kB of MemTotal in the fixture.
    val out = render(meminfoAt(Meminfo), cgroup = cgroupDir("1073741824", "734003200"))
    sample(out, Available) shouldBe Some(1073741824d - 734003200d)
    sample(out, TotalMem) shouldBe Some(1073741824d)
    // Nothing from /proc/meminfo leaked through.
    sample(out, Available) should not be Some(651932d * 1024d)
    sample(out, TotalMem) should not be Some(1009836d * 1024d)
  }

  it should "fall back to /proc/meminfo when the cgroup is unlimited (`max`)" in {
    // The Fly/VM shape, and any box where the files exist but bound nothing.
    // `max` is a kernel spelling, not a number, and parsing it as one would be
    // either an exception or a nonsense ceiling.
    val out = render(meminfoAt(Meminfo), cgroup = cgroupDir("max", "734003200"))
    sample(out, Available) shouldBe Some(651932d * 1024d)
    sample(out, TotalMem) shouldBe Some(1009836d * 1024d)
  }

  it should "fall back to /proc/meminfo when there is no cgroup at all" in {
    val out = render(meminfoAt(Meminfo), cgroup = Paths.get("/nonexistent-cgroup"))
    sample(out, Available) shouldBe Some(651932d * 1024d)
    sample(out, TotalMem) shouldBe Some(1009836d * 1024d)
  }

  it should "not publish negative headroom when usage has passed the limit" in {
    // memory.current can exceed memory.max briefly before the kernel reclaims or
    // kills. A negative gauge would render as a downward spike on a bytes panel.
    val out = render(meminfoAt(Meminfo), cgroup = cgroupDir("1073741824", "1200000000"))
    sample(out, Available) shouldBe Some(0d)
  }

  /** The gauge's sample value, or `None` when the family carried no data point. */
  private def sample(exposition: String, name: String): Option[Double] =
    exposition.linesIterator.collectFirst {
      case line if line.startsWith(s"""$name{country="pl"} """) => line.split(' ').last.toDouble
    }

  /** `Meminfo` on disk, for the tests that vary the CGROUP and so cannot use
   *  `withMeminfo`, which fixes the cgroup at "absent". */
  private def meminfoAt(contents: String): Path = {
    val file = Files.createTempFile("meminfo", ".txt")
    Files.writeString(file, contents)
    file
  }

  private def withMeminfo(contents: String)(check: String => Unit): Unit = {
    val file = Files.createTempFile("meminfo", ".txt")
    try {
      Files.writeString(file, contents)
      check(render(file))
    } finally Files.deleteIfExists(file)
  }

  "the host memory gauges" should "report MemAvailable, not MemFree, in bytes" in withMeminfo(Meminfo) { exposition =>
    sample(exposition, Available) shouldBe Some(651932.0 * 1024) // NOT MemFree's 73048 kB
    sample(exposition, TotalMem) shouldBe Some(1009836.0 * 1024)
  }

  it should "publish no sample when there is no /proc/meminfo to read" in {
    val exposition = render(Paths.get("/no/such/proc/meminfo"))
    sample(exposition, Available) shouldBe None
    sample(exposition, TotalMem) shouldBe None
  }

  it should "publish no sample when the file exists but omits the field" in withMeminfo("MemFree: 1 kB\n") { exposition =>
    sample(exposition, Available) shouldBe None
  }

  "the host disk gauges" should "report the root filesystem's free and total space" in withMeminfo(Meminfo) { exposition =>
    val free  = sample(exposition, DiskFree).getOrElse(fail(s"$DiskFree carried no sample"))
    val total = sample(exposition, DiskTotal).getOrElse(fail(s"$DiskTotal carried no sample"))
    total should be > 0.0
    free should be > 0.0
    free should be <= total
  }

  it should "publish no sample for a mount point the process cannot stat" in {
    val exposition = render(Paths.get("/no/such/proc/meminfo"), new File("/no/such/mount/point"))
    sample(exposition, DiskFree) shouldBe None
    sample(exposition, DiskTotal) shouldBe None
  }

  "meminfoBytes" should "not match a field whose name merely prefixes the key" in {
    // `Mem` against `MemTotal`/`MemFree`/`MemAvailable`: a prefix match reads the wrong line.
    WebHostMetrics.meminfoBytes(Meminfo, "Mem") shouldBe None
    WebHostMetrics.meminfoBytes(Meminfo, "MemTotal") shouldBe Some(1009836L * 1024)
  }

  it should "not match a count field that carries no kB unit" in {
    // HugePages_Total is a page COUNT; publishing it as bytes would be silently wrong.
    WebHostMetrics.meminfoBytes(Meminfo, "HugePages_Total") shouldBe None
  }
}
