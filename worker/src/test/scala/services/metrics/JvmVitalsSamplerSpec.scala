package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the native-memory census the worker exposes for diagnosing its
 * non-heap OOM restarts: `kinowo_worker_native_memory_bytes{category=…}`,
 * parsed from the HotSpot NMT `summary` dump. The parser is the load-bearing
 * piece (an NMT format drift would silently zero the gauge), so it is tested
 * against a real captured dump.
 */
class JvmVitalsSamplerSpec extends AnyFlatSpec with Matchers {

  // A real `jcmd VM.native_memory summary` dump (trimmed), incl. a multi-word
  // category (Native Memory Tracking) and the extra ", readonly=…KB" field
  // (Shared class space) — the two shapes most likely to trip a naive parser.
  private val nmtSummary =
    """Native Memory Tracking:
      |
      |Total: reserved=11351974KB, committed=734355KB
      |
      |-                 Java Heap (reserved=9437184KB, committed=598016KB)
      |-                    Thread (reserved=59815KB, committed=59815KB)
      |-                      Code (reserved=248071KB, committed=8727KB)
      |-                        GC (reserved=240698KB, committed=68058KB)
      |-                 Metaspace (reserved=65593KB, committed=11193KB)
      |-    Native Memory Tracking (reserved=1036KB, committed=1036KB)
      |-        Shared class space (reserved=16384KB, committed=12944KB, readonly=0KB)
      |""".stripMargin

  // A trimmed real `/proc/self/status` blob. Includes the sibling Vm* fields
  // (VmHWM/VmData/VmSize) most likely to trip a parser that matches too loosely —
  // only VmRSS is the resident-set gauge we want.
  private val procStatus =
    """Name:	java
      |State:	S (sleeping)
      |VmPeak:	 2410960 kB
      |VmSize:	 2400000 kB
      |VmHWM:	  900000 kB
      |VmRSS:	  812345 kB
      |VmData:	 1700000 kB
      |Threads:	88
      |""".stripMargin

  "parseCommittedByCategory" should "read committed KB→bytes for each category" in {
    val m = JvmVitalsSampler.parseCommittedByCategory(nmtSummary)
    m("Thread")    shouldBe 59815L * 1024
    m("Code")      shouldBe 8727L * 1024
    m("GC")        shouldBe 68058L * 1024
    m("Metaspace") shouldBe 11193L * 1024
    m("Java Heap") shouldBe 598016L * 1024
  }

  it should "handle multi-word categories and a trailing readonly field" in {
    val m = JvmVitalsSampler.parseCommittedByCategory(nmtSummary)
    m("Native Memory Tracking") shouldBe 1036L * 1024
    m("Shared class space")     shouldBe 12944L * 1024 // committed, not the readonly=0
  }

  "a disabled-NMT dump" should "yield no categories (never throw)" in {
    JvmVitalsSampler.parseCommittedByCategory(
      "Native memory tracking is not enabled") shouldBe empty
    JvmVitalsSampler.parseCommittedByCategory("") shouldBe empty
  }

  "parseVmRssBytes" should "read VmRSS kB→bytes, not a sibling Vm* field" in {
    JvmVitalsSampler.parseVmRssBytes(procStatus) shouldBe Some(812345L * 1024)
  }

  it should "yield None when VmRSS is absent (non-Linux/empty)" in {
    JvmVitalsSampler.parseVmRssBytes("") shouldBe None
    JvmVitalsSampler.parseVmRssBytes("Name:\tjava\nVmSize:\t100 kB\n") shouldBe None
  }

  "JvmVitalsSampler.sample" should "publish per-category committed bytes onto the registry" in {
    val registry = new PrometheusRegistry()
    val sampler  = new JvmVitalsSampler(
      registry, readNmtSummary = () => nmtSummary, readProcStatus = () => procStatus)

    sampler.sample()
    val text = PrometheusExposition.render(registry)

    PrometheusExposition.sample(text, JvmVitalsSampler.Name, """category="Thread"""") shouldBe
      Some(59815.0 * 1024)
    PrometheusExposition.sample(text, JvmVitalsSampler.Name, """category="GC"""") shouldBe
      Some(68058.0 * 1024)
  }

  it should "publish RSS and the RSS − NMT-committed off-book gap" in {
    val registry = new PrometheusRegistry()
    val sampler  = new JvmVitalsSampler(
      registry, readNmtSummary = () => nmtSummary, readProcStatus = () => procStatus)

    sampler.sample()
    val text = PrometheusExposition.render(registry)

    val committedSum = JvmVitalsSampler.parseCommittedByCategory(nmtSummary).values.sum
    PrometheusExposition.value(text, JvmVitalsSampler.RssName) shouldBe Some(812345.0 * 1024)
    PrometheusExposition.value(text, JvmVitalsSampler.GapName) shouldBe
      Some((812345L * 1024 - committedSum).toDouble)
  }

  it should "seed the common categories at 0 before the first sample" in {
    val registry = new PrometheusRegistry()
    new JvmVitalsSampler(registry, readNmtSummary = () => "") // constructed, not sampled
    val text = PrometheusExposition.render(registry)

    JvmVitalsSampler.SeedCategories.foreach { c =>
      PrometheusExposition.sample(text, JvmVitalsSampler.Name, s"""category="$c"""") shouldBe Some(0.0)
    }
    PrometheusExposition.value(text, JvmVitalsSampler.RssName) shouldBe Some(0.0)
    PrometheusExposition.value(text, JvmVitalsSampler.GapName) shouldBe Some(0.0)
  }
}
