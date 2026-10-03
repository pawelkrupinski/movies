package tools

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A class loaded from the AOT cache costs no metaspace; one the cache lacks costs its share. The
 *  JVM counts both in its own perf data — nowhere a JMX bean reaches — so the gauge that shows code
 *  drifting away from the image's class list (`aot-classes.txt`) reads them there. */
class HotSpotPerfDataSpec extends AnyFlatSpec with Matchers {

  "this JVM's perf data" should "count the classes it loaded, and the archived share of them" in {
    val perf     = HotSpotPerfData.own().getOrElse(fail("no hsperfdata for this JVM"))
    val loaded   = perf.long("java.cls.loadedClasses").getOrElse(fail("no java.cls.loadedClasses"))
    val archived = perf.long("java.cls.sharedLoadedClasses").getOrElse(fail("no java.cls.sharedLoadedClasses"))
    archived should be > 0L          // the JDK's own CDS archive serves this test JVM's first classes
    archived should be < loaded      // and the test's own classes come from outside it
    perf.long("no.such.counter") shouldBe None
  }

  it should "read live values, not the ones at the time it opened the file" in {
    val perf   = HotSpotPerfData.own().getOrElse(fail("no hsperfdata for this JVM"))
    val before = perf.long("java.cls.loadedClasses").get
    // A class of this test's own, loaded only as this line runs: a JDK class "nothing else loads" was
    // already loaded by whichever spec in the same JVM had reached it first, and the count stood still.
    val fresh  = new Serializable {}
    perf.long("java.cls.loadedClasses").get should be > before
    fresh.getClass.getClassLoader should not be null
  }

  "the JVM metrics" should "export how many loaded classes came from the archive" in {
    val registry = new PrometheusRegistry()
    services.metrics.JvmProcessMetrics.register(registry)
    def value(name: String) = registry.scrape().stream().filter(_.getMetadata.getPrometheusName == name).findFirst()
      .map(_.getDataPoints.get(0).asInstanceOf[io.prometheus.metrics.model.snapshots.GaugeSnapshot.GaugeDataPointSnapshot].getValue)
    val archived = value("jvm_classes_archived_loaded").orElseThrow()
    archived should be > 0.0
    archived should be < value("jvm_classes_currently_loaded").orElseThrow()
  }
}
