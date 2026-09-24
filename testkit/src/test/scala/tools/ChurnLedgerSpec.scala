package tools

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicLong

class ChurnLedgerSpec extends AnyFlatSpec with Matchers {

  private def registryWith(): (PrometheusRegistry, Counter) = {
    val registry = new PrometheusRegistry()
    val counter  = Counter.builder().name("kinowo_worker_merges").help("h").labelNames("country", "reason").register(registry)
    (registry, counter)
  }

  "ChurnLedger" should "report only the series a pass moved, without the country label" in {
    val (registry, merges) = registryWith()
    merges.labelValues("pl", "canonicalize").inc()
    val ledger = new ChurnLedger().registry(registry, Set("kinowo_worker_merges"))

    ledger.churnOf(merges.labelValues("pl", "tmdb-identity").inc(2)) shouldBe
      Map("kinowo_worker_merges{reason=tmdb-identity}" -> 2.0)
    ledger.churnOf(()) shouldBe empty
  }

  // A registry several countries' wirings share (one JVM, one default registry) must not let
  // one country's series overwrite another's once the label is gone.
  it should "keep the country label when the registry holds more than one country" in {
    val (registry, merges) = registryWith()
    merges.labelValues("pl", "canonicalize").inc()
    merges.labelValues("uk", "canonicalize").inc()
    val ledger = new ChurnLedger().registry(registry, Set("kinowo_worker_merges"))

    ledger.churnOf { merges.labelValues("pl", "canonicalize").inc(2); merges.labelValues("uk", "canonicalize").inc(3) } shouldBe
      Map("kinowo_worker_merges{country=pl,reason=canonicalize}" -> 2.0,
          "kinowo_worker_merges{country=uk,reason=canonicalize}" -> 3.0)
  }

  it should "print a series that went DOWN with its own sign" in {
    ChurnLedger.describe(Map("writes" -> -3.0)) should (include("-3") and not include("+-"))
  }

  it should "leave out the series `keep` says are not work" in {
    val (registry, merges) = registryWith()
    val ledger = new ChurnLedger().registry(registry, Set("kinowo_worker_merges"),
      keep = (_, labels) => labels.get("reason") != "canonicalize")

    ledger.churnOf(merges.labelValues("pl", "canonicalize").inc()) shouldBe empty
  }

  it should "count a subscription's deliveries from the moment it is registered" in {
    var ring: () => Unit = () => ()
    val ledger = new ChurnLedger().deliveries("stream")(tick => ring = tick)

    ledger.churnOf { ring(); ring() } shouldBe Map("stream" -> 2.0)
  }

  it should "fail a pass that did work, naming the axis and the explanation" in {
    val writes = new AtomicLong(0)
    val ledger = new ChurnLedger().counter("writes")(writes.get).explain("the film that moved: Foo")

    val failure = the[TestFailedException] thrownBy ledger.assertNoChurn("a quiet pass")(writes.incrementAndGet())
    failure.getMessage should (include("a quiet pass did work") and include("writes") and include("Foo"))
    noException should be thrownBy ledger.assertNoChurn("a quiet pass")(())
  }
}
