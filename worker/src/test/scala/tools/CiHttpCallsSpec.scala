package tools

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerHttpMetrics

class CiHttpCallsSpec extends AnyFlatSpec with Matchers {

  "WorkerHttpMetrics.snapshot" should "read back every cell's count with its labels" in {
    val metrics = new WorkerHttpMetrics(Seq("pl"), new PrometheusRegistry())
    metrics.recorderFor("pl", WorkerHttpMetrics.Phase.Enrich).record(HttpOutcome.Http429)
    val cells = metrics.snapshot().getDataPoints.toArray.toSeq.collect {
      case p: io.prometheus.metrics.model.snapshots.CounterSnapshot.CounterDataPointSnapshot =>
        (p.getLabels.get("phase"), p.getLabels.get("outcome")) -> p.getValue
    }.toMap
    cells(("enrich", HttpOutcome.Http429)) shouldBe 1.0
    cells(("scrape", HttpOutcome.Success)) shouldBe 0.0
  }

  "CiHttpCalls" should "sum every tracked wiring's live calls and keep only the cells that were asked" in {
    def pass(successes: Int): WorkerHttpMetrics = {
      val metrics = new WorkerHttpMetrics(Seq("de"), new PrometheusRegistry())
      val enrich  = metrics.recorderFor("de", WorkerHttpMetrics.Phase.Enrich)
      (1 to successes).foreach(_ => enrich.record(HttpOutcome.Success))
      metrics
    }
    val (first, second) = (pass(2), pass(3))
    CiHttpCalls.track(first)
    CiHttpCalls.track(second)
    val ours = CiHttpCalls.totals().filter(_._1.contains("country" -> "de"))
    ours shouldBe Map(Seq("country" -> "de", "outcome" -> HttpOutcome.Success, "phase" -> "enrich") -> 5.0)
  }

  it should "write the cells as a TSV: a header of label names then count, a row per cell" in {
    CiHttpCalls.tsv(Map(
      Seq("country" -> "pl", "outcome" -> "success", "phase" -> "enrich") -> 7.0,
      Seq("country" -> "pl", "outcome" -> "http_429", "phase" -> "enrich") -> 2.0)) shouldBe
      "country\toutcome\tphase\tcount\npl\thttp_429\tenrich\t2\npl\tsuccess\tenrich\t7\n"
  }
}
