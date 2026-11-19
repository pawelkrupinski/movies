package services.corpus

import io.prometheus.metrics.core.metrics.Counter

/** Where [[CorpusCensus]] reports a tick that could not count the whole corpus. A trait so the census stays testable
 *  without a Prometheus registry. */
trait CorpusCensusMetrics {
  def recordIncompleteSample(): Unit
}

object CorpusCensusMetrics {
  val noop: CorpusCensusMetrics = new CorpusCensusMetrics { def recordIncompleteSample(): Unit = () }

  /** Binds one country's slice of the shared counter, materializing the series at 0 up front: a healthy country must
   *  be an explicit 0, not an absent series, or an alert on it has nothing to compare against. */
  def prometheus(counter: Counter, countryCode: String): CorpusCensusMetrics = {
    counter.labelValues(countryCode)
    new CorpusCensusMetrics {
      def recordIncompleteSample(): Unit = counter.labelValues(countryCode).inc()
    }
  }
}
