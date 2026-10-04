package services.readmodel

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{PrometheusExposition, WebReadModelStreamMetrics}

/** A Mongo blip that ended the read model's change streams was a WARN line only: the pod served
 *  writes late with nothing on a panel. The two series a real model now feeds, per collection. */
class WebReadModelStreamMetricsSpec extends AnyFlatSpec with Matchers {

  "the read-model stream metrics" should "show an ended stream as 0 and count its reopen, per collection" in {
    val registry   = new PrometheusRegistry()
    val repository = new InMemoryReadModelRepository
    @volatile var model: WebReadModel = null
    val metrics = new WebReadModelStreamMetrics(registry, "pl", collection => model.streamLive(collection))
    model = new WebReadModel(repository, streamMetrics = metrics, clock = _root_.tools.SpecClock.Pinned)
    model.start()
    try {
      PrometheusExposition.render(registry) should include ("""kinowo_web_readmodel_stream_live{collection="web_movies",country="pl"} 1""")

      repository.failMovieStream()
      val down = PrometheusExposition.render(registry)
      down should include ("""kinowo_web_readmodel_stream_live{collection="web_movies",country="pl"} 0""")
      down should include ("""kinowo_web_readmodel_stream_live{collection="web_screenings",country="pl"} 1""")

      model.coldRetryTick()
      val reopened = PrometheusExposition.render(registry)
      reopened should include ("""kinowo_web_readmodel_stream_live{collection="web_movies",country="pl"} 1""")
      reopened should include ("""kinowo_web_readmodel_stream_reopens_total{collection="web_movies",country="pl"} 1""")
      reopened should include ("""kinowo_web_readmodel_stream_reopens_total{collection="web_screenings",country="pl"} 0""")
    } finally model.stop()
  }
}
