package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UserStateIndexMetricsSpec extends AnyFlatSpec with Matchers {

  "UserStateIndexMetrics" should "export 0 for a failed unique-index build and 1 once it is in place" in {
    val registry = new PrometheusRegistry()
    val metrics  = new UserStateIndexMetrics(registry, "uk")
    metrics.uniqueUserIdIndex(false)
    PrometheusExposition.render(registry) should include ("""kinowo_web_user_state_userid_index_unique{country="uk"} 0""")
    metrics.uniqueUserIdIndex(true)
    PrometheusExposition.render(registry) should include ("""kinowo_web_user_state_userid_index_unique{country="uk"} 1""")
  }
}
