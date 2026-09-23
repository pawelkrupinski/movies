package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.UserStateWriteOutcomes.{Endpoint, Outcome}

class UserStateWriteMetricsSpec extends AnyFlatSpec with Matchers {

  "UserStateWriteMetrics" should "seed every endpoint × outcome at 0, so a rate over it exists before the first failure" in {
    val registry = new PrometheusRegistry()
    val _ = new UserStateWriteMetrics(registry, "pl")
    val text = PrometheusExposition.render(registry)
    for (e <- Endpoint.all; o <- Outcome.all)
      text should include (s"""kinowo_web_user_state_writes_total{country="pl",endpoint="$e",outcome="$o"} 0""")
  }

  it should "count each write on its endpoint and outcome" in {
    val registry = new PrometheusRegistry()
    val metrics  = new UserStateWriteMetrics(registry, "pl")
    metrics.record(Endpoint.Hide, Outcome.Ok)
    metrics.record(Endpoint.Hide, Outcome.Conflict)
    metrics.record(Endpoint.LegacyPut, Outcome.StoreFailure)
    metrics.record(Endpoint.LegacyPut, Outcome.StoreFailure)

    val text = PrometheusExposition.render(registry)
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="hide",outcome="ok"} 1""")
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="hide",outcome="conflict"} 1""")
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="legacy_put",outcome="store_failure"} 2""")
    text should include ("""kinowo_web_user_state_writes_total{country="pl",endpoint="unhide",outcome="ok"} 0""")
  }
}
