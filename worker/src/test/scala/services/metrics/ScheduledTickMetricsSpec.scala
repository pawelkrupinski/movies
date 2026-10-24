package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration.*

/** The pulse `ScheduledTickStale` holds every periodic loop to: when it last completed a run, and
 *  how often it means to. */
class ScheduledTickMetricsSpec extends AnyFlatSpec with Matchers {

  private val LastSuccess = "kinowo_worker_scheduled_tick_last_success_timestamp_seconds"
  private val Interval    = "kinowo_worker_scheduled_tick_interval_seconds"
  private val Labels      = """country="pl",tick="scrape-reaper""""

  "ScheduledTickMetrics" should "seed a loop with its first run's due time, then stamp each completed run" in {
    val clock    = new MutableClock(Instant.parse("2026-10-24T10:00:00Z"))
    val metrics  = WorkerMetrics.singleCountry(models.Country.Poland, poolSize = settings.WorkerPoolSize(1))
    val ticks    = metrics.scheduledTicks.forCountry("pl", clock)
    val armedAt  = clock.instant().getEpochSecond.toDouble

    ticks.started("scrape-reaper", 1.minute, 5.minutes)
    val armed = PrometheusExposition.render(metrics.registry)
    PrometheusExposition.sample(armed, LastSuccess, Labels) shouldBe Some(armedAt + 300)
    PrometheusExposition.sample(armed, Interval, Labels) shouldBe Some(60.0)

    clock.advance(java.time.Duration.ofMinutes(7))
    ticks.completed("scrape-reaper", 2.minutes)
    val ran = PrometheusExposition.render(metrics.registry)
    PrometheusExposition.sample(ran, LastSuccess, Labels) shouldBe Some(armedAt + 420)
    PrometheusExposition.sample(ran, Interval, Labels) shouldBe Some(120.0) // an /admin/config flip, read afresh
  }
}
