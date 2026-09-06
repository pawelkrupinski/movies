package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures.row

/**
 * The widest-film gauge must count the slots that BECOME `screenings` rows, and nothing else.
 *
 * It counted every entry in the record's `data` map until 2026-09-06, which folded the Tmdb /
 * Imdb / Filmweb metadata slots into a number the help text, the panel and the whole
 * blast-radius argument describe as cinema slots. Every film read two or three wider than it
 * is, and a film with no venue at all reported a fanout — a metric that cannot be zero on a
 * film nothing screens is not measuring what it says.
 */
class WorkerSlotFanoutMetricsSpec extends AnyFlatSpec with Matchers {

  private def fixture = {
    val registry = new PrometheusRegistry()
    val gauge = Gauge.builder().name("kinowo_worker_film_widest_slots")
      .help("test").labelNames("country").register(registry)
    (registry, new WorkerSlotFanoutMetrics(gauge, "pl"))
  }

  private def widest(registry: PrometheusRegistry): Option[Double] =
    PrometheusExposition.sample(PrometheusExposition.render(registry),
      "kinowo_worker_film_widest_slots", """country="pl"""")

  private def slot(title: String) = SourceData(title = Some(title))

  "WorkerSlotFanoutMetrics" should "count a film's cinema slots and not its metadata sources" in {
    val (registry, metrics) = fixture
    val sampler = metrics.startSample()
    // Two venues, three metadata sources. The blast radius is two.
    sampler.accept(row("Wide Release", MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> slot("Wide Release"),
      KinoMuranow -> slot("Wide Release"),
      Tmdb        -> slot("Wide Release"),
      Imdb        -> slot("Wide Release"),
      Filmweb     -> slot("Wide Release")))))
    sampler.publish(scanComplete = true)

    widest(registry) shouldBe Some(2.0)
  }

  it should "report zero for a film that no cinema screens" in {
    val (registry, metrics) = fixture
    val sampler = metrics.startSample()
    // Resolved against TMDB, showing nowhere. It writes no `screenings` row, so it has no
    // blast radius — the case that made the old count structurally unable to reach zero.
    sampler.accept(row("Metadata Only", MovieRecord(tmdbId = Some(2), data = Map[Source, SourceData](
      Tmdb -> slot("Metadata Only"), Imdb -> slot("Metadata Only")))))
    sampler.publish(scanComplete = true)

    widest(registry) shouldBe Some(0.0)
  }

  it should "take the maximum across the corpus, not the last row" in {
    val (registry, metrics) = fixture
    val sampler = metrics.startSample()
    sampler.accept(row("Three Venues", MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot("Three Venues"), KinoMuranow -> slot("Three Venues"), Helios -> slot("Three Venues")))))
    sampler.accept(row("One Venue", MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot("One Venue")))))
    sampler.publish(scanComplete = true)

    widest(registry) shouldBe Some(3.0)
  }
}
