package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{KinoApollo, KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.ListingCompleteness

/** A venue whose every listing lands incomplete never has its stopped films pruned; these
 *  series are what make that visible. */
class ListingIncompleteMetricsSpec extends AnyFlatSpec with Matchers {

  private val Roster: Set[models.Cinema] = Set(Multikino, KinoMuza)

  private def scraped(registry: PrometheusRegistry, name: String, labels: (String, String)*): Double =
    PrometheusExposition.sample(PrometheusExposition.render(registry), name,
      labels.map { case (k, v) => s"""$k="$v"""" }.mkString(",")).getOrElse(fail(s"no $name{$labels}"))

  "the recorder" should "count incomplete landings by reason, and nothing for a complete one" in {
    val registry = new PrometheusRegistry
    val recorder = new ListingIncompleteMetrics(Seq("pl"), registry).recorderFor("pl", Roster)
    recorder.landed(Multikino, ListingCompleteness.PageFailed)
    recorder.landed(Multikino, ListingCompleteness.PageFailed)
    recorder.landed(KinoMuza, ListingCompleteness.ChunkIncomplete)
    recorder.landed(KinoMuza, ListingCompleteness.Complete)
    scraped(registry, "kinowo_worker_scrape_listing_incomplete_total", "country" -> "pl", "reason" -> "page_failed") shouldBe 2
    scraped(registry, "kinowo_worker_scrape_listing_incomplete_total", "country" -> "pl", "reason" -> "chunk_incomplete") shouldBe 1
  }

  it should "count a venue as stuck once it has landed the threshold incomplete in a row, until a complete one" in {
    val registry = new PrometheusRegistry
    val recorder = new ListingIncompleteMetrics(Seq("pl"), registry).recorderFor("pl", Roster)
    def stuck = scraped(registry, "kinowo_worker_scrape_listing_incomplete_streak_venues", "country" -> "pl")
    (1 until ListingIncompleteMetrics.StreakThreshold).foreach(_ => recorder.landed(Multikino, ListingCompleteness.PageFailed))
    stuck shouldBe 0
    recorder.landed(Multikino, ListingCompleteness.PageFailed)
    stuck shouldBe 1
    recorder.landed(KinoMuza, ListingCompleteness.PageFailed)
    stuck shouldBe 1
    recorder.landed(Multikino, ListingCompleteness.Complete)
    stuck shouldBe 0
  }

  // A venue off the roster is never scraped again, so a streak it held would never end: the
  // gauge would count it stuck for the life of the process.
  it should "never count a venue off the roster as stuck" in {
    val registry = new PrometheusRegistry
    val recorder = new ListingIncompleteMetrics(Seq("pl"), registry).recorderFor("pl", Roster)
    def stuck = scraped(registry, "kinowo_worker_scrape_listing_incomplete_streak_venues", "country" -> "pl")
    (1 to ListingIncompleteMetrics.StreakThreshold).foreach(_ => recorder.landed(KinoApollo, ListingCompleteness.PageFailed))
    stuck shouldBe 0
    scraped(registry, "kinowo_worker_scrape_listing_incomplete_total", "country" -> "pl", "reason" -> "page_failed") shouldBe
      ListingIncompleteMetrics.StreakThreshold
  }
}
