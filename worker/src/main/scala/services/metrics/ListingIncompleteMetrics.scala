package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Cinema
import play.api.Logging
import services.cinemas.common.{ListingCompleteness, ListingCompletenessRecorder}

import java.util.concurrent.ConcurrentHashMap

/**
 * `kinowo_worker_scrape_listing_incomplete_total{country,reason}` — every listing landed
 * INCOMPLETE (a page failed, or a chunked run came up short), and
 * `kinowo_worker_scrape_listing_incomplete_streak_venues{country}` — how many venues have
 * landed incomplete [[ListingIncompleteMetrics.StreakThreshold]] scrapes running.
 *
 * WHY: an incomplete listing skips the prune (its missing films are kept), which is right for
 * one scrape and silent staleness for a venue that is incomplete EVERY scrape — its stopped
 * films are never retired, and nothing else shows it. The counter says how often it happens;
 * the streak gauge and its WARN line (once, when a venue reaches the threshold) say which
 * venues are stuck there. No venue label: the venue is in the log line.
 */
class ListingIncompleteMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`.
  private val incomplete: Counter = Counter.builder()
    .name("kinowo_worker_scrape_listing_incomplete")
    .help("Listings landed INCOMPLETE since boot, by country and reason (page_failed: a page the " +
      "scrape read failed; chunk_incomplete: a chunked run reduced with a chunk missing or a read " +
      "inside it failed). An incomplete listing skips the prune.")
    .labelNames("country", "reason")
    .register(registry)

  private val streakVenues: Gauge = Gauge.builder()
    .name("kinowo_worker_scrape_listing_incomplete_streak_venues")
    .help(s"Venues whose last ${ListingIncompleteMetrics.StreakThreshold}+ landed listings were all " +
      "incomplete, so none of their stopped films has been pruned since.")
    .labelNames("country")
    .register(registry)

  for (c <- countryCodes) {
    ListingCompleteness.Reasons.foreach(r => incomplete.labelValues(c, r.label))
    streakVenues.labelValues(c).set(0)
  }

  /** The recorder one country's scrape runner reports to; only `roster`'s venues can be stuck. */
  def recorderFor(country: String, roster: Set[Cinema]): ListingCompletenessRecorder =
    new ListingIncompleteMetrics.Streaks(ListingIncompleteMetrics.StreakThreshold, roster,
      reason => incomplete.labelValues(country, reason.label).inc(),
      stuck => streakVenues.labelValues(country).set(stuck.toDouble))
}

object ListingIncompleteMetrics {
  /** How many incomplete landings in a row make a venue "stuck incomplete": a day of hourly
   *  scrapes in PL, a few days at the slower countries' cadence. */
  val StreakThreshold: Int = 24

  /** Each roster venue's run of incomplete landings, the stuck count it implies, and the one WARN as a
   *  venue reaches the threshold. A complete landing ends the run. A venue off the roster is counted
   *  incomplete but never holds a run: nothing scrapes it again, so no complete landing would ever
   *  end it, and the gauge would count it stuck for the life of the process. */
  private[metrics] final class Streaks(threshold: Int, roster: Set[Cinema], counted: ListingCompleteness => Unit, stuck: Int => Unit)
      extends ListingCompletenessRecorder with Logging {
    private val runs = new ConcurrentHashMap[Cinema, Integer]()

    def landed(cinema: Cinema, completeness: ListingCompleteness): Unit = {
      val incomplete = completeness != ListingCompleteness.Complete
      if (incomplete) counted(completeness)
      val run =
        if (incomplete && roster(cinema)) runs.merge(cinema, 1, (a, b) => a + b).intValue
        else { runs.remove(cinema); 0 }
      if (run == threshold)
        logger.warn(s"${cinema.displayName} has landed $threshold incomplete listings in a row (${completeness.label}) — " +
          "none of its stopped films is being pruned until a scrape reads its whole listing")
      stuck(runs.values.stream.filter(_ >= threshold).count.toInt)
    }
  }
}
