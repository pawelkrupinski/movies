package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.movies.{ListingKey, ListingKeyedRows, ScreeningsRepository, SlotKeyed, SlotsRepository}

import scala.concurrent.duration._

/**
 * Counts the side-collection rows (`movie_slots`, `screenings`) that hold a venue listing but
 * carry no `listingKey` — the rows a read by listing cannot find (docs/design/identity-resolver.md
 * §16). The dual write stamps every row it writes, so after `scripts.ListingKeyBackfill --apply`
 * this is zero; above zero means a write path that bypasses the stamp, or a backfill still owed.
 * Dual reads may not start in a country until it reads zero.
 *
 * The documented exemptions are not counted: a row is expected to carry a key exactly when
 * [[ListingKey.isVenueRow]] holds for its wire key — never for an enrichment slot (TMDB, IMDb,
 * Filmweb), a chain's network-level detail slot, or a retired venue's row (the retired-venue
 * census watches those). The same rule the write path stamps by, so the two cannot disagree.
 *
 * Reads ids and keys only. A store whose read fails publishes nothing ("could not read" is not
 * "none").
 */
class UnstampedListingCensus(
  screenings: ListingKeyedRows,
  slots:      ListingKeyedRows,
  rows:       Gauge,
  country:    Country,
  override protected val sampleInterval: FiniteDuration = UnstampedListingCensus.DefaultSampleInterval
) extends SampledCensus {

  private val stores = Seq(SlotsRepository.Collection -> slots, ScreeningsRepository.Collection -> screenings)

  def sample(): Unit = stores.foreach { case (collection, store) =>
    val (keys, read) = store.rowListingKeysChecked()
    if (read) rows.labelValues(country.code, collection).set(UnstampedListingCensus.unstamped(keys).size.toDouble)
  }

  override protected val censusName: String = "unstamped-listing-census"
}

object UnstampedListingCensus {
  val Name = "kinowo_worker_listing_key_unstamped_rows"

  /** Hourly: the count moves with deploys and the one-shot backfill, not by the minute. */
  val DefaultSampleInterval: FiniteDuration = 1.hour

  /** The row ids among `keys` that hold a venue listing and carry no key. */
  def unstamped(keys: Map[String, Option[String]]): Set[String] =
    keys.collect { case (id, None) if ListingKey.isVenueRow(SlotKeyed.slotKeyOf(id)) => id }.toSet

  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Side-collection rows (collection=movie_slots|screenings) that hold a venue listing but carry no listingKey, by country: the rows a read by listing cannot find. Enrichment slots (TMDB/IMDb/Filmweb), chain network detail slots and retired venues are exempt. Zero once the dual write is deployed and scripts.ListingKeyBackfill has been applied; dual reads wait for zero. Hourly, id+key reads; a failed read keeps the last value.")
      .labelNames("country", "collection")
      .register(registry)
}
