package services.identity

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.metrics.SampledCensus
import services.movies.{ListingKey, ScreeningsRepository, SlotKeyed, SlotsRepository, StoredSlotDto}
import settings.ListingKeyShadowSample

import scala.concurrent.duration._
import scala.util.Random

/**
 * The identity migration's SHADOW READ (docs/design/identity-resolver.md §10 phase 2, §16): for a
 * sample of venue slot rows, resolve each listing both ways and compare the answers.
 *
 *  - today's read, by slot key: the `movie_slots` row at `(filmId, slotKey)`, and the `screenings`
 *    row at the same id when there is one;
 *  - the dual read, by listing: every row of each collection stamped with the key that slot
 *    derives ([[StoredSlotDto.listingKeyOf]], the stamp's own derivation), served by the
 *    `listingKey` index.
 *
 * The two must name the same rows. A row the key read misses is unstamped or stamped stale; a row
 * it finds beyond the slot's own is a listing filed twice (across films, or a legacy bare-cinema
 * slot beside its per-title successor); a `screenings` answer that differs is a showtimes row keyed
 * apart from its slot. Each counts, per collection, into the gauge, and the first
 * [[ListingKeyShadowRead.MaxLoggedDisagreements]] are logged with their keys.
 *
 * It serves nothing and writes nothing but the gauge; it runs only behind
 * `KINOWO_LISTING_KEY_SHADOW_READ` (off by default), wired at the composition root. The comparison
 * lives here, above the repositories, so the Mongo and in-memory stores answer the same question.
 */
class ListingKeyShadowRead(
  slots:      SlotsRepository,
  screenings: ScreeningsRepository,
  sampleSize: ListingKeyShadowSample,
  outcomes:   Gauge,
  country:    Country,
  random:     Random,
  override protected val sampleInterval: FiniteDuration = ListingKeyShadowRead.DefaultSampleInterval
) extends SampledCensus {
  import ListingKeyShadowRead._

  /** One tick's comparison, or None when a read it needed failed (nothing is then published). */
  def compare(): Option[Report] = {
    val (keyed, read) = slots.rowListingKeysChecked()
    if (!read) None
    else {
      val venueRows = keyed.keys.filter(id => ListingKey.isVenueRow(SlotKeyed.slotKeyOf(id))).toSeq.sorted
      val sampled   = random.shuffle(venueRows).take(sampleSize.value)
      val films     = sampled.map(SlotKeyed.filmIdOf).toSet
      val (slotRows, slotsRead)           = slots.findForFilmsChecked(films)
      val (screeningRows, screeningsRead) = screenings.findForFilmsChecked(films)
      Option.when(slotsRead && screeningsRead) {
        val compared = sampled.flatMap { id =>
          val (filmId, slotKey) = (SlotKeyed.filmIdOf(id), SlotKeyed.slotKeyOf(id))
          // A row deleted between the id scan and the film read is no answer either way.
          slotRows.get(filmId).flatMap(_.get(slotKey)).flatMap(StoredSlotDto.listingKeyOf(slotKey, _)).map { key =>
            val (slotsByKey, slotsOk)           = slots.rowIdsForListingKeyChecked(key)
            val (screeningsByKey, screeningsOk) = screenings.rowIdsForListingKeyChecked(key)
            val screeningsBySlot = if (screeningRows.get(filmId).exists(_.contains(slotKey))) Set(id) else Set.empty[String]
            Option.when(slotsOk && screeningsOk)(Compared(id, key, Set(id), slotsByKey, screeningsBySlot, screeningsByKey))
          }
        }
        Report(compared.flatten, unread = compared.count(_.isEmpty))
      }
    }
  }

  def sample(): Unit = compare().foreach { report =>
    Outcomes.foreach(o => outcomes.labelValues(country.code, o).set(report.count(o).toDouble))
    report.disagreements.take(MaxLoggedDisagreements).foreach(d => logger.warn(s"$censusName ${country.code}: ${d.describe}"))
  }

  override protected val censusName: String = "listing-key-shadow-read"
}

object ListingKeyShadowRead {
  val Name = "kinowo_worker_listing_key_shadow_read_rows"

  val DefaultSampleInterval: FiniteDuration = 1.hour
  /** Sampled rows per tick unless `KINOWO_LISTING_KEY_SHADOW_SAMPLE` says otherwise: two index
   *  reads each, so a tick is ~1,000 point reads. */
  val DefaultSample: ListingKeyShadowSample = ListingKeyShadowSample(500)
  /** Disagreements a tick logs, with their keys; the gauge counts all of them. */
  val MaxLoggedDisagreements = 10

  val Agree              = "agree"
  val SlotsDisagree      = "slots_disagree"
  val ScreeningsDisagree = "screenings_disagree"
  val Unread             = "unread"
  val Outcomes: Seq[String] = Seq(Agree, SlotsDisagree, ScreeningsDisagree, Unread)

  /** One sampled listing, read both ways. */
  final case class Compared(rowId: String, listingKey: String,
                            slotsBySlotKey: Set[String], slotsByListingKey: Set[String],
                            screeningsBySlotKey: Set[String], screeningsByListingKey: Set[String]) {
    def slotsAgree: Boolean      = slotsBySlotKey == slotsByListingKey
    def screeningsAgree: Boolean = screeningsBySlotKey == screeningsByListingKey
    def agrees: Boolean          = slotsAgree && screeningsAgree
    def describe: String = {
      def show(ids: Set[String]) = ids.toSeq.sorted.map(readable).mkString("[", "; ", "]")
      s"${readable(rowId)} listing ${readable(listingKey)}: movie_slots by slot key ${show(slotsBySlotKey)} vs by listing " +
        s"${show(slotsByListingKey)}; screenings by slot key ${show(screeningsBySlotKey)} vs by listing ${show(screeningsByListingKey)}"
    }
  }

  /** One tick: every compared listing, and how many could not be read by listing. */
  final case class Report(compared: Seq[Compared], unread: Int) {
    def disagreements: Seq[Compared] = compared.filterNot(_.agrees)
    def count(outcome: String): Int = outcome match {
      case Agree              => compared.count(_.agrees)
      case SlotsDisagree      => compared.count(!_.slotsAgree)
      case ScreeningsDisagree => compared.count(!_.screeningsAgree)
      case _                  => unread
    }
  }

  /** A key or row id as a log line shows it: its NUL and unit separators as ` | `. */
  private def readable(s: String): String = s.replace('\u0000', '|').replace(SlotKeyed.IdSep, '|')

  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("The identity migration's shadow read, per country: of the venue slot rows sampled in the last tick, how many resolve to the same movie_slots and screenings rows by listingKey as by today's slot key (outcome=agree), how many differ in movie_slots (slots_disagree: unstamped, stale, or one listing filed twice) or in screenings (screenings_disagree), and how many could not be read by listing (unread). Dual reads wait for agree to be the whole sample. Published only where KINOWO_LISTING_KEY_SHADOW_READ is on; hourly.")
      .labelNames("country", "outcome")
      .register(registry)
}
