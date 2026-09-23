package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, Country}
import play.api.Logging
import services.cinemas.roster.DistinctVenuePairs
import services.movies.StoredMovieRecord

import java.time.{Clock, LocalDateTime, ZoneOffset}

/**
 * Counts pairs of roster venues in one city whose UPCOMING programmes are (nearly) the same —
 * one screen listed twice under two names.
 *
 * Two roster venues were once the same screen under two names, with identical showtimes, and
 * nothing noticed: the offline roster audit compares NAMES, and a pair whose names share
 * nothing passes it. What cannot differ between two listings of one screen is the programme,
 * so this compares programmes: each venue's upcoming (film, start time) set, and a pair counts
 * when the shared part is at least [[DuplicateVenueCensus.MinOverlap]] of the LARGER set — both
 * ways, so a small venue whose few showtimes a multiplex happens to share never matches.
 *
 * Same city only: a chain can run one national programme at the same times in two towns, which
 * is two screens. Venues with fewer than [[DuplicateVenueCensus.MinShowtimes]] upcoming
 * showtimes are skipped — six identical showtimes in two unrelated towns happen by chance. A
 * pair on [[DistinctVenuePairs]] (shared with the offline audit) is genuinely two screens.
 *
 * Rides the shared [[WorkerCorpusScan]] pass, so it costs no reads of its own. Each start time
 * is folded to one primitive `Long` per (film, minute) — no boxing — because the US pass holds
 * ~4,000 venues' programmes at once. The pairs found are logged when they CHANGE, so the alert
 * can be acted on without re-deriving them; publishing skips a partial scan like its siblings.
 */
class DuplicateVenueCensus(pairs: Gauge, country: Country, clock: Clock = Clock.systemUTC())
    extends CorpusMetricsCollector with Logging {
  import DuplicateVenueCensus._

  private val countryCode = country.code
  // A showtime is city-local wall-clock time; the country's first city's clock judges "upcoming"
  // for all of them, off by at most a zone's width in a multi-zone country — and a programme
  // compared against another programme on the SAME clock is unaffected by the offset anyway.
  private val zone       = country.cities.headOption.map(_.zoneId).getOrElse(ZoneOffset.UTC)
  private val cityVenues = country.cities.map(_.cinemas.distinct)
  @volatile private var reported: Set[(String, String)] = Set.empty

  pairs.labelValues(countryCode).set(0.0)

  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private val now       = LocalDateTime.now(clock.withZone(zone))
    private val programme = scala.collection.mutable.HashMap.empty[Cinema, scala.collection.mutable.ArrayBuilder.ofLong]

    def accept(row: StoredMovieRecord): Unit = {
      val film = row.id.value.hashCode.toLong << 32
      row.record.cinemaShowings.foreach { case (cinema, slot) =>
        slot.showtimes.foreach { showtime =>
          if (showtime.isUpcoming(now))
            programme.getOrElseUpdate(cinema, new scala.collection.mutable.ArrayBuilder.ofLong)
              .addOne(film | (showtime.dateTime.toEpochSecond(ZoneOffset.UTC) / 60 & 0xffffffffL))
        }
      }
    }

    def publish(scanComplete: Boolean): Unit = if (scanComplete) {
      val sets  = programme.view.mapValues(b => sortedDistinct(b.result())).toMap
      val found = overlapping(sets, cityVenues)
      pairs.labelValues(countryCode).set(found.size.toDouble)
      val named = found.map { case (a, b) => (a.displayName, b.displayName) }
      if (named != reported) {
        if (named.nonEmpty)
          logger.warn(s"duplicate-venue census ($countryCode): ${named.size} same-city venue pair(s) share " +
            s"${(MinOverlap * 100).round}%+ of their upcoming programme — one screen listed twice? " +
            named.toSeq.sorted.map { case (a, b) => s"'$a' / '$b'" }.mkString(", "))
        reported = named
      }
    }
  }
}

object DuplicateVenueCensus {
  val Name = "kinowo_worker_duplicate_venue_pairs"

  /** The share of the LARGER programme two venues must share to count as one screen. */
  val MinOverlap: Double = 0.9

  /** Below this many upcoming showtimes a venue's programme is too small to tell from chance. */
  val MinShowtimes: Int = 5

  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Pairs of roster venues in one city whose upcoming (film, start time) programmes overlap by 90% or more of the larger one, per country — one screen listed twice under two names, which the name-based roster audit cannot see. Venues with fewer than 5 upcoming showtimes are skipped; pairs on DistinctVenuePairs are genuinely two screens. Zero is healthy; the worker's WARN line names the pairs. Off the shared 5-min corpus scan. Alerted by DuplicateVenueListing.")
      .labelNames("country")
      .register(registry)

  /** Every same-city pair (each pair once, however many cities list both) whose sorted,
   *  de-duplicated programmes overlap by [[MinOverlap]] of the larger, skipping venues under
   *  [[MinShowtimes]] and pairs on [[DistinctVenuePairs]]. */
  private[metrics] def overlapping(programmes: Map[Cinema, Array[Long]], cities: Seq[Seq[Cinema]]): Set[(Cinema, Cinema)] = {
    val found = scala.collection.mutable.Set.empty[(Cinema, Cinema)]
    cities.foreach { venues =>
      val sized = venues.flatMap(v => programmes.get(v).filter(_.length >= MinShowtimes).map(v -> _))
      for {
        i <- sized.indices
        j <- (i + 1) until sized.size
      } {
        val (a, as) = sized(i)
        val (b, bs) = sized(j)
        if (shared(as, bs) >= MinOverlap * math.max(as.length, bs.length) && !DistinctVenuePairs.contains(a, b))
          found += (if (a.displayName <= b.displayName) (a, b) else (b, a))
      }
    }
    found.toSet
  }

  /** `keys` sorted and de-duplicated in place, primitive throughout. */
  private def sortedDistinct(keys: Array[Long]): Array[Long] = {
    java.util.Arrays.sort(keys)
    var n = 0
    keys.indices.foreach(i => if (i == 0 || keys(i) != keys(i - 1)) { keys(n) = keys(i); n += 1 })
    java.util.Arrays.copyOf(keys, n)
  }

  /** The size of the intersection of two sorted, de-duplicated arrays, by merge. */
  private def shared(a: Array[Long], b: Array[Long]): Int = {
    var i = 0; var j = 0; var n = 0
    while (i < a.length && j < b.length) {
      if (a(i) == b(j)) { n += 1; i += 1; j += 1 }
      else if (a(i) < b(j)) i += 1
      else j += 1
    }
    n
  }
}
