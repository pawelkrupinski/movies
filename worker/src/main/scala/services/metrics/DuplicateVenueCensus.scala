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
 * Two scopes, one label each. `same_city`: a pair listed in one city at 90%+ — a chain can run
 * one national programme at the same times in two towns, which is two screens, so across cities
 * that bar would mostly count chain-mates. `cross_city`: a pair with NO city in common at 95%+
 * and at least [[DuplicateVenueCensus.CrossCityMinShowtimes]] showtimes each — an aggregator
 * feeding one venue's programme under another venue's name, far away (Flicks gave the Syracuse
 * IN Pickwick the Park Ridge IL one's, 216/216). Venues with fewer than
 * [[DuplicateVenueCensus.MinShowtimes]] upcoming showtimes are skipped — six identical
 * showtimes in two unrelated towns happen by chance. A pair on [[DistinctVenuePairs]] (shared
 * with the offline audit) is genuinely two screens, in either scope.
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
  @volatile private var reported: Map[String, Set[(String, String)]] = Map.empty

  Scopes.foreach(scope => pairs.labelValues(countryCode, scope).set(0.0))

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
      val sets = programme.view.mapValues(b => sortedDistinct(b.result())).toMap
      publishScope(SameCity, overlapping(sets, cityVenues), MinOverlap, "one screen listed twice?")
      publishScope(CrossCity, overlappingAcrossCities(sets, cityVenues), CrossCityMinOverlap,
        "one venue's feed under another's name?")
    }

    private def publishScope(scope: String, found: Set[(Cinema, Cinema)], bar: Double, question: String): Unit = {
      pairs.labelValues(countryCode, scope).set(found.size.toDouble)
      val named = found.map { case (a, b) => (a.displayName, b.displayName) }
      if (!reported.get(scope).contains(named)) {
        if (named.nonEmpty)
          logger.warn(s"duplicate-venue census ($countryCode, $scope): ${named.size} venue pair(s) share " +
            s"${(bar * 100).round}%+ of their upcoming programme — $question " +
            named.toSeq.sorted.map { case (a, b) => s"'$a' / '$b'" }.mkString(", "))
        reported = reported.updated(scope, named)
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

  /** Across cities a chain's template schedule reaches 90% often (Odeon's run ~95%), so the bar
   *  is higher and the programme larger: a feed mix-up is identical, not merely alike. */
  val CrossCityMinOverlap: Double = 0.95
  val CrossCityMinShowtimes: Int  = 20

  val SameCity  = "same_city"
  val CrossCity = "cross_city"
  val Scopes: Seq[String] = Seq(SameCity, CrossCity)

  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Pairs of roster venues whose upcoming (film, start time) programmes are (nearly) the same, per country and scope. scope=same_city: two venues of one city overlapping by 90%+ of the larger programme — one screen listed twice under two names, which the name-based roster audit cannot see. scope=cross_city: two venues sharing no city, each with 20+ showtimes, overlapping by 95%+ — one venue's feed listed under another's name. Venues with fewer than 5 upcoming showtimes are skipped; pairs on DistinctVenuePairs are genuinely two screens. Zero is healthy; the worker's WARN line names the pairs. Off the shared 5-min corpus scan. Alerted by DuplicateVenueListing.")
      .labelNames("country", "scope")
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

  /** Every pair of venues that share NO city, each with at least [[CrossCityMinShowtimes]]
   *  showtimes, whose programmes overlap by [[CrossCityMinOverlap]] of the larger, skipping pairs
   *  on [[DistinctVenuePairs]].
   *
   *  All pairs of the US's ~4,000 venues is 8M merges, so candidates come from a prefix-filter
   *  join instead: order every key by how many venues hold it (rarest first); a pair sharing
   *  `t` of its keys must share one of the first `n - t + 1` keys of each set, so only venues
   *  meeting in those short, rare prefixes are merged. Exact, not approximate. */
  private[metrics] def overlappingAcrossCities(programmes: Map[Cinema, Array[Long]], cities: Seq[Seq[Cinema]]): Set[(Cinema, Cinema)] = {
    val cityOf  = cities.zipWithIndex.flatMap { case (venues, i) => venues.map(_ -> i) }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val venues  = programmes.iterator.filter { case (v, keys) => keys.length >= CrossCityMinShowtimes && cityOf.contains(v) }.toIndexedSeq
    val holders = scala.collection.mutable.HashMap.empty[Long, Int]
    venues.foreach { case (_, keys) => keys.foreach(k => holders.update(k, holders.getOrElse(k, 0) + 1)) }
    val index   = scala.collection.mutable.HashMap.empty[Long, scala.collection.mutable.ArrayBuffer[Int]]
    venues.indices.foreach { i =>
      val keys   = venues(i)._2
      val prefix = keys.length - math.ceil(CrossCityMinOverlap * keys.length).toInt + 1
      keys.sortBy(k => (holders(k), k)).iterator.take(prefix)
        .foreach(k => index.getOrElseUpdate(k, scala.collection.mutable.ArrayBuffer.empty) += i)
    }
    val candidates = scala.collection.mutable.HashSet.empty[(Int, Int)]
    index.valuesIterator.foreach { held =>
      for { x <- held.indices; y <- (x + 1) until held.size } candidates += ((held(x) min held(y), held(x) max held(y)))
    }
    candidates.iterator.flatMap { case (i, j) =>
      val (a, as) = venues(i)
      val (b, bs) = venues(j)
      val apart   = (cityOf(a) intersect cityOf(b)).isEmpty
      if (apart && shared(as, bs) >= CrossCityMinOverlap * math.max(as.length, bs.length) && !DistinctVenuePairs.contains(a, b))
        Some(if (a.displayName <= b.displayName) (a, b) else (b, a))
      else None
    }.toSet
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
