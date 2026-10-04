package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, City, MovieRecord, Showtime, Source, SourceData, VenueClock}
import play.api.Logging
import services.movies.{CacheKey, CinemaCorroboration, MovieCache, ShowtimesDigest, StoredMovieRecord, TitleNormalizer}
import services.readmodel.ReadModelProjection
import tools.DaemonExecutors

import java.time.{Clock, LocalDateTime, ZoneId, ZoneOffset}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Every `movies`-census gauge the worker exposes — [[WorkerCorpusMetrics]], [[WorkerSourceFilmsMetrics]],
 * [[WorkerShowtimesMetrics]] and [[WorkerSlotFanoutMetrics]] — kept film by film as the worker's cache changes, never
 * by reading the corpus.
 *
 * These were a 15-minute scan of `movies` with its showtimes stitched from `screenings` and its slots from
 * `movie_slots`: on the US a pass read every film, every venue slot and every screenings row four times an hour, and
 * cost 29% of the worker's CPU at its earlier 5-minute cadence (2026-09-30). The cache already holds every film, and
 * hears of every change — a scrape landing, the identity projection's writes, another process's write through the
 * change stream, a rehydrate. So each film's part in the census ([[FilmCensus]]) is derived when the cache's copy of it
 * changes (`MovieCache.onResident`), and kept:
 *   - the time-free counts — the corpus subsets but `unresolved_with_showtimes` — as running sums, moved by each
 *     film's old and new part;
 *   - the rest — films served, upcoming showtimes, unresolved-yet-screening, the widest film — depend on the clock as
 *     much as on the corpus (a showtime stops being upcoming as the evening passes), so each tick counts them afresh
 *     over the parts held in memory: a few binary searches per venue slot, no read.
 *
 * The cache holds a film LEAN: each slot's showtime starts (`SourceData.showtimeStartMinutes`), not its showtimes.
 * Every gauge reads only starts, which is what a stitched film would give, but for one thing: the read model lists
 * each of a venue's showtimes ONCE, and a start cannot tell two equal showtimes from two screens showing the film at
 * the same minute. So a venue that lists one showtime twice — in one slot, or in two slots of one card — counts it
 * twice in `kinowo_worker_showtimes`. The scan this replaced counted it once.
 *
 * Honest about what it does not know, as the scan was: until the cache has read the whole corpus once (`hydrated`)
 * and every film it holds has been counted, a tick publishes NOTHING and counts the miss on
 * `kinowo_worker_corpus_scan_incomplete_total` instead. A cache that only holds the films written since boot is not a
 * smaller corpus, and as a gauge value the two are indistinguishable (2026-07-27: a decode bug failed every batch and
 * the censuses published 0 while the corpus sat intact).
 */
final class CorpusCensus(
  cache:           MovieCache,
  corpus:          Gauge,
  served:          Gauge,
  showtimes:       Gauge,
  widest:          Gauge,
  countryCode:     String,
  cities:          Seq[City],
  clock:           Clock,
  metrics:         CorpusScanMetrics = CorpusScanMetrics.noop,
  publishInterval: FiniteDuration    = CorpusCensus.DefaultPublishInterval
) extends services.Stoppable with Logging {
  import CorpusCensus._

  private val films  = new ConcurrentHashMap[CacheKey, FilmCensus]()
  private val static = new Array[Long](StaticSubsets.size)
  @volatile private var seeded = false

  // Every (city, scope) and city seeded at 0, so a city that empties reads as an explicit 0, not a vanished series.
  // The corpus subsets are not: a 0 there reads as an empty corpus, and every restart drew the coverage chart to 0
  // and back. Each appears with the first complete census instead.
  for (c <- cities) {
    WorkerSourceFilmsMetrics.Scope.all.foreach(scope => served.labelValues(countryCode, c.slug, scope).set(0.0))
    showtimes.labelValues(countryCode, c.slug).set(0.0)
  }
  widest.labelValues(countryCode).set(0.0)

  private val scheduler = DaemonExecutors.scheduler("corpus-census")

  /** Take the film `key` now holds (`None`: gone) in place of its last part. Called under the cache's lock for the
   *  key, so one key's calls never cross. */
  private[metrics] def held(key: CacheKey, film: Option[StoredMovieRecord]): Unit = {
    val prior = Option(films.get(key))
    val next  = film.map(FilmCensus.of(_, cache.normalizer, prior))
    next.fold(films.remove(key))(films.put(key, _))
    static.synchronized {
      prior.foreach(p => add(p.subsets, -1))
      next.foreach(n => add(n.subsets, +1))
    }
  }

  private def add(subsets: Int, by: Int): Unit =
    StaticSubsets.indices.foreach(i => if ((subsets & (1 << i)) != 0) static(i) += by)

  /** The census as the parts held now give it, against the clock now. */
  def reading(): Reading = tally(films.values.iterator.asScala, static.synchronized(static.toSeq), cities, clock)

  /** Publish [[reading]] — or, while the cache does not hold the whole corpus, nothing, counted as a miss. */
  def publish(): Unit =
    if (!seeded || !cache.hydrated) {
      metrics.recordIncompleteSample()
      logger.warn("corpus-census: the cache does not hold the whole corpus yet — census gauges keep their previous " +
        "values rather than publishing a partial count as if the corpus had shrunk.")
    } else {
      val now = reading()
      now.corpus.foreach { case (subset, n) => corpus.labelValues(countryCode, subset).set(n.toDouble) }
      for (c <- cities) {
        WorkerSourceFilmsMetrics.Scope.all.foreach(scope =>
          served.labelValues(countryCode, c.slug, scope).set(now.served.getOrElse((c.slug, scope), 0).toDouble))
        showtimes.labelValues(countryCode, c.slug).set(now.showtimes.getOrElse(c.slug, 0).toDouble)
      }
      widest.labelValues(countryCode).set(now.widest.toDouble)
    }

  private def publishQuietly(): Unit = {
    Try(publish()).recover { case e => logger.warn(s"corpus-census publish failed: ${e.getMessage}") }
    ()
  }

  /** Count every film the cache holds, and follow its changes from now on. */
  def seed(): Unit = {
    cache.onResident(held)
    seeded = true
  }

  /** [[seed]], then publish at once and on every
   *  [[publishInterval]]. The first count — a derivation per film, no read — runs on the census's own thread, never
   *  the boot's. */
  def start(): Unit = {
    scheduler.execute { () =>
      Try(seed()).recover { case e => logger.warn(s"corpus-census could not count the cache: ${e.getMessage}") }
      publishQuietly()
    }
    scheduler.scheduleAtFixedRate(() => publishQuietly(), publishInterval.toSeconds, publishInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object CorpusCensus {
  /** Every 5 minutes: a tick reads nothing, and the clock-bound gauges move by the minute (a showtime passing). */
  val DefaultPublishInterval: FiniteDuration = 5.minutes

  val IncompleteMetricName = "kinowo_worker_corpus_scan_incomplete_total"

  /** Register the shared counter every country's census increments (leading `country` label). */
  def incompleteCounter(registry: PrometheusRegistry): Counter =
    Counter.builder()
      .name(IncompleteMetricName)
      .help("Corpus census ticks that published nothing because the worker's cache did not hold the whole `movies` " +
        "corpus (no corpus read has completed since boot), by country. The census gauges (kinowo_worker_corpus_movies / " +
        "_showtimes / _movies_served / _film_widest_slots) deliberately publish NOTHING on such a tick — they hold " +
        "their last complete values — so a rising rate here is the only sign those gauges have gone stale.")
      .labelNames("country")
      .register(registry)

  /** The corpus subsets that do not move with the clock, in the order of their bits in [[FilmCensus.subsets]]. */
  private[metrics] val StaticSubsets: IndexedSeq[(String, MovieRecord => Boolean)] = {
    import WorkerCorpusMetrics.Subset._
    IndexedSeq(
      Total         -> (_ => true),
      WithAnyRating -> (r => r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined),
      WithTmdbId    -> (_.tmdbId.isDefined),
      WithImdbId    -> (_.imdbId.isDefined),
      ImdbRating    -> (_.imdbRating.isDefined),
      RtRating      -> (_.rottenTomatoes.isDefined),
      McRating      -> (_.metascore.isDefined),
      FwRating      -> (_.filmwebRating.isDefined),
      Misresolved   -> (r => CinemaCorroboration.contradicts(r)))
  }

  /** The census at one instant: each corpus subset, films served by (city, scope), upcoming showtimes by city, and
   *  the widest film's cinema slots. */
  final case class Reading(corpus: Seq[(String, Int)], served: Map[(String, String), Int], showtimes: Map[String, Int], widest: Int) {
    def subset(name: String): Int = corpus.collectFirst { case (`name`, n) => n }.getOrElse(0)
  }

  /** The census of `rows` against `clock` — what a census holding exactly these films reads. */
  def read(rows: Iterable[StoredMovieRecord], cities: Seq[City], clock: Clock, normalizer: TitleNormalizer): Reading = {
    val parts  = rows.map(FilmCensus.of(_, normalizer, None)).toSeq
    val static = StaticSubsets.indices.map(i => parts.count(p => (p.subsets & (1 << i)) != 0).toLong)
    tally(parts.iterator, static, cities, clock)
  }

  /** One city's window on a tick: a start after `upcomingAfter` is upcoming ([[Showtime.isUpcoming]]); one in
   *  `[tomorrowFrom, tomorrowUntil)` is on the city's local tomorrow. All in [[ShowtimesDigest.startMinute]]s. */
  private final case class Window(upcomingAfter: Int, tomorrowFrom: Int, tomorrowUntil: Int)

  /** The minute after which a start at `now` (venue-local) is still upcoming: a showtime is until [[Showtime.Grace]]
   *  past its start. Exact for starts on the minute, which every showtime a venue lists is. */
  private def upcomingAfter(now: LocalDateTime): Int = ShowtimesDigest.startMinute(now.minus(Showtime.Grace))

  private def tally(films: Iterator[FilmCensus], static: Seq[Long], cities: Seq[City], clock: Clock): Reading = {
    val instant    = clock.instant()
    val venueClock = new VenueClock(Clock.fixed(instant, ZoneOffset.UTC))
    val windows    = cities.map { c =>
      val now      = venueClock.nowIn(c)
      val tomorrow = now.toLocalDate.plusDays(1)
      c.slug -> Window(upcomingAfter(now), ShowtimesDigest.startMinute(tomorrow.atStartOfDay),
        ShowtimesDigest.startMinute(tomorrow.plusDays(1).atStartOfDay))
    }.toMap
    val zoneCutoffs = scala.collection.mutable.HashMap.empty[ZoneId, Int]
    def zoneCutoff(zone: ZoneId) = zoneCutoffs.getOrElseUpdate(zone, upcomingAfter(venueClock.now(zone)))

    val served    = scala.collection.mutable.HashMap.empty[(String, String), Int].withDefaultValue(0)
    val upcoming  = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    var unresolvedScreening = 0
    var widest = 0
    films.foreach { film =>
      if (film.cinemaSlots > widest) widest = film.cinemaSlots
      if (film.ready) film.cards.foreach { card =>
        val cardCities = scala.collection.mutable.HashMap.empty[String, (Boolean, Boolean)]
        card.foreach { slot =>
          slot.city.flatMap(slug => windows.get(slug).map(slug -> _)).foreach { case (slug, window) =>
            val ahead = slot.startsAfter(window.upcomingAfter)
            upcoming(slug) += ahead
            val (anyAhead, anyTomorrow) = cardCities.getOrElse(slug, (false, false))
            cardCities(slug) = (anyAhead || ahead > 0, anyTomorrow || slot.startsWithin(window.tomorrowFrom, window.tomorrowUntil))
          }
        }
        cardCities.foreach { case (slug, (anyAhead, anyTomorrow)) =>
          if (anyAhead) served((slug, WorkerSourceFilmsMetrics.Scope.All)) += 1
          if (anyTomorrow) served((slug, WorkerSourceFilmsMetrics.Scope.Tomorrow)) += 1
        }
      }
      else if (film.screenable.exists(slot => slot.startsAfter(zoneCutoff(slot.zone)) > 0)) unresolvedScreening += 1
    }
    val corpus = StaticSubsets.indices.map(i => StaticSubsets(i)._1 -> static(i).toInt) :+
      (WorkerCorpusMetrics.Subset.UnresolvedWithShowtimes -> unresolvedScreening)
    Reading(WorkerCorpusMetrics.Subset.all.map(name => name -> corpus.collectFirst { case (`name`, n) => n }.getOrElse(0)),
      served.toMap, upcoming.toMap, widest)
  }
}

/** One film's part in the [[CorpusCensus]], from the film as the cache holds it.
 *
 *  `subsets` has a bit per [[CorpusCensus.StaticSubsets]] the film belongs to. A film ready to project
 *  (`readyToProject`, the projector's own gate) files its screening venue slots under `cards` — one card per
 *  display-title group, as [[ReadModelProjection.partition]] splits it — and one that is not keeps them in
 *  `screenable`, for `unresolved_with_showtimes`. A venue slot with no showtime start is in neither: it screens nothing. */
final class FilmCensus private (val subsets: Int, val ready: Boolean, val cinemaSlots: Int,
                                private val bySource: Map[Source, SlotCensus], val cards: Seq[Seq[SlotCensus]],
                                val screenable: Seq[SlotCensus]) {
  /** The part of the slot at `source`, if the film holds one there. */
  private[metrics] def partAt(source: Source): Option[SlotCensus] = bySource.get(source)
}

object FilmCensus {
  /** `stored`'s part. A slot the cache holds as the very object it held for `prior` keeps its derived part: a scrape
   *  landing moves one slot of a film, and a film can hold thousands. */
  def of(stored: StoredMovieRecord, normalizer: TitleNormalizer, prior: Option[FilmCensus]): FilmCensus = {
    val record   = stored.record
    val subsets  = CorpusCensus.StaticSubsets.indices.foldLeft(0)((bits, i) =>
      if (CorpusCensus.StaticSubsets(i)._2(record)) bits | (1 << i) else bits)
    val bySource = record.data.iterator.flatMap { case (source, slot) =>
      Source.cinemaOf(source).map { cinema =>
        source -> prior.flatMap(_.bySource.get(source)).filter(_.slot eq slot).getOrElse(SlotCensus(slot, cinema, normalizer))
      }
    }.toMap
    val screening = bySource.valuesIterator.filter(_.starts.nonEmpty).toSeq
    val ready     = record.readyToProject
    val cards     =
      if (!ready) Nil
      else {
        val anchor = ReadModelProjection.anchorKeyOf(stored.asReadBack(normalizer), normalizer)
        screening.filter(_.city.isDefined).groupBy(_.titleKey.getOrElse(anchor)).values.toSeq
      }
    new FilmCensus(subsets, ready, bySource.size, bySource, cards, if (ready) Nil else screening)
  }
}

/** One venue slot's part: its title group ([[ReadModelProjection.titleKeyOf]]), its city, the zone its venue keeps
 *  time in, and its showtime starts — the lean slot's own array. */
final class SlotCensus private (val slot: SourceData, val titleKey: Option[String], val city: Option[String],
                                val zone: ZoneId, val starts: IArray[Int]) {
  /** How many starts fall after `minute`. */
  def startsAfter(minute: Int): Int = starts.length - firstAbove(minute)

  /** Whether a start falls in `[from, until)`. */
  def startsWithin(from: Int, until: Int): Boolean = {
    val i = firstAbove(from - 1)
    i < starts.length && starts(i) < until
  }

  /** The index of the first start above `minute`; the starts are ascending. */
  private def firstAbove(minute: Int): Int = {
    var low  = 0
    var high = starts.length
    while (low < high) { val mid = (low + high) >>> 1; if (starts(mid) <= minute) low = mid + 1 else high = mid }
    low
  }
}

object SlotCensus {
  def apply(slot: SourceData, cinema: Cinema, normalizer: TitleNormalizer): SlotCensus =
    new SlotCensus(slot, ReadModelProjection.titleKeyOf(slot, normalizer), City.forCinema(cinema).map(_.slug),
      VenueClock.zoneOf(cinema, ZoneOffset.UTC), ShowtimesDigest.startMinutes(slot))
}

/** Where [[CorpusCensus]] reports a tick that could not count the whole corpus. A trait so the census stays testable
 *  without a Prometheus registry. */
trait CorpusScanMetrics {
  def recordIncompleteSample(): Unit
}

object CorpusScanMetrics {
  val noop: CorpusScanMetrics = new CorpusScanMetrics { def recordIncompleteSample(): Unit = () }

  /** Binds one country's slice of the shared counter, materializing the series at 0 up front: a healthy country must
   *  be an explicit 0, not an absent series, or an alert on it has nothing to compare against. */
  def prometheus(counter: Counter, countryCode: String): CorpusScanMetrics = {
    counter.labelValues(countryCode)
    new CorpusScanMetrics {
      def recordIncompleteSample(): Unit = counter.labelValues(countryCode).inc()
    }
  }
}
