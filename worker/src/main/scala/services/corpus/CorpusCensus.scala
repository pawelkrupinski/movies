package services.corpus

import models.{CacheKey, StoredMovieRecord}

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.City
import play.api.Logging
import services.movies.{LatestTitleYear, MovieCache}
import services.titles.TitleNormalizer
import models.MovieRecord
import tools.{ThreadPools, ScheduledTick, TickRecorder, TickRun}

import java.time.Clock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * Every `movies`-census gauge the worker exposes ([[CensusGauges]]) — `WorkerCorpusMetrics`, `WorkerSourceFilmsMetrics`,
 * `WorkerShowtimesMetrics` and `WorkerSlotFanoutMetrics`, with each city's off-season verdict beside them
 * (`WorkerCitySeasonMetrics`) — kept film by film as the worker's cache changes, never
 * by reading the corpus.
 *
 * These were a 15-minute scan of `movies` with its showtimes stitched from `screenings` and its slots from
 * `movie_slots`: on the US a pass read every film, every venue slot and every screenings row four times an hour, and
 * cost 29% of the worker's CPU at its earlier 5-minute cadence (2026-09-30). The cache already holds every film, and
 * hears of every change — a scrape landing, the identity projection's writes, another process's write through the
 * change stream, a rehydrate. So the cache notes each film it changed (`MovieCache.onResident`, a map put under the
 * film's lock), each tick derives the noted films' parts ([[FilmCensus]], held by [[CensusParts]]) — reusing every slot it
 * holds unchanged — and keeps them:
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
 * `kinowo_worker_corpus_census_incomplete_total` instead. A cache that only holds the films written since boot is not a
 * smaller corpus, and as a gauge value the two are indistinguishable (2026-07-27: a decode bug failed every batch and
 * the censuses published 0 while the corpus sat intact).
 */
final class CorpusCensus(
  cache:           MovieCache,
  corpus:          Gauge,
  served:          Gauge,
  showtimes:       Gauge,
  widest:          Gauge,
  mayBeDark:       Gauge,
  countryCode:     String,
  cities:          Seq[City],
  clock:           Clock,
  metrics:         CorpusCensusMetrics = CorpusCensusMetrics.noop,
  publishInterval: FiniteDuration    = CorpusCensus.DefaultPublishInterval,
  // Where each completed publish is reported (`ScheduledTickStale`): a dead census leaves its gauges flat.
  ticks:           TickRecorder      = TickRecorder.Unrecorded,
  threadPools:     ThreadPools
) extends tools.Stoppable with Logging {
  // What the cache noted, and each film's part: the specs note a film straight into it.
  private[corpus] val parts = {
    // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
    given LatestTitleYear = LatestTitleYear(clock)
    val normalizer = cache.normalizer
    new CensusParts[CacheKey, StoredMovieRecord](FilmCensus.of(_, normalizer, _), _.cleanTitle, cities, clock)
  }
  private val gauges = new CensusGauges(corpus, served, showtimes, widest, mayBeDark, countryCode, cities)
  @volatile private var seeded = false

  gauges.seeded()
  gauges.seasons(clock.instant())

  private val scheduler = threadPools.scheduler("corpus-census")

  /** The census as the films the cache holds now give it, against the clock now. */
  def reading(): CensusReading = parts.drainedReading()._1

  /** Publish [[reading]] — or, while the cache does not hold the whole corpus, nothing, counted as a miss. The
   *  off-season verdicts are published either way: they read no corpus. [[TickRun.Incomplete]] for a miss (its gauges
   *  keep old values), or a reading with a film it could not count. */
  def publish(): TickRun = {
    gauges.seasons(clock.instant())
    if (!seeded || !cache.hydrated) {
      metrics.recordIncompleteSample()
      logger.warn("corpus-census: the cache does not hold the whole corpus yet — census gauges keep their previous " +
        "values rather than publishing a partial count as if the corpus had shrunk.")
      TickRun.Incomplete
    } else {
      val (now, whole) = parts.drainedReading()
      gauges.published(now)
      TickRun.completedWhen(whole)
    }
  }

  private def publishQuietly(): Unit = {
    Try(publish()).recover { case e => logger.warn(s"corpus-census publish failed: ${e.getMessage}") }
    ()
  }

  /** Count every film the cache holds, and follow its changes from now on. */
  def seed(): Unit = {
    cache.onResident(parts.held)
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
    ticks.started("corpus-census", publishInterval, publishInterval)
    scheduler.scheduleAtFixedRate(() => ScheduledTick.recorded("corpus-census", publishInterval, logger, ticks)(publish()),
      publishInterval.toSeconds, publishInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object CorpusCensus {
  /** Every 5 minutes: a tick reads nothing, and the clock-bound gauges move by the minute (a showtime passing). */
  val DefaultPublishInterval: FiniteDuration = 5.minutes

  val IncompleteMetricName = "kinowo_worker_corpus_census_incomplete_total"

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

  /** What each of [[CensusSubsets.StaticNames]] counts, in its order: the bits of [[FilmCensus.subsets]]. */
  private[corpus] val StaticSubsets: IndexedSeq[MovieRecord => LatestTitleYear ?=> Boolean] = {
    val counts: IndexedSeq[MovieRecord => LatestTitleYear ?=> Boolean] = IndexedSeq(
      _ => true,
      r => r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined,
      _.tmdbId.isDefined,
      _.imdbId.isDefined,
      _.imdbRating.isDefined,
      _.rottenTomatoes.isDefined,
      _.metascore.isDefined,
      _.filmwebRating.isDefined,
      r => CinemaCorroboration.contradicts(r))
    require(counts.size == CensusSubsets.StaticNames.size, "a static subset without its name, or a name without its count")
    counts
  }

  /** The census of `rows` against `clock` — what a census holding exactly these films reads. */
  def read(rows: Iterable[StoredMovieRecord], cities: Seq[City], clock: Clock, normalizer: TitleNormalizer)(using LatestTitleYear): CensusReading = {
    val parts = rows.map(FilmCensus.of(_, normalizer, None)).toSeq
    CensusReading.tally(parts.iterator, CensusSubsets.counted(parts.map(_.subsets)), cities, clock)
  }
}
