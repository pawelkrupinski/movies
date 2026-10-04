package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import models.{City, MovieRecord}
import play.api.Logging
import services.movies.StoredMovieRecord
import tools.Stopwatch

import java.time.{Clock, ZoneOffset}
import scala.concurrent.duration._

/**
 * THE REFERENCE the incremental [[CorpusCensus]] is held to: the full corpus pass that fed the census gauges until
 * 2026-10-04, kept as it ran — every stitched film, showtimes and all, fanned out to one collector per gauge family,
 * each counting by the definition its gauge documents. `CorpusCensusEquivalenceSpec` applies writes and deletes to a
 * cache and asserts the census reads, after each, what this pass reads over the store.
 *
 * Failure semantics: a scan that reports `false` publishes NOTHING — a partial census is fewer rows read, not a
 * smaller corpus.
 */
object WorkerCorpusScan extends Logging {
  /** One census pass over the rows `scan` delivers: every row fanned out to all `collectors`, then each publishes. An
   *  incomplete scan publishes NOTHING. */
  def pass(collectors: Seq[CorpusMetricsCollector], stopwatch: Stopwatch = Stopwatch.System)
          (scan: (StoredMovieRecord => Unit) => tools.ScanOutcome): Pass = {
    val samplers = collectors.map(c => (nameOf(c), c.startSample(), stopwatch.total()))
    val started  = stopwatch.start()
    val complete = scan { stored =>
      val row = new CorpusRow(stored)
      samplers.foreach { case (_, sampler, time) => time(sampler.accept(row)) }
    }
    samplers.foreach { case (_, sampler, time) => time(sampler.publish(complete.isComplete)) }
    Pass(started.elapsed, samplers.map { case (name, _, time) => name -> time.elapsed })
  }

  /** [[pass]] over a store's whole corpus, stitched. */
  def over(repository: services.movies.MovieRepository, collectors: Seq[CorpusMetricsCollector]): Pass =
    pass(collectors)(repository.foreachRecord)

  /** One pass's time, and each collector's share of it. */
  final case class Pass(total: FiniteDuration, byCollector: Seq[(String, FiniteDuration)])

  private def nameOf(collector: CorpusMetricsCollector): String =
    Option(collector.getClass.getSimpleName).filter(_.nonEmpty).getOrElse(collector.getClass.getName).stripSuffix("$")
}

/** A gauge family populated by censusing the whole corpus: an accumulator per pass of [[WorkerCorpusScan]]. */
trait CorpusMetricsCollector {
  def startSample(): CorpusRowSampler
}

/** One collector's accumulator over a single corpus pass. */
trait CorpusRowSampler {
  def accept(row: CorpusRow): Unit
  /** `scanComplete` is `false` when the scan stopped early: the rows seen are NOT the whole corpus. */
  def publish(scanComplete: Boolean): Unit
}

/** One corpus row as the scan hands it to every collector: the stored row, and its venues as the read model
 *  partitions them, derived at most once per row. `None` for a row not ready to project, or one that fails to. */
final class CorpusRow(val stored: StoredMovieRecord) {
  private var partitioned: Option[(services.movies.TitleNormalizer, Option[Seq[Seq[services.readmodel.ReadModelProjection.VenueScreening]]])] = None

  def venues(normalizer: services.movies.TitleNormalizer): Option[Seq[Seq[services.readmodel.ReadModelProjection.VenueScreening]]] =
    partitioned match {
      case Some((by, cards)) if by eq normalizer => cards
      case _ =>
        val partition = Option.when(stored.record.readyToProject)(
          scala.util.Try(services.readmodel.ReadModelProjection.partition(stored, normalizer)).toOption).flatten
        val cards = partition.flatMap(p => scala.util.Try(p.venuesAll).toOption)
        partitioned = Some((normalizer, cards))
        cards
    }
}

/** `kinowo_worker_corpus_movies{subset}` by its definition: every subset counted over stitched rows. */
final class ReferenceCorpusMetrics(corpus: Gauge, countryCode: String, clock: Clock) extends CorpusMetricsCollector {
  import ReferenceCorpusMetrics._
  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private val now    = Clock.fixed(clock.instant(), ZoneOffset.UTC)
    private var counts = CorpusCounts.empty
    def accept(row: CorpusRow): Unit = counts = counts.add(row.stored.record, now)
    def publish(scanComplete: Boolean): Unit =
      if (scanComplete)
        counts.bySubset.foreach { case (subset, value) => corpus.labelValues(countryCode, subset).set(value.toDouble) }
  }
}

object ReferenceCorpusMetrics {
  import WorkerCorpusMetrics.Subset

  case class CorpusCounts(
    total: Int, withAnyRating: Int, withTmdbId: Int, withImdbId: Int,
    imdbRating: Int, rtRating: Int, mcRating: Int, fwRating: Int, misresolved: Int,
    unresolvedWithShowtimes: Int
  ) {
    def add(r: MovieRecord, now: Clock): CorpusCounts = CorpusCounts(
      total         = total + 1,
      withAnyRating = withAnyRating + bool(hasAnyRating(r)),
      withTmdbId    = withTmdbId + bool(r.tmdbId.isDefined),
      withImdbId    = withImdbId + bool(r.imdbId.isDefined),
      imdbRating    = imdbRating + bool(r.imdbRating.isDefined),
      rtRating      = rtRating + bool(r.rottenTomatoes.isDefined),
      mcRating      = mcRating + bool(r.metascore.isDefined),
      fwRating      = fwRating + bool(r.filmwebRating.isDefined),
      misresolved   = misresolved + bool(services.movies.CinemaCorroboration.contradicts(r)),
      unresolvedWithShowtimes = unresolvedWithShowtimes + bool(unresolvedYetScreening(r, now))
    )

    def bySubset: Seq[(String, Int)] = Seq(
      Subset.Total -> total, Subset.WithAnyRating -> withAnyRating,
      Subset.WithTmdbId -> withTmdbId, Subset.WithImdbId -> withImdbId,
      Subset.ImdbRating -> imdbRating, Subset.RtRating -> rtRating,
      Subset.McRating -> mcRating, Subset.FwRating -> fwRating,
      Subset.Misresolved -> misresolved,
      Subset.UnresolvedWithShowtimes -> unresolvedWithShowtimes
    )
  }

  object CorpusCounts {
    val empty: CorpusCounts = CorpusCounts(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  def hasAnyRating(r: MovieRecord): Boolean =
    r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined

  /** Not ready to project, with an upcoming showtime on a cinema slot — each slot judged in its venue's own zone. */
  def unresolvedYetScreening(r: MovieRecord, now: Clock): Boolean =
    !r.readyToProject && r.cinemaSlots.exists { case (source, slot) =>
      models.Source.cinemaOf(source).exists { cinema =>
        val local = new models.VenueClock(now).nowAt(cinema, now.getZone)
        slot.showtimes.exists(_.isUpcoming(local))
      }
    }

  private def bool(b: Boolean): Int = if (b) 1 else 0
}

/** `kinowo_worker_movies_served{city,scope}` by its definition: each ready row projected exactly as the read model
 *  does, a card counted once per city it has an upcoming showtime in (`all`) or a showtime on that city's tomorrow. */
final class ReferenceSourceFilms(served: Gauge, countryCode: String, clock: Clock, cities: Seq[City] = City.all,
                                 normalizer: services.movies.TitleNormalizer) extends CorpusMetricsCollector {
  import WorkerSourceFilmsMetrics.Scope
  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private val clocks = cities.map { c =>
      val now = new models.VenueClock(clock).nowIn(c); c.slug -> (now, now.toLocalDate.plusDays(1)) }.toMap
    private val acc    = scala.collection.mutable.Map.empty[(String, String), Int].withDefaultValue(0)

    def accept(row: CorpusRow): Unit =
      row.venues(normalizer).foreach(_.foreach { venues =>
        venues.groupBy(_.citySlug).toSeq.flatMap { case (citySlug, inCity) =>
          clocks.get(citySlug).toSeq.flatMap { case (now, tomorrow) =>
            val showtimes = inCity.iterator.flatMap(_.showtimes).toSeq
            Seq(
              Option.when(showtimes.exists(_.isUpcoming(now)))(citySlug -> Scope.All),
              Option.when(showtimes.exists(_.dateTime.toLocalDate == tomorrow))(citySlug -> Scope.Tomorrow)
            ).flatten
          }
        }.toSet.foreach(key => acc(key) += 1)
      })

    def publish(scanComplete: Boolean): Unit = if (scanComplete)
      for (c <- cities; scope <- Scope.all)
        served.labelValues(countryCode, c.slug, scope).set(acc.getOrElse((c.slug, scope), 0).toDouble)
  }
}

/** `kinowo_worker_showtimes{city}` by its definition: each ready row projected exactly as the read model does, and
 *  each venue's distinct upcoming showtimes summed into its city. */
final class ReferenceShowtimes(showtimes: Gauge, countryCode: String, clock: Clock, cities: Seq[City] = City.all,
                               normalizer: services.movies.TitleNormalizer) extends CorpusMetricsCollector {
  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private val nowIn = { val venueClock = new models.VenueClock(clock); cities.map(c => c.slug -> venueClock.nowIn(c)).toMap }
    private val acc   = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)

    def accept(row: CorpusRow): Unit =
      row.venues(normalizer).foreach(_.foreach { venues =>
        venues.groupBy(_.citySlug).foreach { case (citySlug, inCity) =>
          nowIn.get(citySlug).foreach { now =>
            val upcoming = inCity.iterator.flatMap(_.showtimes).count(_.isUpcoming(now))
            if (upcoming > 0) acc(citySlug) += upcoming
          }
        }
      })

    def publish(scanComplete: Boolean): Unit = if (scanComplete)
      for (c <- cities) showtimes.labelValues(countryCode, c.slug).set(acc.getOrElse(c.slug, 0).toDouble)
  }
}

/** `kinowo_worker_film_widest_slots` by its definition: the most cinema slots any one film carries. */
final class ReferenceSlotFanout(widest: Gauge, countryCode: String) extends CorpusMetricsCollector {
  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private var max = 0
    def accept(row: CorpusRow): Unit = max = math.max(max, row.stored.record.cinemaSlotCount)
    def publish(scanComplete: Boolean): Unit = if (scanComplete) widest.labelValues(countryCode).set(max.toDouble)
  }
}

/** The four reference collectors over one registry's gauges, as the worker wired them. */
object ReferenceCensus {
  def collectors(registry: io.prometheus.metrics.model.registry.PrometheusRegistry, countryCode: String, cities: Seq[City],
                 clock: Clock, normalizer: services.movies.TitleNormalizer): Seq[CorpusMetricsCollector] = Seq(
    new ReferenceCorpusMetrics(WorkerCorpusMetrics.gauge(registry), countryCode, clock),
    new ReferenceSourceFilms(WorkerSourceFilmsMetrics.gauge(registry), countryCode, clock, cities, normalizer),
    new ReferenceShowtimes(WorkerShowtimesMetrics.gauge(registry), countryCode, clock, cities, normalizer),
    new ReferenceSlotFanout(WorkerSlotFanoutMetrics.gauge(registry), countryCode))
}
