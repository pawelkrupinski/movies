package services.metrics

import models.{MovieRecord, Showtime, Source, SourceData}
import services.movies.{InMemoryMovieRepository, MovieRepository, StoredMovieRecord}

import java.time.{Clock, LocalDateTime, ZoneId}

/**
 * The corpus the worker's census specs ([[WorkerCorpusMetricsSpec]],
 * [[WorkerSourceFilmsMetricsSpec]], [[WorkerShowtimesMetricsSpec]],
 * [[WorkerCorpusScanSpec]]) share: one fixed "now", one row/showtime builder and one
 * repository factory, so the film gauge and the showtime gauge are provably counting
 * the SAME rows (a film shown in a city there is N slots here).
 */
object CorpusMetricsFixtures {

  val warsaw: ZoneId = ZoneId.of("Europe/Warsaw")
  /** Fixed "now": 2026-06-08 12:00 Warsaw → tomorrow is 2026-06-09. */
  val now:    LocalDateTime = LocalDateTime.of(2026, 6, 8, 12, 0)
  val clock:  Clock         = Clock.fixed(now.atZone(warsaw).toInstant, warsaw)

  val today:    LocalDateTime = LocalDateTime.of(2026, 6, 8, 18, 0)
  val tomorrow: LocalDateTime = LocalDateTime.of(2026, 6, 9, 18, 0)
  /** Before now − 30 min, so it must drop out of every upcoming count. */
  val past:     LocalDateTime = LocalDateTime.of(2026, 6, 8, 9, 0)

  def slot(times: LocalDateTime*): SourceData =
    SourceData(title = Some("x"), showtimes = times.map(t => Showtime(t, bookingUrl = None)))

  /** tmdbId set → tmdbConcluded → readyToProject, matching what the projector writes. */
  def ready(cinema: Source, tmdb: Int, times: LocalDateTime*): MovieRecord =
    MovieRecord(tmdbId = Some(tmdb), data = Map(cinema -> slot(times*)))

  def row(title: String, record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(title, Some(2026), record)

  /** A read-only repository over these rows — the in-memory store production's cache
   *  tests already use, so the specs exercise the real `foreachRecord` contract. */
  def repositoryOf(rows: StoredMovieRecord*): MovieRepository =
    new InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record)))
}
