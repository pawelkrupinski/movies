package services.metrics

import services.movies.SingleCountryNormalizer

import models.{Helios, HeliosMagnolia, KinoApollo, MovieRecord, Rialto, Showtime, Source, SourceData}
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
    StoredMovieRecord.synthesised(title, Some(2026), record, services.movies.SingleCountryNormalizer.titleNormalizer)

  /** The upcoming-screenings corpus the films and showtimes gauges both count (mirroring
   *  `WebMovieMetricsSpec`, with tmdbId set so every row is ready). Poznań: two films
   *  with an upcoming slot — 3 slots, 1 film tomorrow — plus a past-only film that drops
   *  out; Wrocław: one film, one slot, tomorrow. */
  val upcomingCorpus: Seq[StoredMovieRecord] = Seq(
    row("Today And Tomorrow", ready(Helios,         1, today, tomorrow)),
    row("Today Only",         ready(KinoApollo,     2, today)),
    row("Past Only",          ready(Rialto,         3, past)),
    row("Wroclaw Tomorrow",   ready(HeliosMagnolia, 4, tomorrow)),
  )

  /** Scraped but unresolved — no tmdbId, no tmdbNoMatch — and playing tomorrow in
   *  Poznań: the projector holds it back, so no source gauge may count it. */
  val pendingInPoznan: StoredMovieRecord = {
    val pending = MovieRecord(data = Map[Source, SourceData](Helios -> slot(tomorrow)))
    require(!pending.readyToProject, "the pending row must fail the projector's gate")
    row("Pending", pending)
  }

  /** A read-only repository over these rows — the in-memory store production's cache
   *  tests already use, so the specs exercise the real `foreachRecord` contract. */
  def repositoryOf(rows: StoredMovieRecord*): MovieRepository =
    new InMemoryMovieRepository(rows.map(r => (r.title, r.year, r.record)), normalizer = SingleCountryNormalizer.titleNormalizer)
}
