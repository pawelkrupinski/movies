package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoMuranow, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord
import services.resolution.TmdbAttempt
import tools.Eventually

import java.time.{Instant, LocalDateTime}

/**
 * A row that goes unready and comes back ready gets its card back from the change stream,
 * at once — not at the next 30-minute prune.
 *
 * Pinned while diagnosing the 2026-09-23 serving gap (`UnresolvedTmdbReaper` cleared a
 * no-match row's `tmdbAttempt`, the projector retired its card as `stream-row-unready`, and
 * the prune's "projected N ready row(s) missing a card" looked like the only way back). The
 * stream half was suspected of dropping the ready transition; this replays ready → unready →
 * ready against real Mongo with the real projector on the real cursor — the writes spaced
 * out, and back-to-back as the ~1s resolve lands them — and the card comes back both ways.
 * Prod agrees: 441 unready retirements in PL/UK/DE over 24h against 9 prune heals. The gap
 * was the un-conclusion itself, fixed at `MovieService.retryResolve` (see
 * `RetryResolveKeepsRowReadySpec`); this spec keeps the recovery half honest, since the
 * readiness gate still retires cards for every other reason a row can lose it.
 *
 * Requires MONGODB_URI.
 */
class UnreadyRoundTripProjectionIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val year = Some(2026)

  private def roundTrip(title: String, settleBetween: Boolean): Unit =
    ProjectedMongoCorpus.withCorpus(mongoTarget, "unready_round_trip") { corpus =>
      import corpus._
      val id         = StoredMovieRecord.keyFor(title, year, titleNormalizer)
      val when       = LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

      def concluded(at: Instant) = MovieRecord(
        tmdbAttempt = Some(TmdbAttempt("fingerprint", at)),
        data = Map[Source, SourceData](KinoMuranow -> SourceData(title = Some(title), showtimes = Seq(Showtime(when, None)))))
      def served: Boolean =
        readModel.findAllMovieIds().contains(id) && readModel.findAllScreenings().exists(_.filmId == id)

      val projecting = repository.watchUpserts(projector.onMovieUpsert)
      projecting should not be empty
      try {
        val miss = concluded(Instant.parse("2026-09-22T10:00:00Z"))
        repository.upsert(title, year, miss)
        withClue("the no-match row never reached the read model, so nothing below tests what it claims: ") {
          Eventually.poll(30000)(served) shouldBe true
        }

        // An un-conclusion (what the re-try used to write): the row is unready.
        repository.upsert(title, year, miss.copy(tmdbAttempt = None))
        if (settleBetween) withClue("the unready row's card was never retired: ") {
          Eventually.poll(30000)(!served) shouldBe true
        }

        // TMDB: no match again — the row is ready in Mongo once more.
        repository.upsert(title, year, concluded(Instant.parse("2026-09-23T10:00:00Z")))
        withClue("the row is ready again in Mongo but its card was not re-projected by the change stream: ") {
          Eventually.poll(30000)(served) shouldBe true
        }
      } finally projecting.foreach(_.close())
    }

  "a row that goes unready and comes back ready" should "be served again once the card was retired" in
    roundTrip("__unready-round-trip-spaced__", settleBetween = true)

  it should "be served again when the ready write follows the unready one immediately" in
    roundTrip("__unready-round-trip-burst__", settleBetween = false)
}
