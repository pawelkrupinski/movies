package integration

import services.movies.ListedShowtimes

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoLuna, KinoMuranow, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord
import tools.Eventually

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger

/**
 * A `movie_slots` row written AFTER the film's last projection — with nothing about the film
 * moving in `movies` or `screenings` — must still reach the read model.
 *
 * THE PROD SHAPE (measured 2026-09-07). The projector re-projects a film when its `movies`
 * document or one of its `screenings` rows changes, and NOTHING watched `movie_slots`. For
 * `doubleindemnity|1944` at Palace Cinema Kent the rows landed screenings-first (08-29 13:26),
 * then movies (13:55), then the slot (08-30 08:41). The projection needs the slot to emit that
 * venue's `web_screenings` row — the showtime stitch keys off the slot map, so a screenings row
 * with no slot twin is invisible — and a repertory film's rows never change again. 63 such
 * (film, venue) pairs in the UK, 33 in PL, every one of them a venue missing from the site.
 *
 * This replays that order against real Mongo with the real projector on the real cursor, and
 * asserts the venue's row appears. Before the third cursor it never does: the spec times out.
 *
 * Its own database: the projector reads and writes `web_*` whole, and a co-running spec's rows
 * would be indistinguishable from this one's. Requires MONGODB_URI; skips otherwise.
 */
class SlotsWatchProjectionIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val Tmdb  = 424242
  private val title = "__slots-watch-sentinel__"
  private val year  = Some(2026)

  "a movie_slots write with no movies or screenings change" should
    "reach the projector and emit the venue's web_screenings row" in {
    ProjectedMongoCorpus.withCorpus(mongoTarget, "slots_watch") { corpus =>
      import corpus._
      val id         = StoredMovieRecord.keyFor(title, year, titleNormalizer)
      val when       = LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

      def venueRows(venue: Source): Seq[String] =
        readModel.findAllScreenings().filter(s => s.filmId == id && s.cinema == venue.displayName).map(_._id)

      // The projector on the repository's shared cursor — the production subscription
      // (`ReadModelProjector.start` does exactly this) without its scheduled sweeps. The second
      // listener only counts deliveries, so liveness is established by DELIVERY, not by a nap.
      val dispatched = new AtomicInteger(0)
      val projecting = repository.watchUpserts(projector.onMovieUpsert)
      val counting   = repository.watchChanges(r => if (r.record.tmdbId.contains(Tmdb)) dispatched.incrementAndGet(), _ => ())
      projecting should not be empty
      try {
        // The film at ONE venue, projected — the state every one of the prod pairs was in.
        repository.upsert(title, year, MovieRecord(tmdbId = Some(Tmdb), data = Map[Source, SourceData](
          KinoMuranow -> SourceData(title = Some(title), showtimes = Seq(Showtime(when, None))))))
        withClue("the film's first venue never reached the read model, so nothing below tests what it claims: ") {
          Eventually.poll(30000)(venueRows(KinoMuranow).nonEmpty) shouldBe true
        }

        // Prod order, step one: the second venue's SCREENINGS row lands first. The screenings
        // cursor rings, the film is re-read — and the row has no slot to stitch onto, so the
        // venue stays invisible. That is correct, and asserted so the final assertion can only be
        // satisfied by the slot write itself.
        val seenBefore = dispatched.get()
        screenings.upsertSlot(id, KinoLuna.displayName, ListedShowtimes(Seq(Showtime(when.plusHours(1), None)), None))
        withClue("the screenings cursor never delivered the second venue's row: ") {
          Eventually.poll(30000)(dispatched.get() > seenBefore) shouldBe true
        }
        venueRows(KinoLuna) shouldBe empty

        // Step two, THE DEFECT: the venue's slot arrives last, and nothing else about the film
        // moves — no `movies` write, no `screenings` write, and a repertory film never writes
        // either again. Only a cursor on `movie_slots` can carry this to the projector.
        slots.upsertSlot(id, KinoLuna.displayName, SourceData(title = Some(title)))
        withClue(s"the slot row for ${KinoLuna.displayName} landed after the film's last projection and " +
                 "nothing re-projected the film, so the venue never reaches the site: ") {
          Eventually.poll(30000)(venueRows(KinoLuna).nonEmpty) shouldBe true
        }
      } finally { counting.foreach(_.close()); projecting.foreach(_.close()) }
    }
  }

  // THE SECOND PASS IS A NO-OP. A film the projector has already written, re-landed exactly as
  // it is and swept again, must cost nothing: no document written, no read-model row touched,
  // and no heal re-projecting it to find nothing to write. The film has a SPENT slot — a venue
  // whose showtimes are all gone, which the sweep's slots-only view cannot tell from a venue
  // whose row is missing. That is the shape that re-projected ~333 Polish rows every 30
  // minutes (dfe62a96c) — against Mongo only, because the in-memory repository stitched the
  // showtimes back in and made every sweep look convergent.
  "a projected film" should "cost nothing on a second pass that brings nothing new" in {
    ProjectedMongoCorpus.withCorpus(mongoTarget, "projection_fixpoint") { corpus =>
      import corpus.{databaseName, readModel, repository}
      val country   = models.Country.Poland
      val metrics   = services.metrics.WorkerMetrics.singleCountry(country, poolSize = settings.WorkerPoolSize(1))
      val projector = new services.readmodel.ReadModelProjector(repository, readModel, readModel, metrics.taskMetricsFor(country), clock = tools.SpecClock.Pinned)
      val id        = StoredMovieRecord.keyFor(fixpointTitle, year, titleNormalizer)
      val projecting = repository.watchUpserts(projector.onMovieUpsert)
      projecting should not be empty
      try {
        repository.upsert(fixpointTitle, year, MovieRecord(tmdbId = Some(FixpointTmdb), data = Map[Source, SourceData](
          KinoMuranow -> SourceData(title = Some(fixpointTitle), showtimes = Seq(Showtime(FarFuture, None))),
          KinoLuna    -> SourceData(title = Some(fixpointTitle), showtimes = Nil))))
        withClue("the film never reached the read model, so there is no second pass to measure: ") {
          Eventually.poll(30000)(readModel.findAllScreenings().exists(_.filmId == id)) shouldBe true
        }
        // The film's three writes reach the projector on three cursors, asynchronously. Let the
        // last of them land before counting, or a late first-pass projection reads as churn.
        awaitQuiet(metrics.registry)
        projector.pruneOrphans()   // the FIRST sweep may legitimately look

        val oplog = new tools.OplogWrites(mongoTarget.uri.value, databaseName)
        try new tools.ChurnLedger()
          .registry(metrics.registry, tools.FixpointPass.WorkFamilies, tools.FixpointPass.isWork)
          .counter(s"oplog writes to $databaseName")(oplog.count())
          .assertNoChurn("re-landing a projected film unchanged and sweeping again") {
            repository.findAll().foreach(film => repository.upsert(film.id, film.title, film.year, film.record))
            projector.pruneOrphans()
          }
        finally oplog.close()
      } finally projecting.foreach(_.close())
    }
  }

  private val fixpointTitle = "__projection-fixpoint-sentinel__"
  private val FixpointTmdb  = 424243
  // Fixed rather than read off the wall clock, and far enough out to stay upcoming.
  private val FarFuture     = LocalDateTime.of(2099, 1, 1, 20, 0)

  /** Until the projector has made no projection for a second: its cursors deliver on their own
   *  threads, so "the write returned" is not "the projection happened". */
  private def awaitQuiet(registry: io.prometheus.metrics.model.registry.PrometheusRegistry): Unit = {
    def calls = tools.ChurnLedger.countersOf(registry, Set("kinowo_worker_readmodel_project_calls"), (_, _) => true)
    // `last` starts EMPTY, so the first probe (which `poll` takes immediately) can never
    // read as quiet: two readings a full poll interval apart must agree.
    var last = Map.empty[String, Double]
    Eventually.poll(30000, 1000) { val now = calls; val quiet = now.nonEmpty && now == last; last = now; quiet }
    ()
  }
}
