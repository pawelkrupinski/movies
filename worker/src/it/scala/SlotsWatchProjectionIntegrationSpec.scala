package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoLuna, KinoMuranow, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import services.readmodel.{MongoReadModelRepository, ReadModelProjector}
import tools.{Env, IntegrationCorpusDatabase}

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
class SlotsWatchProjectionIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val Tmdb  = 424242
  private val title = "__slots-watch-sentinel__"
  private val year  = Some(2026)

  /** Poll `probe` until it holds or `deadlineMs` pass; the cursor delivers asynchronously so
   *  there is nothing to await but the outcome. */
  private def eventually(deadlineMs: Long)(probe: => Boolean): Boolean = {
    val by = System.currentTimeMillis() + deadlineMs
    var ok = probe
    while (!ok && System.currentTimeMillis() < by) { Thread.sleep(100); ok = probe }
    ok
  }

  "a movie_slots write with no movies or screenings change" should
    "reach the projector and emit the venue's web_screenings row" in {
    IntegrationCorpusDatabase.withDatabase(Env.get("MONGODB_URI").get, "slots_watch") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots),
        normalizer = titleNormalizer)
      val readModel  = new MongoReadModelRepository(Some(db))
      val projector  = new ReadModelProjector(repository, readModel, readModel)
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
          eventually(30000)(venueRows(KinoMuranow).nonEmpty) shouldBe true
        }

        // Prod order, step one: the second venue's SCREENINGS row lands first. The screenings
        // cursor rings, the film is re-read — and the row has no slot to stitch onto, so the
        // venue stays invisible. That is correct, and asserted so the final assertion can only be
        // satisfied by the slot write itself.
        val seenBefore = dispatched.get()
        screenings.upsertSlot(id, KinoLuna.displayName, Seq(Showtime(when.plusHours(1), None)))
        withClue("the screenings cursor never delivered the second venue's row: ") {
          eventually(30000)(dispatched.get() > seenBefore) shouldBe true
        }
        venueRows(KinoLuna) shouldBe empty

        // Step two, THE DEFECT: the venue's slot arrives last, and nothing else about the film
        // moves — no `movies` write, no `screenings` write, and a repertory film never writes
        // either again. Only a cursor on `movie_slots` can carry this to the projector.
        slots.upsertSlot(id, KinoLuna.displayName, SourceData(title = Some(title)))
        withClue(s"the slot row for ${KinoLuna.displayName} landed after the film's last projection and " +
                 "nothing re-projected the film, so the venue never reaches the site: ") {
          eventually(30000)(venueRows(KinoLuna).nonEmpty) shouldBe true
        }
      } finally {
        counting.foreach(_.close()); projecting.foreach(_.close())
        readModel.close(); repository.close(); slots.close(); screenings.close()
      }
    }
  }
}
