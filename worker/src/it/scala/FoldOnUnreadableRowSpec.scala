package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{StoredMovieRecord, UnreadableByIdMovieRepository}

/**
 * What the staging fold does when the storage underneath it does not answer — the branches
 * that decide whether a film keeps its board, and none of which a healthy run reaches.
 *
 * Two are "a failed read is not data" guards, and they are the reason the fold plans and
 * completes against a stitched view at all. Under the storage split a film's cinemas live in
 * `movie_slots`, so the fold reads them back through `MovieRepository.findByIdChecked`. That
 * read can FAIL — a Mongo timeout, a slot-read error — and `findByIdChecked` answers
 * `(None, false)` precisely so a caller cannot mistake "I could not tell you" for "this film
 * has no cinemas". Both mistakes are destructive here:
 *
 *   - voting on an empty pool re-keys the film onto whatever single staging row is in hand,
 *     which is the settle-beat oscillation (`FoldSpellingAgreesWithSettleSpec`);
 *   - completing on an empty record makes `upsert`'s `replaceFilm` delete every slot and
 *     screening the record does not name, which is the shape that took prod PL from 39,413
 *     upcoming showtimes to 18,161 (@8033e39c6).
 *
 * The other two are the side-collection MIGRATION failing, which must not take a COMMITTED
 * fold down with it.
 */
class FoldOnUnreadableRowSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  // Its own sentinel anchor and tmdbId — see `FoldFixture`, the it suites share one database.
  private val title    = "__foldunreadable-it-sentinel__"
  private val sanitize = titleNormalizer.sanitize(title)
  private val tmdbId   = 42433

  it should "refuse to fold a film whose cinemas it could not read, rather than re-key it on the staging spelling" in {
    FoldFixture.withFold(sanitize) { fold =>
      fold.seedMigratedFilm(title, Some(2026), tmdbId)
      fold.seedStagingRow(Multikino.displayName, title, Some(2026), tmdbId)

      val thrown = intercept[IllegalStateException](
        fold.folder(new UnreadableByIdMovieRepository()).foldGroup(title))
      withClue(s"the fold failed, but not for the reason under test: ${thrown.getMessage}\n") {
        thrown.getMessage should include("Refusing to re-key the film")
      }
    }
  }

  /** Reads succeed for the transaction body and fail afterwards, which is what separates the
   *  two read guards: `stitchedCinemaTitles` reads once per group row INSIDE the transaction,
   *  `completeSideCollections` reads again per upsert AFTER the commit. Counts `upsert` so the
   *  assertion can be on the write that must not happen, rather than on a downstream shape
   *  that a no-op would satisfy anyway.
   *
   *  ⚠️ THE BUDGET IS PER FOLD AND THE PLANNING READ IS PER ATTEMPT, so this double only says
   *  what it means while the fold makes exactly ONE attempt — which is why its spec pins
   *  `maxRetries = 1`. Left at production's 3, a transient transaction error spends the good
   *  read on attempt 1 and the RETRY's planning read gets `(None, false)`, so the fold dies on
   *  the planning guard and the spec fails claiming the film was unreadable. That is not
   *  hypothetical: it is what this spec did on CI on 2026-09-04, once, with the message
   *  "Refusing to re-key the film" — the guard the OTHER test in this file is about.
   *
   *  `transientOnFirstAttempt` injects that retry deliberately, for the spec below that pins
   *  the interaction: it lets attempt 1's planning read SUCCEED — spending the budget, exactly
   *  as the real thing does — and then raises the transient error, which is the order a write
   *  conflict actually arrives in. */
  private class FailAfterPlanningRepository(transientOnFirstAttempt: Boolean = false)
      extends UnreadableByIdMovieRepository {
    failing = false
    val completionWrites = new java.util.concurrent.atomic.AtomicInteger(0)
    val reads            = new java.util.concurrent.atomic.AtomicInteger(0)
    override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) = {
      val n   = reads.incrementAndGet()
      val out = if (n > 1) (None, false) else super.findByIdChecked(id)
      if (n == 1 && transientOnFirstAttempt) {
        val e = new com.mongodb.MongoException("simulated write conflict")
        e.addLabel(com.mongodb.MongoException.TRANSIENT_TRANSACTION_ERROR_LABEL)
        throw e
      }
      out
    }
    override def upsert(t: String, y: Option[Int], e: models.MovieRecord): Unit = {
      completionWrites.incrementAndGet()
      super.upsert(t, y, e)
    }
  }

  it should "skip the completion write when the film cannot be read back, rather than write one that deletes its cinemas" in {
    FoldFixture.withFold(sanitize) { fold =>
      fold.seedMigratedFilm(title, Some(2026), tmdbId)
      val stagingId  = fold.seedStagingRow(Multikino.displayName, title, Some(2026), tmdbId)
      val repository = new FailAfterPlanningRepository

      // ONE ATTEMPT. `repository`'s good read is a per-FOLD budget and the fold's planning
      // read is per ATTEMPT, so a retry would spend it in planning and this spec would fail
      // as though the film were unreadable. See the double's own note.
      fold.folder(repository, maxRetries = 1).foldGroup(title)

      withClue("the fold did not consume its staging row, so it never reached the completion " +
               "and this asserts nothing: ")(
        fold.stagingRowExists(stagingId) shouldBe false)

      val writes = repository.completionWrites.get()
      withClue("the completion wrote the film after a FAILED read-back — that write names no " +
               "cinemas, and `replaceFilm` deletes every slot and screening it does not name: ")(
        writes shouldBe 0)
      withClue("expected exactly two reads — one planning, one completion. More means the fold " +
               "retried, spent the good read on the retry's PLANNING, and this asserted nothing: ")(
        repository.reads.get() shouldBe 2)
    }
  }

  it should "report a retried transaction as the transient error it was, not as an unreadable film" in {
    // THE FLAKE THIS FILE HAD, MADE DETERMINISTIC. A transient transaction error is normal on a
    // busy replica set — the it-suites share one Mongo and run in parallel — and the fold is
    // built to retry it. What must never happen is the retry being REPORTED as the storage
    // split failing to read a film back: that message names a data-loss guard, it is what the
    // other test in this file is about, and reading it on CI sends you looking for a corruption
    // that is not there. Whatever comes out of a fold that could not commit, it is the Mongo
    // error, not `IllegalStateException("Refusing to re-key the film")`.
    FoldFixture.withFold(sanitize) { fold =>
      fold.seedMigratedFilm(title, Some(2026), tmdbId)
      fold.seedStagingRow(Multikino.displayName, title, Some(2026), tmdbId)
      val repository = new FailAfterPlanningRepository(transientOnFirstAttempt = true)

      val thrown = intercept[Exception](fold.folder(repository, maxRetries = 1).foldGroup(title))
      withClue(s"the fold blamed the film's cinemas for what was a write conflict: ${thrown.getMessage}\n") {
        thrown shouldBe a[com.mongodb.MongoException]
        thrown.getMessage should not include "Refusing to re-key the film"
      }
    }
  }

  /** A repository whose side-collection MIGRATION fails, the two ways it can: reporting
   *  `false` (a read or write it depended on did not happen) or raising outright. */
  private class UnmovableFilmRepository(raise: Boolean) extends UnreadableByIdMovieRepository {
    failing = false
    val attempts = new java.util.concurrent.atomic.AtomicInteger(0)
    override def moveFilm(fromId: String, toId: String): Boolean = {
      attempts.incrementAndGet()
      if (raise) throw new RuntimeException("simulated side-collection move failure") else false
    }
  }

  /** A migration that fails must NOT take the fold down with it. The fold has already
   *  COMMITTED by the time the side rows are carried across, so raising here would reschedule
   *  a fold that already happened — the `Commit`-vs-`Abandon` confusion `nextAfterAttempt`
   *  exists to prevent, and the reason `pending_movies` once grew without bound. A stranded
   *  side row is the recoverable direction: it stays under the old id, where it remains the
   *  only copy, and `ReapOrphanedFilmRows` clears it out of band. */
  Seq("reports failure" -> false, "raises" -> true).foreach { case (label, raise) =>
    it should s"commit the fold even when carrying a retired key's cinemas $label" in {
      FoldFixture.withFold(sanitize) { fold =>
        // Two year-variants, which `planGroup` collapses onto the TMDB year — so the fold
        // RETIRES one key, which is what makes it try to migrate that key's side rows.
        Seq(2025, 2026).foreach(y => fold.seedMigratedFilm(title, Some(y), tmdbId))
        val stagingId  = fold.seedStagingRow(Multikino.displayName, title, Some(2026), tmdbId)
        val repository = new UnmovableFilmRepository(raise)

        noException should be thrownBy fold.folder(repository).foldGroup(title)

        withClue("the fold did not consume its staging row, so it never reached the " +
                 "migration and this asserts nothing: ")(
          fold.stagingRowExists(stagingId) shouldBe false)
        // Without this the test passes just as well when the migration is never attempted,
        // which is precisely the state this suite is here to rule out.
        val attempted = repository.attempts.get()
        withClue("no migration was attempted, so the failure branch under test never ran: ")(
          attempted should be > 0)
        withClue("the fold collapsed nothing, so no key was retired: ")(
          fold.filmIds(sanitize).size shouldBe 1)
      }
    }
  }
}
