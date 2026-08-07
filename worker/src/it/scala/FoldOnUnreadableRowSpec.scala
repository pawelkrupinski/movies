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
   *  that a no-op would satisfy anyway. */
  private class FailAfterPlanningRepository extends UnreadableByIdMovieRepository {
    failing = false
    val completionWrites = new java.util.concurrent.atomic.AtomicInteger(0)
    private val reads    = new java.util.concurrent.atomic.AtomicInteger(0)
    override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) =
      if (reads.incrementAndGet() > 1) (None, false) else super.findByIdChecked(id)
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

      fold.folder(repository).foldGroup(title)

      withClue("the fold did not consume its staging row, so it never reached the completion " +
               "and this asserts nothing: ")(
        fold.stagingRowExists(stagingId) shouldBe false)

      val writes = repository.completionWrites.get()
      withClue("the completion wrote the film after a FAILED read-back — that write names no " +
               "cinemas, and `replaceFilm` deletes every slot and screening it does not name: ")(
        writes shouldBe 0)
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
