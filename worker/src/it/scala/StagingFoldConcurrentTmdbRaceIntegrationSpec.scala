package integration

import models.{Helios, Multikino}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.ObservableFuture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.CyclicBarrier
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The 2026-09-08 prod PL incident's SHAPE, reproduced against a real replica set:
 * 'Lalka' (tmdbId 1321666) folded from a dozen decorated spellings — 'Lalka reż.
 * Maciej Kawalski', 'Ladies Night - Lalka', 'Kino kobiet: Lalka' among them — each its
 * OWN `sanitize(title)` fold GROUP (`StagingFold.selectStagingGroup`), so each ran
 * through its OWN `MongoStagingFolder.foldGroup` call, in its OWN Mongo transaction,
 * with no lock serializing them against each other (`TaskWorker` claims across a
 * shared 4-thread pool — `KINOWO_WORKER_POOL_SIZE` — with no per-tmdbId or per-anchor
 * exclusion). Two such folds resolving to the SAME tmdbId can therefore both read "no
 * existing `movies` row for this identity" and both decide to mint a fresh one — prod
 * saw the second one's insert collide with the partial UNIQUE index on `tmdbId`:
 *
 *   `Staging fold 'Lalka' aborted after 1 attempt(s): ... E11000 duplicate key error
 *   collection: ....movies index: tmdbId_1 dup key: { tmdbId: 1321666 }`
 *
 * Before the fix this was an immediate `Next.Abandon` (a duplicate-key write error
 * carries no transient-transaction label), so the whole fold task rethrew and the
 * `StagingReaper` rescheduled it under backoff instead of just re-reading and finding
 * the sibling. `StagingFoldOutcomeSpec` is what actually PINS that decision (fail
 * before / pass after, on the exact incident message) — it is a pure function, so it
 * can construct the E11000 deterministically. This spec cannot: a genuinely
 * concurrent pair of real Mongo transactions on a single-node local replica set
 * overwhelmingly collides as a transient `WriteConflict` (already retried before this
 * fix) rather than a clean post-commit `E11000`, because a real replica set's
 * network-separated worker processes have room to fully commit one side before the
 * other reads, and this loopback pair usually does not. So treat this spec as what it
 * can honestly prove — that `MongoStagingFolder`'s retry loop, against a REAL
 * transactional Mongo, converges two racing folds for one tmdbId to a single row with
 * every anchor's cinema, whichever of the two error shapes actually fires — and rely
 * on `StagingFoldOutcomeSpec` for the specific regression pin.
 */
class StagingFoldConcurrentTmdbRaceIntegrationSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  // Its own sentinel anchor prefix and tmdbId — see `FoldFixture`, the it suites share
  // one database. `tmdbId` is a shared namespace too (the fold pulls cross-title
  // siblings by it from the WHOLE collection), so this one is unused by any neighbour.
  private val tmdbId = 424350
  // Two DIFFERENT decorated spellings (the actual incident's shape), each its own
  // `sanitize(title)` fold group, both resolving to the SAME tmdbId and neither
  // pre-existing in `movies`, so both folds have to decide "new film or sibling?"
  // from scratch, at the same time.
  private val anchors = Seq(
    Multikino -> "Lalka reż. testfoldrace",
    Helios    -> "Ladies Night - Testfoldrace"
  )

  it should "converge to one `movies` row, with every anchor's cinema, when two " +
    "decorated spellings race to conclude the same tmdbId" in {
    FoldFixture.withFold("staging-fold-tmdb-race") { fold =>
      anchors.foreach { case (cinema, title) => fold.seedStagingRow(cinema.displayName, title, Some(2026), tmdbId) }

      val folder = fold.folder()
      // Both threads block here until both have arrived, so their `foldGroup` calls —
      // session open, read, write, commit — start together rather than queueing one
      // after another, which is what actually raced in prod (a shared `TaskWorker`
      // pool claiming several `StagingFold` tasks at once).
      val barrier = new CyclicBarrier(anchors.size)
      val threads = anchors.map { case (_, title) =>
        var outcome: Either[Throwable, Unit] = Left(new IllegalStateException("thread did not run"))
        val t = new Thread(() => {
          barrier.await()
          outcome = try { folder.foldGroup(title); Right(()) } catch { case e: Throwable => Left(e) }
        })
        t.start()
        (t, () => outcome)
      }
      threads.foreach(_._1.join(30.seconds.toMillis))

      val failures = threads.map(_._2()).collect { case Left(e) => e }
      withClue(s"a losing fold must retry and converge, not rethrow: ${failures.mkString("; ")}\n") {
        failures shouldBe empty
      }

      val survivors = Await.result(fold.movies.find(Filters.eq("tmdbId", tmdbId)).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      withClue(s"survivors=$survivors — two fresh inserts for one tmdbId must settle to ONE document: ") {
        survivors should have size 1
      }

      val cinemaNames = anchors.map(_._1.displayName).toSet
      withClue("every anchor's cinema must reach the surviving film — a race must not " +
        "silently drop the loser's own cinema: ") {
        fold.slots.findForFilm(survivors.head).keySet shouldBe cinemaNames
      }
    }
  }
}
