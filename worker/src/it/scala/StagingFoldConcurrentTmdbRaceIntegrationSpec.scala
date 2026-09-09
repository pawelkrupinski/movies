package integration

import models.{CinemaCityKinepolis, Helios, Multikino}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.ObservableFuture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.duration._

import ConcurrentFoldRaceHarness.RaceGroup

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
 * transactional Mongo, converges racing folds for one tmdbId to a single row with
 * every anchor's cinema, whichever of the two error shapes actually fires — and rely
 * on `StagingFoldOutcomeSpec` for the specific regression pin.
 *
 * The concurrency plumbing (seed, barrier, thread, join, collect) lives in
 * `ConcurrentFoldRaceHarness`, shared with the three-way race below.
 */
class StagingFoldConcurrentTmdbRaceIntegrationSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  it should "converge to one `movies` row, with every anchor's cinema, when two " +
    "decorated spellings race to conclude the same tmdbId" in {
    FoldFixture.withFold("staging-fold-tmdb-race") { fold =>
      // Its own sentinel anchor prefix and tmdbId — see `FoldFixture`, the it suites share
      // one database. `tmdbId` is a shared namespace too (the fold pulls cross-title
      // siblings by it from the WHOLE collection), so this one is unused by any neighbour.
      val tmdbId = 424350
      // Two DIFFERENT decorated spellings (the actual incident's shape), each its own
      // `sanitize(title)` fold group, both resolving to the SAME tmdbId and neither
      // pre-existing in `movies`, so both folds have to decide "new film or sibling?"
      // from scratch, at the same time.
      val groups = Seq(
        RaceGroup(Multikino, "Lalka reż. testfoldrace",       Some(2026), tmdbId),
        RaceGroup(Helios,    "Ladies Night - Testfoldrace",   Some(2026), tmdbId)
      )

      val outcomes = ConcurrentFoldRaceHarness.race(fold, groups)

      val failures = outcomes.collect { case Left(e) => e }
      withClue(s"a losing fold must retry and converge, not rethrow: ${failures.mkString("; ")}\n") {
        failures shouldBe empty
      }

      val survivors = Await.result(fold.movies.find(Filters.eq("tmdbId", tmdbId)).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      withClue(s"survivors=$survivors — two fresh inserts for one tmdbId must settle to ONE document: ") {
        survivors should have size 1
      }

      val cinemaNames = groups.map(_.cinema.displayName).toSet
      withClue("every anchor's cinema must reach the surviving film — a race must not " +
        "silently drop the loser's own cinema: ") {
        fold.slots.findForFilm(survivors.head).keySet shouldBe cinemaNames
      }
    }
  }

  // Proves the harness generalises PAST the pairwise case it was extracted from: three
  // decorated spellings, three separate fold groups, all racing to conclude the SAME
  // tmdbId at once. Same shape prod saw (a dozen decorated 'Lalka' spellings, not just
  // two) — this is about exercising `ConcurrentFoldRaceHarness` at N>2 groups, not a new
  // bug case, so it asserts the identical two invariants the pairwise test does.
  it should "converge to one `movies` row, with every anchor's cinema, when THREE " +
    "decorated spellings race to conclude the same tmdbId" in {
    FoldFixture.withFold("staging-fold-tmdb-race-3way") { fold =>
      val tmdbId = 424351
      val groups = Seq(
        RaceGroup(Multikino,            "Lalka reż. threewayrace",     Some(2026), tmdbId),
        RaceGroup(Helios,               "Ladies Night - Threewayrace", Some(2026), tmdbId),
        RaceGroup(CinemaCityKinepolis,  "Kino kobiet: Threewayrace",   Some(2026), tmdbId)
      )

      val outcomes = ConcurrentFoldRaceHarness.race(fold, groups)

      val failures = outcomes.collect { case Left(e) => e }
      withClue(s"a losing fold must retry and converge, not rethrow: ${failures.mkString("; ")}\n") {
        failures shouldBe empty
      }

      val survivors = Await.result(fold.movies.find(Filters.eq("tmdbId", tmdbId)).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      withClue(s"survivors=$survivors — three fresh inserts for one tmdbId must settle to ONE document: ") {
        survivors should have size 1
      }

      val cinemaNames = groups.map(_.cinema.displayName).toSet
      withClue("every anchor's cinema must reach the surviving film — a race must not " +
        "silently drop a loser's own cinema: ") {
        fold.slots.findForFilm(survivors.head).keySet shouldBe cinemaNames
      }
    }
  }
}
