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
 * the sibling. The two RACE tests below cannot reach that error: a genuinely concurrent pair
 * of transactions on a single-node local replica set collides as a transient `WriteConflict`
 * (retried long before this fix), never a clean post-commit `E11000`. They prove what they
 * honestly can — that the retry loop converges racing folds for one tmdbId to a single row with
 * every anchor's cinema, whichever error fires. The FORCED test at the bottom drives the prod
 * interleaving deterministically (the winner commits inside the loser's planning read) and is
 * the one that fails when the tmdbId retry is reverted; `StagingFoldOutcomeSpec` pins the pure
 * decision alongside it. The merge-order test beside it is a second, non-racing way to the
 * same error that the retry could not fix: see its comment.
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

  /** Two decorated spellings of one film, each its own fold group, both concluded to `tmdbId` —
   *  and the loser's group already holding an UNRESOLVED `movies` row of its own (a spelling that
   *  was promoted before TMDB answered). Returns the loser's title and row id. */
  private def seedWinnerAndResolvingLoser(fold: FoldFixture.Handles, tmdbId: Int, word: String): (String, String, String) = {
    import org.mongodb.scala.SingleObservableFuture
    import org.mongodb.scala.bson.collection.immutable.Document
    import services.movies.SingleCountryNormalizer.titleNormalizer
    val winnerTitle = s"Lalka reż. $word"
    val loserTitle  = s"Ladies Night - ${word.capitalize}"
    awaitTmdbIdIndex(fold)
    fold.seedStagingRow(Multikino.displayName, winnerTitle, Some(2026), tmdbId)
    fold.seedStagingRow(Helios.displayName, loserTitle, Some(2026), tmdbId)
    val loserId = services.movies.StoredMovieRecord.keyFor(loserTitle, Some(2026), titleNormalizer)
    Await.result(fold.movies.insertOne(Document("_id" -> loserId, "key" -> loserId,
      "sourceData" -> Document(), "updatedAt" -> java.util.Date.from(java.time.Instant.parse("2026-09-01T00:00:00Z")))).toFuture(), 10.seconds)
    (winnerTitle, loserTitle, loserId)
  }

  private def assertOneFilmWithBothCinemas(fold: FoldFixture.Handles, tmdbId: Int) = {
    val survivors = Await.result(fold.movies.find(Filters.eq("tmdbId", tmdbId)).toFuture(), 10.seconds)
      .flatMap(_.get("_id").map(_.asString().getValue))
    withClue(s"survivors=$survivors: ") { survivors should have size 1 }
    fold.slots.findForFilm(survivors.head).keySet shouldBe Set(Multikino.displayName, Helios.displayName)
  }

  // Not a race at all, and the shape the tmdbId retry (a42086081) could never have helped: the
  // loser's plan merges its own unresolved row INTO the sibling that already carries the tmdbId,
  // and the fold wrote the survivor before deleting the row it retires. Both hold the tmdbId for
  // that instant, so the unique index refused the fold inside its own transaction — the same
  // E11000 on every attempt, then an abandon, then a reschedule that does it again.
  it should "fold a spelling whose unresolved row merges into a sibling already holding the tmdbId" in {
    FoldFixture.withFold("staging-fold-tmdb-merge-order") { fold =>
      val tmdbId = 424353
      val (winnerTitle, loserTitle, _) = seedWinnerAndResolvingLoser(fold, tmdbId, "mergeorder")
      fold.folder().foldGroup(winnerTitle)
      noException should be thrownBy fold.folder().foldGroup(loserTitle)
      assertOneFilmWithBothCinemas(fold, tmdbId)
    }
  }

  // THE prod interleaving, forced rather than raced. The two race tests above cannot reproduce
  // it: on a single-node replica set a sibling that commits AFTER the loser's snapshot always
  // surfaces as a transient `WriteConflict` (retried long before a42086081), and one that
  // commits BEFORE it is visible to the loser's sibling lookup, which then merges instead of
  // inserting. Prod's `E11000 … index: tmdbId_1` needs the loser's reads to miss a row its
  // write then collides with — a window a multi-node set's lagging majority point may open and
  // a loopback single node does not (probed 2026-09-24: every in-transaction collision after
  // the snapshot was a WriteConflict; test commands, and so failpoints, are off).
  //
  // So the harness drives the ORDER instead of hoping for it: the loser starts planning, the
  // winner folds and commits inside the loser's planning read, and the loser's attempt then
  // fails with the server's own duplicate-key error for that very tmdbId — raised by the real
  // unique index, not constructed. What is under test is what `MongoStagingFolder` does next:
  // before a42086081 it abandoned on attempt 1 and rethrew; it must abort, re-read with the
  // winner visible, and merge into it.
  it should "retry, not abandon, a fold whose write loses the tmdbId index to a sibling that " +
    "committed while it was planning — and converge onto the winner's row" in {
    FoldFixture.withFold("staging-fold-tmdb-race-e11000") { fold =>
      import org.mongodb.scala.SingleObservableFuture
      import org.mongodb.scala.bson.collection.immutable.Document
      import services.movies.SingleCountryNormalizer.titleNormalizer
      import services.movies.FilmId

      val tmdbId = 424352
      val (winnerTitle, loserTitle, _) = seedWinnerAndResolvingLoser(fold, tmdbId, "forcedrace")
      val winnerRepo = fold.splitAwareRepository

      // The loser reads its own unresolved row back through the repository mid-plan
      // (`stitchedCinemaTitles`) — after its snapshot, before its write: that is where the
      // winner gets to commit.
      @volatile var armed = true
      val loserRepo = new services.movies.MongoMovieRepository(Some(fold.db),
        normalizer = titleNormalizer, screenings = Some(fold.screenings), slots = Some(fold.slots)) {
        override def findByIdChecked(id: FilmId) = {
          if (armed) {
            armed = false
            fold.folder(winnerRepo).foldGroup(winnerTitle)
            // The collision the loser's write now hits, as the server words it.
            Await.result(fold.movies.insertOne(Document("_id" -> "__e11000-probe__", "tmdbId" -> tmdbId)).toFuture(), 10.seconds)
            fail("the tmdbId unique index let a second document for one tmdbId in")
          }
          super.findByIdChecked(id)
        }
      }

      noException should be thrownBy fold.folder(loserRepo).foldGroup(loserTitle)
      withClue("the winner never got to commit mid-plan, so this asserted nothing: ") { armed shouldBe false }
      assertOneFilmWithBothCinemas(fold, tmdbId)
    }
  }

  /** The repository builds the partial unique `tmdbId` index asynchronously at construction;
   *  a collision is only real once it exists. */
  private def awaitTmdbIdIndex(fold: FoldFixture.Handles): Unit = {
    fold.splitAwareRepository
    val present = tools.Eventually.poll(timeoutMs = 10000, pollMs = 20) {
      Await.result(fold.movies.listIndexes().toFuture(), 10.seconds)
        .exists(_.get("name").exists(_.asString().getValue == "tmdbId_1"))
    }
    if (!present) fail("the tmdbId unique index never appeared")
  }
}
