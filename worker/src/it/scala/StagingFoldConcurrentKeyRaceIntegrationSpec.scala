package integration

import models.{CinemaCityWroclavia, Helios, KinoMuza, Multikino, Tmdb}
import org.mongodb.scala.{Document, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.util.concurrent.CyclicBarrier
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The 2026-09-08 ROUND 4 prod PL incident's SHAPE (poland/convergence run
 * 34285923158), reproduced against a real replica set. `StagingFoldConcurrentTmdbRaceIntegrationSpec`
 * covers round 2's shape — two folds racing to CREATE a film's first row for one
 * tmdbId. This is the DIFFERENT, worse shape round 4 hit: two films with SEPARATE
 * tmdbIds already have their OWN pre-existing `movies` row (one at the plain
 * "sanitize|year" key, the other disambiguated onto a suffixed one by an earlier
 * `StagingFold.resolveKeyCollisions` — see round 3, a784d5d68), and several BRAND-NEW
 * decorated spellings of the disambiguated film then fold as their OWN separate
 * `sanitize(title)` groups. Each such fold's own read is scoped by ITS OWN sanitize
 * prefix and ITS OWN tmdbId — the OTHER film's plain-key row shares neither, so it is
 * never loaded, and `resolveKeyCollisions` never gets a chance to run: the fold's own
 * `canonical()` vote recomputes the bare key from scratch and tries to RE-KEY the
 * disambiguated sibling it DID find (by tmdbId) back onto the plain key another film
 * already holds:
 *
 *   `Staging fold 'Kino na obcasach: Lalka' aborted after 1 attempt(s): ... E11000
 *   duplicate key error collection: ....movies index: key_1 dup key: { key: "lalka|2026" }`
 *
 * — dozens of times, across five decorated spellings, none of them converging on
 * retry: round 2's tmdbId retry (a42086081) re-reads BY TMDBID, which is exactly as
 * blind to the OTHER film (a different tmdbId) as the first attempt. The fix is
 * `StagingFold.planGroupProbingContestedKeys`: before committing, a fold probes Mongo
 * for the literal key(s) it is about to write that its own narrow read never
 * explained, folds any occupant it finds back into the group, and lets
 * `resolveKeyCollisions` run the SAME deterministic tie-break it always does — which
 * this spec exercises against a real transactional Mongo, with TWO decorated
 * spellings racing concurrently for the disambiguated film's identity.
 */
class StagingFoldConcurrentKeyRaceIntegrationSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  // Its own sentinel title and tmdbIds — see `FoldFixture`, the it suites share one
  // database and `tmdbId` is a shared namespace (siblings are pulled by it from the
  // WHOLE collection), so these are unused by any neighbour.
  private val bareTitle       = "Testfoldkeyrace"
  private val bareSanitize    = titleNormalizer.sanitize(bareTitle)
  private val otherTmdbId     = 424360
  private val disambiguatedId = 424361
  private val disambiguatedSuffix = s"tmdb$disambiguatedId"

  private val plainKey         = s"$bareSanitize|2026"
  private val disambiguatedKey = s"$bareSanitize~$disambiguatedSuffix|2026"

  // Two DIFFERENT decorated spellings of the ALREADY-DISAMBIGUATED film, each its own
  // `sanitize(title)` fold group — the actual incident's shape — both resolving to
  // `disambiguatedId`, racing to fold at once.
  private val newAnchors = Seq(
    Multikino -> "Kino na obcasach: Testfoldkeyrace",
    Helios    -> "Ladies Night - Testfoldkeyrace"
  )

  private def now = java.util.Date.from(java.time.Instant.now())

  it should "not corrupt the plain-key film, nor abandon forever, when brand-new decorated " +
    "spellings of a DIFFERENT, already-disambiguated film race to fold" in {
    FoldFixture.withFold("staging-fold-key-race") { fold =>
      import fold.movies

      // The OTHER film — already holds the PLAIN key from an earlier, unrelated fold.
      Await.result(movies.replaceOne(Filters.eq("_id", "f0aaaaaaaaaaaaa"),
        Document("_id" -> "f0aaaaaaaaaaaaa", "key" -> plainKey, "tmdbId" -> otherTmdbId,
          "sourceData" -> Document(), "updatedAt" -> now),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      // The DISAMBIGUATED film — two bare-title cinema votes of its own (the shape a real
      // fold of the bare title group produces: `clusterByFilm` kept its cinemas apart from
      // the other film's by tmdbId, but every one of ITS OWN cinemas still reported the
      // film as plain "Testfoldkeyrace") so the dominant-title vote favours the bare
      // spelling by COUNT once a single new decorated listing joins, not by an alphabetical
      // tie-break that would depend on which decorated spelling happened to win the race.
      Await.result(movies.replaceOne(Filters.eq("_id", "f0bbbbbbbbbbbbb"),
        Document("_id" -> "f0bbbbbbbbbbbbb", "key" -> disambiguatedKey, "tmdbId" -> disambiguatedId,
          "sourceData" -> Document(
            CinemaCityWroclavia.displayName -> Document("title" -> bareTitle),
            KinoMuza.displayName            -> Document("title" -> bareTitle),
            Tmdb.displayName                -> Document("title" -> bareTitle)),
          "updatedAt" -> now),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      newAnchors.foreach { case (cinema, title) => fold.seedStagingRow(cinema.displayName, title, Some(2026), disambiguatedId) }

      val folder = fold.folder()
      // Both threads block here until both have arrived, so their `foldGroup` calls start
      // together — the shape a shared `TaskWorker` pool claiming several `StagingFold`
      // tasks at once actually produces, not one queued cleanly after the other.
      val barrier = new CyclicBarrier(newAnchors.size)
      val threads = newAnchors.map { case (_, title) =>
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
      withClue(s"a losing fold must probe, converge, and retry through a genuine race — never abandon: ${failures.mkString("; ")}\n") {
        failures shouldBe empty
      }

      // The OTHER film is UNTOUCHED: same id, same plain key, same tmdbId — never moved,
      // never duplicated.
      val otherRows = Await.result(movies.find(Filters.eq("tmdbId", otherTmdbId)).toFuture(), 10.seconds)
      withClue(s"the plain-key film must survive as exactly one, unmoved document: $otherRows\n") {
        otherRows should have size 1
        otherRows.head.get("_id").map(_.asString().getValue) shouldBe Some("f0aaaaaaaaaaaaa")
        otherRows.head.get("key").map(_.asString().getValue) shouldBe Some(plainKey)
      }

      // The disambiguated film converges to ONE document, STILL at its disambiguated key —
      // never flipped onto the plain key another film holds — with BOTH new decorated
      // spellings' cinemas merged in.
      val kawalskiRows = Await.result(movies.find(Filters.eq("tmdbId", disambiguatedId)).toFuture(), 10.seconds)
      withClue(s"the disambiguated film must converge to exactly one document, at its OWN key: $kawalskiRows\n") {
        kawalskiRows should have size 1
        kawalskiRows.head.get("key").map(_.asString().getValue) shouldBe Some(disambiguatedKey)
      }
      val survivorId = kawalskiRows.head.get("_id").map(_.asString().getValue).get
      // Includes `TMDB` itself: the pre-existing seed's `sourceData` carries a `Tmdb`
      // slot too, and `movie_slots` stitches every titled slot alike, cinema or not.
      val cinemaNames = (newAnchors.map(_._1.displayName) :+ CinemaCityWroclavia.displayName :+ KinoMuza.displayName :+ Tmdb.displayName).toSet
      withClue("every anchor's cinema — the two pre-existing and the two new decorated " +
        "spellings — must reach the surviving film: ") {
        fold.slots.findForFilm(survivorId).keySet shouldBe cinemaNames
      }

      // No THIRD document ever sneaks in under the plain key or anywhere else.
      val allSentinelRows = Await.result(movies.find(Filters.regex("key", s"^$bareSanitize")).toFuture(), 10.seconds)
      withClue(s"expected exactly two surviving documents (plain + disambiguated), no more: $allSentinelRows\n") {
        allSentinelRows should have size 2
      }
    }
  }
}
