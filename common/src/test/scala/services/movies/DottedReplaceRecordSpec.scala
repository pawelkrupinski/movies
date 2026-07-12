package services.movies

import models.{HeliosOstrowWlkp, MovieRecord, Multikino, Source, SourceData}
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `updateIfPresent` falls back to a whole-document replace when a per-source `$set
 * sourceData.<displayName>` would be rejected — a cinema whose displayName holds a '.'
 * ("Helios Ostrów Wlkp."). The replacement must NOT be the in-memory cache row verbatim:
 * that row can be missing a rating Mongo already holds (post-restart rehydrate lag, or an
 * out-of-band FilmwebUrlAudit edit), and a blind replace would NULL it on every scrape tick
 * for every dotted-name cinema. `dottedReplaceRecord` instead applies the SAME field-level
 * patch to the persisted doc, so the replace carries exactly the diff — ratings preserved.
 *
 * Pure decision (no Mongo I/O), so it's driven directly here against a client that never
 * connects, mirroring `RelaxedWriteConcernSpec`. Fails before the fix (the fallback wrote the
 * cache row, nulling the rating); passes after.
 */
class DottedReplaceRecordSpec extends AnyFlatSpec with Matchers {

  private def neverConnects =
    MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200").getDatabase("test")
  private val repo = new MongoMovieRepository(sharedDb = Some(neverConnects))

  // What Mongo holds: a fully-rated row.
  private val persisted = MovieRecord(
    imdbRating     = Some(7.5),
    metascore      = Some(80),
    filmwebRating  = Some(7.2),
    rottenTomatoes = Some(91),
    metacriticUrl  = Some("https://mc/film"),
    data           = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dotted"))))

  // The cache row the scrape tick produced: same slots, but the ratings were evicted (not yet
  // rehydrated), and a dotted-name cinema's slot just changed.
  private val cacheBefore = persisted.copy(
    imdbRating = None, metascore = None, filmwebRating = None, rottenTomatoes = None, metacriticUrl = None)
  private val cacheAfter = cacheBefore.copy(data = cacheBefore.data +
    (HeliosOstrowWlkp -> SourceData(title = Some("Dotted"), synopsis = Some("from Ostrów"))))
  private val patch = MovieRecordPatch.diff(cacheBefore, cacheAfter)

  "dottedReplaceRecord" should "preserve Mongo-owned ratings the cache row lacks while applying the slot change" in {
    val merged = repo.dottedReplaceRecord(Some(persisted), patch).getOrElse(fail("expected a replacement record"))

    // Ratings survive (the bug: a blind replace of the cache row nulled these)…
    merged.imdbRating     shouldBe Some(7.5)
    merged.metascore      shouldBe Some(80)
    merged.filmwebRating  shouldBe Some(7.2)
    merged.rottenTomatoes shouldBe Some(91)
    merged.metacriticUrl  shouldBe Some("https://mc/film")
    // …and the dotted-name slot change still lands.
    merged.cinemaData.get(HeliosOstrowWlkp).flatMap(_.synopsis) shouldBe Some("from Ostrów")
  }

  it should "report None (not-present) when the row is absent — nothing to replace, no upsert" in {
    repo.dottedReplaceRecord(None, patch) shouldBe None
  }
}
