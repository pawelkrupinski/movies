package integration

import models.MovieRecord
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import services.movies.MovieRepo
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of MovieRepo against real MongoDB Atlas. Requires MONGODB_URI
 * to be set (in `.env.local` or the environment). Skips otherwise so CI doesn't
 * fail without secrets.
 *
 * Writes a sentinel record under a deterministic id, reads it back, and cleans
 * up. Run-isolated so it won't interfere with the production collection of
 * real enrichments.
 */
class MovieRepoIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val repo = new MovieRepo()

  // Tidy sentinel rows so they don't leak into the production positive cache
  // at the next app startup (the service hydrates *everything* from Mongo).
  // Cleans both the canonical 'movies' collection (where production writes go)
  // and the legacy 'enrichments' collection (which the dual-read tests seed
  // directly).
  override protected def afterAll(): Unit = try {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    Seq("movies", "enrichments").foreach { name =>
      Await.ready(
        db.getCollection(name).deleteMany(Filters.regex("_id", "^__integration-test-")).toFuture(),
        10.seconds
      )
    }
    client.close()
    repo.close()
  } finally super.afterAll()

  "MovieRepo" should "be enabled when MONGODB_URI is set" in {
    repo.enabled shouldBe true
  }

  it should "round-trip an MovieRecord: upsert → findAll → match" in {
    val sentinelTitle = "__integration-test-sentinel__"
    val sentinelYear  = Some(1900)
    val toStore = MovieRecord(
      imdbId         = Some("tt0000001"),
      imdbRating     = Some(7.5),
      metascore      = Some(80),
      originalTitle  = Some("Integration Test"),
      filmwebUrl     = Some("https://www.filmweb.pl/film/Test-1900-1"),
      filmwebRating  = Some(7.2),
      rottenTomatoes = Some(91),
      tmdbId            = Some(424242),
      metacriticUrl     = Some("https://www.metacritic.com/movie/integration-test"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/integration_test")
    )

    repo.upsert(sentinelTitle, sentinelYear, toStore)

    val all   = repo.findAll()
    val found = all.find { case (t, y, _) => t == sentinelTitle && y == sentinelYear }
    found should not be empty
    val (_, _, e) = found.get
    e.imdbId         shouldBe Some("tt0000001")
    e.imdbRating     shouldBe Some(7.5)
    e.metascore      shouldBe Some(80)
    e.originalTitle  shouldBe Some("Integration Test")
    e.filmwebUrl     shouldBe Some("https://www.filmweb.pl/film/Test-1900-1")
    e.filmwebRating  shouldBe Some(7.2)
    e.rottenTomatoes shouldBe Some(91)
    e.tmdbId            shouldBe Some(424242)
    e.metacriticUrl     shouldBe Some("https://www.metacritic.com/movie/integration-test")
    e.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/integration_test")
  }

  it should "handle Enrichments with all-None optional fields" in {
    val title = "__integration-test-sparse__"
    val toStore = MovieRecord(
      imdbId         = Some("tt0000002"),
      imdbRating     = None,
      metascore      = None,
      originalTitle  = None,
      filmwebUrl     = None,
      filmwebRating  = None,
      rottenTomatoes = None
    )
    repo.upsert(title, None, toStore)
    val found = repo.findAll().find { case (t, y, _) => t == title && y.isEmpty }
    found should not be empty
    val (_, _, e) = found.get
    e.imdbId         shouldBe Some("tt0000002")
    e.imdbRating     shouldBe None
    e.metascore      shouldBe None
    e.originalTitle  shouldBe None
    e.filmwebUrl     shouldBe None
    e.filmwebRating  shouldBe None
    e.rottenTomatoes    shouldBe None
    e.metacriticUrl     shouldBe None
    e.rottenTomatoesUrl shouldBe None
  }

  // Regression for "Tom i Jerry: Przygoda w muzeum" / "Tom i jerry: przygoda w
  // muzeum": case-only variants of the same Polish title accumulated as
  // separate Mongo rows because docId was case-preserved. The hourly refresh
  // walks the Caffeine cache (which collapses them) and only ever wrote back
  // to one row, leaving the other(s) frozen at whatever they were when first
  // upserted — including with metacriticUrl/rottenTomatoesUrl set to None for
  // records created before that feature shipped.
  it should "collapse case-variant cleanTitle upserts into a single Mongo row" in {
    val titleCaps = "__integration-test-CASEDEDUPE__"
    val titleLow  = "__integration-test-casededupe__"
    val withUrls = MovieRecord(
      imdbId            = Some("tt0000010"),
      imdbRating        = Some(7.5),
      metascore         = Some(80),
      originalTitle     = Some("Case Dedupe Test"),
      metacriticUrl     = Some("https://www.metacritic.com/movie/case-dedupe-test"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/case_dedupe_test")
    )
    val withoutUrls = withUrls.copy(
      metacriticUrl     = None,
      rottenTomatoesUrl = None
    )

    // Upsert UPPER first (with URLs), then LOWER (without). With normalized
    // docId both writes target the same _id, so the second overwrites.
    repo.upsert(titleCaps, Some(2025), withUrls)
    repo.upsert(titleLow,  Some(2025), withoutUrls)

    val rows = repo.findAll().filter(_._3.imdbId.contains("tt0000010"))
    rows                                  should have size 1
    // Second upsert wins: URLs nulled, which is exactly what made the
    // production case observable.
    rows.head._3.metacriticUrl     shouldBe None
    rows.head._3.rottenTomatoesUrl shouldBe None
  }

  // ── Dual-read migration ─────────────────────────────────────────────────
  //
  // The collection was renamed `enrichments` → `movies` as part of the
  // unified-MovieCache transition. To avoid a downtime window during the
  // deploy, MovieRepo reads from BOTH collections (legacy 'enrichments' +
  // canonical 'movies') and merges them, with 'movies' winning on docId
  // conflict. All writes go to 'movies'. Once natural re-enrichment cycles
  // (or the one-shot `MoviesCollectionMigrate` script) have drained the
  // legacy collection, a follow-up commit drops the dual-read.

  private def seedLegacy(id: String, doc: Document): Unit = {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val legacy = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
      .getCollection[Document]("enrichments")
    Await.ready(
      legacy.replaceOne(Filters.eq("_id", id), doc, new ReplaceOptions().upsert(true)).toFuture(),
      10.seconds
    )
    client.close()
  }

  it should "dual-read the legacy 'enrichments' collection — rows that exist only there still come through findAll" in {
    val legacyOnlyId    = "__integration-test-LEGACY-ONLY__|2026"
    val legacyOnlyTitle = "__integration-test-LEGACY-ONLY__"
    seedLegacy(legacyOnlyId, Document(
      "_id"    -> BsonString(legacyOnlyId),
      "title"  -> BsonString(legacyOnlyTitle),
      "year"   -> BsonInt32(2026),
      "imdbId" -> BsonString("tt0000999")
    ))

    val all   = repo.findAll()
    val found = all.find { case (t, _, _) => t == legacyOnlyTitle }
    found            shouldBe defined
    found.get._3.imdbId shouldBe Some("tt0000999")
  }

  it should "prefer 'movies' over 'enrichments' when the same docId exists in both" in {
    val sharedTitle = "__integration-test-CONFLICT__"
    val sharedYear  = Some(2026)
    val sharedId    = s"${services.movies.MovieService.normalize(sharedTitle)}|2026"

    // Legacy says rating 5.0; canonical (movies) says 8.0. movies wins.
    seedLegacy(sharedId, Document(
      "_id"        -> BsonString(sharedId),
      "title"      -> BsonString(sharedTitle),
      "year"       -> BsonInt32(2026),
      "imdbId"     -> BsonString("tt0001000"),
      "imdbRating" -> BsonDouble(5.0)
    ))
    repo.upsert(sharedTitle, sharedYear, MovieRecord(
      imdbId     = Some("tt0001000"),
      imdbRating = Some(8.0),
      metascore  = None,
      originalTitle = None
    ))

    val rows = repo.findAll().filter(_._3.imdbId.contains("tt0001000"))
    rows                  should have size 1
    rows.head._3.imdbRating shouldBe Some(8.0)
  }
}
