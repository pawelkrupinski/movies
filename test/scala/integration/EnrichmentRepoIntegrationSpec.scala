package integration

import models.Enrichment
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.MongoClient
import org.mongodb.scala.model.Filters
import services.enrichment.EnrichmentRepo
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Live test of EnrichmentRepo against real MongoDB Atlas. Requires MONGODB_URI
 * to be set (in `.env.local` or the environment) AND the cluster to be
 * reachable. Skips otherwise so CI doesn't fail without secrets and so
 * intermittent Atlas TLS / network issues don't fail the suite.
 *
 * Writes a sentinel record under a deterministic id, reads it back, and cleans
 * up. Run-isolated so it won't interfere with the production collection of
 * real enrichments.
 */
class EnrichmentRepoIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val repo = new EnrichmentRepo()

  // Class-level `assume(...)` aborts the suite (sbt counts that as a failure).
  // Per-test `cancel(...)` marks individual tests as skipped instead — which is
  // what we want when MONGODB_URI is unset (CI without secrets) or the cluster
  // is unreachable (TLS handshake refused / allowlist miss / Atlas blip).
  private def requireMongo(): Unit = {
    if (Env.get("MONGODB_URI").isEmpty) cancel("MONGODB_URI not set")
    if (!repo.enabled) cancel("MongoDB Atlas not reachable (init timed out or refused)")
  }

  // Tidy sentinel rows so they don't leak into the production positive cache
  // at the next app startup (the service hydrates *everything* from Mongo).
  // Best-effort: if the cluster has gone away between the test and teardown,
  // log and move on instead of blocking the suite for 10s on the timeout.
  override protected def afterAll(): Unit = try {
    Try {
      val client = MongoClient(Env.get("MONGODB_URI").get)
      val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
        .getCollection("enrichments")
      Await.ready(
        coll.deleteMany(Filters.regex("_id", "^__integration-test-")).toFuture(),
        10.seconds
      )
      client.close()
    }.recover { case ex =>
      info(s"afterAll cleanup skipped — Mongo unreachable: ${ex.getMessage}")
    }
    repo.close()
  } finally super.afterAll()

  "EnrichmentRepo" should "be enabled when MONGODB_URI is set" in {
    requireMongo()
    repo.enabled shouldBe true
  }

  it should "round-trip an Enrichment: upsert → findAll → match" in {
    requireMongo()
    val sentinelTitle = "__integration-test-sentinel__"
    val sentinelYear  = Some(1900)
    val toStore = Enrichment(
      imdbId         = "tt0000001",
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
    e.imdbId         shouldBe "tt0000001"
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
    requireMongo()
    val title = "__integration-test-sparse__"
    val toStore = Enrichment(
      imdbId         = "tt0000002",
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
    e.imdbId         shouldBe "tt0000002"
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
    requireMongo()
    val titleCaps = "__integration-test-CASEDEDUPE__"
    val titleLow  = "__integration-test-casededupe__"
    val withUrls = Enrichment(
      imdbId            = "tt0000010",
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

    val rows = repo.findAll().filter(_._3.imdbId == "tt0000010")
    rows                                  should have size 1
    // Second upsert wins: URLs nulled, which is exactly what made the
    // production case observable.
    rows.head._3.metacriticUrl     shouldBe None
    rows.head._3.rottenTomatoesUrl shouldBe None
  }
}
