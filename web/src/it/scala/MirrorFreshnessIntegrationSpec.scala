package integration

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoMirrorFreshness
import tools.Env

import java.util.Date
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The `/debug` navbar's mirror age is only as good as the read behind it, and
 * that read has one job the pure rule cannot cover: take the NEWEST stamp across
 * BOTH timestamped collections. Reading `movies` alone would have reported a
 * mirror as fresh whenever the corpus happened to be written last — which is
 * most of the time, and precisely when a half-synced mirror is at its most
 * convincing. Requires MONGODB_URI against a throwaway db; skips otherwise.
 */
class MirrorFreshnessIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: this spec writes + purges sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  private val SentinelId = "^__it-mirror-freshness-"
  // Dated past anything the throwaway db could already hold, so the assertion is
  // about which sentinel wins rather than about what else a shared fixture db
  // happens to carry.
  private val older      = Date.from(java.time.Instant.parse("2099-08-30T08:03:00Z"))
  private val newer      = Date.from(java.time.Instant.parse("2099-08-31T09:04:00Z"))

  // Upsert, not insert: a previous run killed before its cleanup leaves the same
  // `_id` behind, and a duplicate-key failure there would read as a bug in the
  // thing under test.
  private def stamp(collection: String, suffix: String, at: Date): Unit = {
    val id = s"__it-mirror-freshness-$suffix"
    Await.result(
      db.getCollection(collection)
        .replaceOne(Filters.eq("_id", id), Document("_id" -> id, "updatedAt" -> at),
          com.mongodb.client.model.ReplaceOptions().upsert(true))
        .toFuture(),
      10.seconds)
  }

  override protected def afterAll(): Unit = try {
    Seq("movies", "screenings").foreach(name =>
      Await.ready(db.getCollection(name).deleteMany(Filters.regex("_id", SentinelId)).toFuture(), 10.seconds))
    client.close()
  } finally super.afterAll()

  "mirror freshness" should "report the newest stamp across movies AND screenings" in {
    // The corpus behind, the showtimes ahead: reading `movies` alone would call
    // this mirror a day staler than it is.
    stamp("movies", "corpus", older)
    stamp("screenings", "showtimes", newer)

    new MongoMirrorFreshness(Some(db)).newestUpdate().value shouldBe newer.toInstant
  }

  it should "report nothing at all when there is no database to read" in {
    new MongoMirrorFreshness(None).newestUpdate() shouldBe None
  }
}
