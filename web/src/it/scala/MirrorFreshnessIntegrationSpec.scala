package integration

import org.mongodb.scala.{Document, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoMirrorFreshness
import tools.{Env, IsolatedMongoDatabase}

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

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // A database of its own (`IsolatedMongoDatabase` refuses a real cluster), so the
  // newest stamp is one this spec wrote, not whatever a co-running suite last touched.
  private val isolated = IsolatedMongoDatabase.open(tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get, "mirror-freshness")
  private val db       = isolated.database

  private val older      = Date.from(java.time.Instant.parse("2099-08-30T08:03:00Z"))
  private val newer      = Date.from(java.time.Instant.parse("2099-08-31T09:04:00Z"))

  private def stamp(collection: String, suffix: String, at: Date): Unit =
    Await.result(db.getCollection(collection).insertOne(Document("_id" -> suffix, "updatedAt" -> at)).toFuture(), 10.seconds)

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

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
