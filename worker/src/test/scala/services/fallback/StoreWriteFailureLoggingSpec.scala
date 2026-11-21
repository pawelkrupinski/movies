package services.fallback

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cadence.MongoRatingCadenceStore

/** Two mirror-backed stores against a Mongo that refuses at once (port 1). Each keeps its in-memory mirror when a
 *  write fails, by design, so the pod and Mongo then disagree until a restart: logged at DEBUG, which production does
 *  not print, nothing said so. */
class StoreWriteFailureLoggingSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private def database = Some(client.getDatabase("unreachable"))

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  private def warnings(logger: String)(body: => Unit): Seq[String] =
    _root_.tools.LogCapture.capture(logger)(body).filter(_.getLevel == ch.qos.logback.classic.Level.WARN).map(_.getFormattedMessage)

  "MongoFallbackStore.put" should "warn when the fallback state could not be written" in {
    val store = new MongoFallbackStore(database, new _root_.tools.Stopwatch(() => 0L))
    val state = FallbackState("kino-x", active = true, fallbackSource = "Filmweb", fallbackRef = None, since = None,
      lastReason = None, consecutiveFailures = 1, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
      updatedAt = java.time.Instant.parse("2026-11-21T00:00:00Z"), history = Nil)
    warnings(classOf[MongoFallbackStore].getName)(store.put(state)).exists(_.contains("Filmweb-fallback write failed for kino-x")) shouldBe true
  }

  "MongoRatingCadenceStore.record" should "warn when the cadence could not be written" in {
    val store  = new MongoRatingCadenceStore(database)
    val logged = warnings(classOf[MongoRatingCadenceStore].getName) {
      store.record("imdb|tt0000001", Some("7.1"), java.time.Instant.parse("2026-11-21T00:00:00Z"))
      // Fire-and-forget: the failure arrives on the driver's thread once server selection gives up (200 ms).
      Thread.sleep(_root_.tools.SpecTimeouts.quiet(scala.concurrent.duration.Duration(2, "seconds")).toMillis)
    }
    logged.exists(_.contains("Rating-cadence write failed for imdb|tt0000001")) shouldBe true
  }
}
