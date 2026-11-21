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
    // Fire-and-forget: the failure is logged on the driver's thread once server selection gives up, after `record`
    // returned -- so this listens on the logger for as long as that takes.
    val logger = _root_.tools.LogCapture.logbackLogger(classOf[MongoRatingCadenceStore].getName)
    val heard  = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val ear    = new ch.qos.logback.core.AppenderBase[ch.qos.logback.classic.spi.ILoggingEvent] {
      override def append(event: ch.qos.logback.classic.spi.ILoggingEvent): Unit =
        if (event.getLevel == ch.qos.logback.classic.Level.WARN) { heard.add(event.getFormattedMessage); () }
    }
    ear.setContext(logger.getLoggerContext); ear.start(); logger.addAppender(ear)
    try {
      store.record("imdb|tt0000001", Some("7.1"), java.time.Instant.parse("2026-11-21T00:00:00Z"))
      _root_.tools.Eventually.eventually(
        heard.toArray.exists(_.toString.contains("Rating-cadence write failed for imdb|tt0000001")) shouldBe true,
        timeoutMs = _root_.tools.SpecTimeouts.Io.toMillis)
    } finally { logger.detachAppender(ear); ear.stop() }
  }
}
