package services.movies

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The side collections' `filmId` / `listingKey` indexes against a Mongo that refuses at once (port 1). Building
 *  them is best effort, but one that could not be built turns every per-film read and delete into a collection
 *  scan, and the failure was dropped without a word. */
class SlotKeyedIndexesUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  "ensureIndexes" should "log each side-collection index it could not build" in {
    val logged = _root_.tools.LogCapture.capture("tools.MongoIndex") {
      SlotKeyed.ensureIndexes(client.getDatabase("unreachable"), "screenings", "ScreeningsRepository")
    }.map(_.getFormattedMessage)
    logged.filter(_.startsWith("ScreeningsRepository: index on unreachable.screenings")) should have size 2
  }
}
