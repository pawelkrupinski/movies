package services.movies

import org.bson.BsonDocument
import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[MongoResumeTokenStore]] against a Mongo that refuses at once (port 1). Its failures are dropped by design, but a
 *  clean shutdown's synchronous save that failed, or a clear of an invalid token that failed, left no trace at all. */
class MongoResumeTokenStoreUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private val store  = new MongoResumeTokenStore(client.getDatabase("unreachable"))

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  private def warnings(body: => Unit): Seq[String] =
    _root_.tools.LogCapture.capture(classOf[MongoResumeTokenStore].getName)(body).map(_.getFormattedMessage)

  "save" should "log a synchronous save that failed" in {
    val logged = warnings(store.save("movies", new BsonDocument(), synchronously = true))
    logged.exists(_.contains("resume token save(movies) failed")) shouldBe true
  }

  "clear" should "log a clear that failed" in {
    val logged = warnings(store.clear("movies"))
    logged.exists(_.contains("resume token clear(movies) failed")) shouldBe true
  }
}
