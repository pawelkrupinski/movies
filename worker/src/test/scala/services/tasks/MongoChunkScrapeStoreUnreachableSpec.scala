package services.tasks

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `MongoChunkScrapeStore` against a Mongo it cannot reach (port 1 refuses; ~200ms per op).
 *  Every read must THROW: its empty answer was acted on — a reduce published an empty
 *  listing from chunks it never read, then completed the run and deleted them. */
class MongoChunkScrapeStoreUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private val store  = new MongoChunkScrapeStore(Some(client.getDatabase("unreachable")))

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  "MongoChunkScrapeStore" should "throw, not answer 'nothing stored', when its reads cannot reach Mongo" in {
    an[Exception] should be thrownBy store.activeRun("Kino X")
    an[Exception] should be thrownBy store.storedKeys("Kino X", "run-1")
    an[Exception] should be thrownBy store.loadChunks("Kino X", "run-1")
    an[Exception] should be thrownBy store.activeRuns()
  }
}
