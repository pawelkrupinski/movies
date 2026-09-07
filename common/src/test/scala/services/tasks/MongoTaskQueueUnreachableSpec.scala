package services.tasks

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * `MongoTaskQueue` against a Mongo that cannot be reached at all — the one failure
 * a unit spec can produce without a server. Every op here dies in server selection
 * after 200ms, which stands in for any driver error (a step-down, a socket reset,
 * a timeout) on the path that matters: the queue must REPORT the failure, not
 * dress it up as a healthy answer.
 */
class MongoTaskQueueUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  // Port 1 refuses instantly; the short selection timeout keeps each op to ~200ms.
  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private val queue  = new MongoTaskQueue(Some(client.getDatabase("unreachable")), "tasks")

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  // A Mongo error used to come back as `Duplicate` — "already queued" — so the caller
  // dropped the task believing it was in hand, and nothing ever ran it.
  "MongoTaskQueue.enqueue" should "report a Mongo failure as Failed, not as a duplicate" in {
    queue.enqueue(TaskType.ScrapeCinema, "scrape|kino-x", submittedAt = t0) should matchPattern {
      case EnqueueResult.Failed(_) =>
    }
  }
}
