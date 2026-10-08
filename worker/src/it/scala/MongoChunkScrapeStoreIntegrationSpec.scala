package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

/** The chunk-run store on a real Mongo: a run's stored keys — read keys-only, the chunks' parses left
 *  on the server — name every chunk stored, and the chunks themselves come back whole. */
class MongoChunkScrapeStoreIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {
  private val now = tools.MongoTtlSpecClock.Pinned.instant()

  "MongoChunkScrapeStore" should "name every stored chunk by key, and return each one's value" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "chunk-store") { db =>
      val store = new MongoChunkScrapeStore(Some(db))
      val runId = store.startRun("Odeon Norwich", Seq("2026-10-01", "2026-10-02", "2026-10-03"), now, 1.hour).get
      store.storeChunk("Odeon Norwich", runId, "2026-10-01", StoredChunk("""[{"a":1}]"""), now)
      store.storeChunk("Odeon Norwich", runId, "2026-10-02", StoredChunk("""[{"b":2}]""", complete = false, attempt = 2), now)
      store.storedKeys("Odeon Norwich", runId) shouldBe Set("2026-10-01", "2026-10-02")
      store.loadChunks("Odeon Norwich", runId) shouldBe Map("2026-10-01" -> StoredChunk("""[{"a":1}]"""),
        "2026-10-02" -> StoredChunk("""[{"b":2}]""", complete = false, attempt = 2))
      store.activeRuns().map(_.cinema) shouldBe Seq("Odeon Norwich")
    }
  }

  // A stalled attempt whose lease expired can land after its retry: its slice must not replace the retry's.
  it should "keep a later attempt's slice from an earlier attempt landing after it" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "chunk-store-attempts") { db =>
      val store = new MongoChunkScrapeStore(Some(db))
      val runId = store.startRun("Odeon Norwich", Seq("2026-10-01"), now, 1.hour).get
      store.storeChunk("Odeon Norwich", runId, "2026-10-01", StoredChunk("whole", attempt = 2), now)
      store.storeChunk("Odeon Norwich", runId, "2026-10-01", StoredChunk("partial", complete = false, attempt = 1), now)
      store.loadChunks("Odeon Norwich", runId) shouldBe Map("2026-10-01" -> StoredChunk("whole", attempt = 2))
      store.storeChunk("Odeon Norwich", runId, "2026-10-01", StoredChunk("newer", complete = false, attempt = 3), now)
      store.loadChunks("Odeon Norwich", runId) shouldBe Map("2026-10-01" -> StoredChunk("newer", complete = false, attempt = 3))
    }
  }
}
