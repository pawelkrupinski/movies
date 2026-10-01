package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The chunk-page memo on a real Mongo: what is remembered for a (cinema, chunk) is recalled for it,
 *  replaced by a later remember, and nothing is recalled for a chunk never remembered. */
class ChunkPageMemoIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {
  "MongoChunkPageMemo" should "recall the last entry remembered for a chunk, and none for another" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "chunk-page-memo") { db =>
      val memo = new MongoChunkPageMemo(Some(db), java.time.Clock.fixed(java.time.Instant.parse("2026-10-01T10:00:00Z"), java.time.ZoneOffset.UTC))
      memo.recall("Odeon Norwich", "2026-10-02") shouldBe None
      memo.remember("Odeon Norwich", "2026-10-02", ChunkPageMemo.Entry("abc", 1, """[{"t":1}]"""))
      memo.remember("Odeon Norwich", "2026-10-02", ChunkPageMemo.Entry("def", 2, """[{"t":2}]"""))
      memo.recall("Odeon Norwich", "2026-10-02") shouldBe Some(ChunkPageMemo.Entry("def", 2, """[{"t":2}]"""))
      memo.recall("Odeon Norwich", "2026-10-03") shouldBe None
    }
  }
}
