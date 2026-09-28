package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{MongoScrapeCostStore, ScrapeCost, ScrapeCostStore}

/**
 * Live test of `MongoScrapeCostStore` against real MongoDB: the latest-N cap is a
 * server-side `$push` + `$slice`, and the read is keyset-paged — neither exists in the
 * in-memory store, so only a real server can show them working.
 *
 * Runs in a database of its own, dropped in `afterAll`.
 */
class MongoScrapeCostStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "scrape-cost-store")
  private val store    = new MongoScrapeCostStore(isolated.database)

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  "MongoScrapeCostStore" should "keep each cinema's latest RecentRuns costs, oldest first" in {
    (1 to ScrapeCostStore.RecentRuns + 2).foreach(n => store.record("scrape|Capped", ScrapeCost(n)))
    store.recent()("scrape|Capped") shouldBe (3 to ScrapeCostStore.RecentRuns + 2).map(ScrapeCost(_))
  }

  // Past one keyset page (2000 rows) — the size of the US roster, where one unbounded
  // cursor is the StackOverflow class KeysetScan exists for.
  it should "read every cinema back across more than one page" in {
    val keys = (0 until 2500).map(i => f"scrape|Paged $i%04d")
    keys.foreach(store.record(_, ScrapeCost(7)))
    val recent = store.recent()
    keys.foreach(key => recent(key) shouldBe Seq(ScrapeCost(7)))
  }
}
