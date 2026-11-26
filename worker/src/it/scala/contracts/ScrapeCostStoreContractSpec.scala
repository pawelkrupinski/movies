package services.contracts

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{InMemoryScrapeCostStore, MongoScrapeCostStore, ScrapeCost, ScrapeCostStore}

/**
 * The scrape costs, as both stores keep them: each cinema's latest RecentRuns costs, oldest first (in Mongo a
 * server-side `$push` + `$slice`), and a read of every cinema across more than one keyset page. Mongo's half runs in a
 * database of its own, dropped in `afterAll`.
 */
class ScrapeCostStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "scrape-cost-store")

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val implementations: Seq[(String, () => ScrapeCostStore)] = Seq(
    "InMemoryScrapeCostStore" -> (() => new InMemoryScrapeCostStore),
    "MongoScrapeCostStore"    -> (() => { isolated.freshCollection(MongoScrapeCostStore.Collection); new MongoScrapeCostStore(isolated.database) }))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "hold nothing before a cost is recorded" in {
      fresh().recent() shouldBe empty
    }

    it should "keep each cinema's latest RecentRuns costs, oldest first" in {
      val store = fresh()
      (1 to ScrapeCostStore.RecentRuns + 2).foreach(n => store.record("scrape|Capped", ScrapeCost(n)))
      store.record("scrape|Other", ScrapeCost(9))
      store.recent() shouldBe Map("scrape|Capped" -> (3 to ScrapeCostStore.RecentRuns + 2).map(ScrapeCost(_)), "scrape|Other" -> Seq(ScrapeCost(9)))
    }

    // Past one keyset page (2000 rows) — the size of the US roster, where one unbounded
    // cursor is the StackOverflow class KeysetScan exists for.
    it should "read every cinema back across more than one page" in {
      val store = fresh()
      val keys  = (0 until 2500).map(i => f"scrape|Paged $i%04d")
      keys.foreach(store.record(_, ScrapeCost(7)))
      val recent = store.recent()
      recent.keySet shouldBe keys.toSet
      keys.foreach(key => recent(key) shouldBe Seq(ScrapeCost(7)))
    }
  }
}
