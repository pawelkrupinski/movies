package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{MongoOmdbBudget, OmdbAsk, OmdbBudget}
import tools.{IntegrationMongoSuite, IsolatedMongoDatabase}

import java.time.Instant

/** The fleet's OMDb budget in Mongo: two countries' workers draw on one day's count, each ask stopping at its own
 *  ceiling — what `InMemoryOmdbBudget` does in one process, here across processes. */
class MongoOmdbBudgetIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite with BeforeAndAfterAll {
  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "omdb_budget")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  "the fleet's OMDb budget" should "count every worker's permits against one day, by ask" in {
    val (pl, de) = (new MongoOmdbBudget(isolated.database, limit = 10), new MongoOmdbBudget(isolated.database, limit = 10))
    val noon = Instant.parse("2026-10-26T17:00:00Z")
    val reset = Instant.parse("2026-10-27T05:00:00Z")
    (1 to 4).foreach(_ => pl.take(OmdbAsk.Routine, noon) shouldBe OmdbBudget.Granted)
    (1 to 4).foreach(_ => de.take(OmdbAsk.Routine, noon) shouldBe OmdbBudget.Granted)
    pl.take(OmdbAsk.Routine, noon) shouldBe OmdbBudget.Spent(reset)  // routine's 80% of 10
    de.take(OmdbAsk.UserWaiting, noon) shouldBe OmdbBudget.Granted   // 90%
    pl.take(OmdbAsk.NewFilm, noon) shouldBe OmdbBudget.Granted       // the reserve
    de.take(OmdbAsk.NewFilm, noon) shouldBe OmdbBudget.Spent(reset)
    pl.take(OmdbAsk.NewFilm, reset) shouldBe OmdbBudget.Granted      // a new OMDb day
    de.markSpent(reset)
    pl.take(OmdbAsk.NewFilm, reset) shouldBe OmdbBudget.Spent(Instant.parse("2026-10-28T05:00:00Z"))
  }
}
