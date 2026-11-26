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

  // The day's first permits race to create its document: the loser's upsert met the winner's id and was
  // read as "spent" with the day's count at 1, sending a new film's IMDb id to the next OMDb day.
  it should "grant every worker's first permits of a day that race to open it" in {
    import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}
    val workers = 8
    val pool    = Executors.newFixedThreadPool(workers)
    try {
      val refused = (1 to 20).flatMap { dayOffset =>
        val day     = Instant.parse("2027-01-01T17:00:00Z").plus(java.time.Duration.ofDays(dayOffset.toLong))
        val barrier = new CyclicBarrier(workers)
        val takes   = (1 to workers).map { _ =>
          val budget = new MongoOmdbBudget(isolated.database, limit = 1000)
          pool.submit(() => { barrier.await(); budget.take(OmdbAsk.NewFilm, day) })
        }
        takes.map(_.get(tools.SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)).filter(_ != OmdbBudget.Granted)
      }
      refused shouldBe empty
    } finally pool.shutdownNow()
  }

  // The days opened before the keyed store hold their count as an Int32 (`$inc` by 1, `$max` to an Int limit); the store
  // raises it by Int64s. Both read as the day's count: one at the limit stays spent, one below it keeps granting.
  it should "read the Int32 counts of days opened before the keyed store" in {
    import org.mongodb.scala.SingleObservableFuture
    import org.mongodb.scala.bson.collection.immutable.Document
    val collection = isolated.database.getCollection[Document](MongoOmdbBudget.Collection)
    def opened(day: String, used: Int): Unit = {
      scala.concurrent.Await.result(collection.insertOne(Document("_id" -> day, "used" -> org.mongodb.scala.bson.BsonInt32(used))).toFuture(),
        tools.SpecTimeouts.Io); ()
    }
    val budget = new MongoOmdbBudget(isolated.database, limit = 10)
    val spentDay = Instant.parse("2027-03-01T17:00:00Z")
    opened(OmdbBudget.dayOf(spentDay).toString, 10)
    budget.take(OmdbAsk.NewFilm, spentDay) shouldBe OmdbBudget.Spent(Instant.parse("2027-03-02T06:00:00Z"))
    val openDay = Instant.parse("2027-03-03T17:00:00Z")
    opened(OmdbBudget.dayOf(openDay).toString, 7)
    budget.take(OmdbAsk.Routine, openDay) shouldBe OmdbBudget.Granted        // 7 of routine's 8
    budget.take(OmdbAsk.Routine, openDay) shouldBe OmdbBudget.Spent(Instant.parse("2027-03-04T06:00:00Z"))
    budget.markSpent(openDay)
    budget.take(OmdbAsk.NewFilm, openDay) shouldBe OmdbBudget.Spent(Instant.parse("2027-03-04T06:00:00Z"))
  }

  it should "leave the day at the limit when every worker marks it spent at once" in {
    import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}
    val workers = 8
    val pool    = Executors.newFixedThreadPool(workers)
    val day     = Instant.parse("2027-04-01T17:00:00Z")
    try {
      val reader  = new MongoOmdbBudget(isolated.database, limit = 1000)
      (1 to 3).foreach(_ => reader.take(OmdbAsk.NewFilm, day))
      val barrier = new CyclicBarrier(workers)
      (1 to workers).map { _ =>
        val budget = new MongoOmdbBudget(isolated.database, limit = 1000)
        pool.submit((() => { barrier.await(); budget.markSpent(day) }): Runnable)
      }.foreach(_.get(tools.SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS))
      val days = tools.KeyedStore.counters(tools.MongoStoreDocuments.of(isolated.database.getCollection(MongoOmdbBudget.Collection)))
      days.counted(OmdbBudget.dayOf(day).toString, "used") shouldBe 1000
    } finally pool.shutdownNow()
  }
}
