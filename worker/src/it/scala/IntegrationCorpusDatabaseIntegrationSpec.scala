import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{Env, IntegrationCorpusDatabase}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * A whole-corpus suite's database must be GONE by the time its scope returns.
 *
 * Every `it/` run used to strand one database per whole-corpus suite —
 * `<MONGODB_DB>_merge-screenings`, `_rekey-screenings`, `_screenings-rewrite` — because
 * those suites deleted their sentinel ROWS in a `finally` and never dropped the database
 * holding them. Fifty of them had accumulated on the local replica set.
 *
 * The subtler half is that a drop which is merely STARTED does not count.
 * `WorkerWiringNormalizerIntegrationSpec` did call `drop()`, but on a `toFuture()` it never
 * awaited, so the JVM exited first and `kinowo_it_wiring_*` survived anyway. Hence the
 * assertion here is "absent immediately after the scope returns", which a fire-and-forget
 * drop passes only by luck.
 */
class IntegrationCorpusDatabaseIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri = Env.get("MONGODB_URI").get

  private def databaseNames(client: MongoClient): Seq[String] =
    Await.result(client.listDatabaseNames().toFuture(), 30.seconds)

  /** Materialise the database — Mongo does not create one until something is written. */
  private def seed(client: MongoClient, name: String): Unit =
    Await.result(client.getDatabase(name).getCollection("probe").insertOne(Document("_id" -> "sentinel")).toFuture(), 30.seconds)

  "a corpus database" should "be dropped by the time its scope returns" in {
    val client = MongoClient(uri)
    try {
      val name = IntegrationCorpusDatabase.withDatabase(uri, "drop-probe") { database =>
        seed(client, database.name)
        withClue("the seeded database must exist while the scope is open: ")(
          databaseNames(client) should contain(database.name))
        database.name
      }

      withClue(s"$name outlived its scope — the drop was never awaited: ")(
        databaseNames(client) should not contain name)
    } finally client.close()
  }

  it should "be dropped even when the body throws, so a failing run leaks nothing" in {
    val client = MongoClient(uri)
    try {
      var name = ""
      a[RuntimeException] should be thrownBy IntegrationCorpusDatabase.withDatabase(uri, "drop-probe-failing") { database =>
        name = database.name
        seed(client, database.name)
        throw new RuntimeException("the body failed")
      }

      name should not be empty
      withClue(s"$name survived a failing body — the drop was not in a finally: ")(
        databaseNames(client) should not contain name)
    } finally client.close()
  }

  it should "keep the configured database as its prefix, so the throwaway guard still recognises it" in {
    val base = Env.get("MONGODB_DB").getOrElse("kinowo")
    IntegrationCorpusDatabase.named("drop-probe") shouldBe s"${base}_drop-probe"
  }
}
