import tools.SpecTimeouts
import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.IntegrationCorpusDatabase

import scala.concurrent.Await

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
class IntegrationCorpusDatabaseIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {


  private def databaseNames(client: MongoClient): Seq[String] =
    Await.result(client.listDatabaseNames().toFuture(), SpecTimeouts.Io)

  /** Materialise the database — Mongo does not create one until something is written. */
  private def seed(client: MongoClient, name: String): Unit =
    Await.result(client.getDatabase(name).getCollection("probe").insertOne(Document("_id" -> "sentinel")).toFuture(), SpecTimeouts.Io)

  "a corpus database" should "be dropped by the time its scope returns" in {
    val client = MongoClient(mongoTarget.uri.value)
    try {
      val name = IntegrationCorpusDatabase.withDatabase(mongoTarget, "drop-probe") { database =>
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
    val client = MongoClient(mongoTarget.uri.value)
    try {
      var name = ""
      a[RuntimeException] should be thrownBy IntegrationCorpusDatabase.withDatabase(mongoTarget, "drop-probe-failing") { database =>
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
    val base = mongoTarget.databasePrefix.value
    IntegrationCorpusDatabase.named(mongoTarget, "drop-probe") should startWith(s"${base}_drop-probe_")
  }

  it should "be this run's own, so a second run started with the same MONGODB_DB cannot drop it mid-test" in {
    IntegrationCorpusDatabase.named(mongoTarget, "drop-probe") should endWith(s"_pid${ProcessHandle.current().pid()}")
    IntegrationCorpusDatabase.named(mongoTarget, "drop-probe") shouldBe IntegrationCorpusDatabase.named(mongoTarget, "drop-probe")
  }

  // A run that is killed never reaches its `finally`, and a later run never generates its pid-scoped
  // name again: 351 such databases had piled up on the local server by 2026-10-04.
  it should "reclaim a killed run's databases on the next scope it opens, and leave a live run's alone" in {
    val client = MongoClient(mongoTarget.uri.value)
    try {
      val ended = new ProcessBuilder("true").start()
      ended.waitFor()
      val orphan = s"${mongoTarget.databasePrefix.value}_sweep-probe_pid${ended.pid()}"
      val live   = IntegrationCorpusDatabase.named(mongoTarget, "sweep-probe-live")
      Seq(orphan, live).foreach(seed(client, _))
      try {
        IntegrationCorpusDatabase.withDatabase(mongoTarget, "sweep-probe") { _ => () }
        val names = databaseNames(client)
        withClue(s"$orphan belongs to an ended run: ")(names should not contain orphan)
        withClue(s"$live belongs to this, live, run: ")(names should contain(live))
      } finally Seq(orphan, live).foreach(name => Await.result(client.getDatabase(name).drop().toFuture(), SpecTimeouts.Io))
    } finally client.close()
  }
}
