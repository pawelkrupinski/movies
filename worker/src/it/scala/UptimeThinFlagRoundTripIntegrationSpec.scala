package integration

import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import tools.Eventually

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The web process learns that a worker's scrape was `thin` (screenings, but none
 * in the next 72h — see `NearTermProgramme`) only through the bucket the worker
 * writes to Mongo. So the flag has to survive a real flush and a real hydrate:
 * one monitor records and closes (which flushes), a fresh one on the same
 * database hydrates, and the /uptime classifier must still read "thin".
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class UptimeThinFlagRoundTripIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolatedDb = tools.IsolatedMongoDatabase.open(mongoTarget, "uptime-thin-roundtrip-spec")
  private val db         = isolatedDb.database
  private val service    = "__uptime-thin-roundtrip__"
  private var reader: Option[UptimeMonitor] = None

  // Same drop-until-it-stays-dropped cleanup as UptimeTagWriteRollbackIntegrationSpec:
  // a monitor's daemon init thread can recreate the database after a single drop.
  override protected def afterAll(): Unit = {
    reader.foreach(_.close())
    try Eventually.eventually({
      Await.result(db.drop().toFuture(), 5.seconds)
      Await.result(db.listCollectionNames().toFuture(), 5.seconds) shouldBe empty
    }, timeoutMs = 20000, pollMs = 250)
    catch { case _: Throwable => info(s"could not confirm ${db.name} was dropped; sweep kinowo_isolated_* if it lingers") }
    finally isolatedDb.drop()
    super.afterAll()
  }

  "a thin success" should "survive a flush to Mongo and a hydrate into a fresh monitor" in {
    val writer = new UptimeMonitor(Some(db))
    writer.recordSuccess(service, 20L, thin = true)
    writer.close()   // flushes the dirty bucket

    val fresh = new UptimeMonitor(Some(db))
    reader = Some(fresh)
    Eventually.eventually({
      fresh.history(service).map(_.thin) shouldBe Seq(true)
      fresh.recentStatuses(service, 3) shouldBe Seq("thin")
    }, timeoutMs = 20000, pollMs = 200)
  }
}
