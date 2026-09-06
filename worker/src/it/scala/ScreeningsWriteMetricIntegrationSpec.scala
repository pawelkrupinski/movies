package integration

import models.Showtime
import org.mongodb.scala.SingleObservableFuture
import org.mongodb.scala.model.{CreateCollectionOptions, Filters, ValidationOptions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoScreeningsRepository, ScreeningsMetrics, ScreeningsRepository}
import tools.Env

import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A slot write that Mongo REFUSED must not be counted as written.
 *
 * `written` is the redundant-write canary: the documented way to notice the skip-guard has
 * broken is that counter climbing under a flat scrape rate. `replaceFilm` swallows its failure
 * into a `logger.warn` — deliberately, so a screenings problem cannot break the `movies` write —
 * so counting the write BEFORE issuing it meant a bulkWrite timeout or a stepdown forged exactly
 * that signature: thousands of writes reported for rows that never landed, while the collection
 * sat untouched. The one moment the canary is read is the one moment it lied.
 *
 * The refusal is structural rather than a broken connection — the collection carries a validator
 * no screenings document can satisfy — so the client, the database and the repository all stay
 * healthy and only the write fails.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ScreeningsWriteMetricIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val db = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "screenings-write-metric-spec")

  Await.result(
    db.createCollection(
      ScreeningsRepository.Collection,
      CreateCollectionOptions().validationOptions(
        ValidationOptions().validator(Filters.exists("__no_screenings_write_may_satisfy_this__")))
    ).toFuture(),
    30.seconds)

  /** Counts what the repository reports, by outcome. */
  private object Counting extends ScreeningsMetrics {
    private val writes = new ConcurrentHashMap[String, AtomicInteger]()
    def recordChangeEvent(op: String): Unit = ()
    def recordCoalescedChange(): Unit       = ()
    def recordWrite(outcome: String, count: Int): Unit =
      writes.computeIfAbsent(outcome, _ => new AtomicInteger()).addAndGet(count)
    def apply(outcome: String): Int = Option(writes.get(outcome)).map(_.get()).getOrElse(0)
    def snapshot: Map[String, Int]  = writes.asScala.map { case (k, v) => k -> v.get() }.toMap
  }

  private val repository = new MongoScreeningsRepository(Some(db), metrics = Counting)
  private val filmId     = "__screenings-write-metric-sentinel__|2026"
  private val showtimes  = Seq(Showtime(LocalDateTime.of(2026, 8, 1, 20, 0), None))

  override protected def afterAll(): Unit = {
    repository.close()
    tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  "replaceFilm" should "not count a write Mongo refused" in {
    repository.replaceFilm(filmId, Map("Multikino" -> showtimes, "Kino Muranów" -> showtimes))

    // The write really was refused.
    Await.result(db.getCollection(ScreeningsRepository.Collection)
      .countDocuments(Filters.eq("filmId", filmId)).toFuture(), 30.seconds) shouldBe 0L

    // …so nothing may be reported as written. Before the counter moved after the bulkWrite this
    // read 2 — one per row the call intended to write.
    withClue(s"outcomes: ${Counting.snapshot} — ") {
      Counting(ScreeningsMetrics.Outcome.Written) shouldBe 0
    }
  }
}
