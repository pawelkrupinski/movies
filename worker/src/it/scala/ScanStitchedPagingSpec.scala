package services.movies

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.MongoClient
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * `foreachRecord` promises its callers it never holds more than one page of the corpus.
 * That promise quietly died when the bulk of a row moved into the side collections:
 * `scanStitched` preloaded `screenings` AND `movie_slots` whole before paging `movies`, so
 * the scan's peak heap was the size of those collections (7.5 MB each on prod PL, more on
 * UK) regardless of the page size — on a worker with a 320 MB heap and an OOM history.
 *
 * The side rows are now fetched per page, for exactly that page's films. This pins it: the
 * whole-collection reads must not be used by a scan at all, and the batched read must be
 * called once per page.
 */
class ScanStitchedPagingSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val when = java.time.LocalDateTime.now().plusDays(2).withHour(18).withMinute(0).withSecond(0).withNano(0)

  /** Counts which read shape the scan reaches for. */
  private class CountingSlots(inner: SlotsRepository, all: AtomicInteger, batched: AtomicInteger)
    extends SlotsRepository {
    def findForFilmChecked(id: String) = inner.findForFilmChecked(id)
    override def findForFilmsChecked(ids: Set[String]) = { batched.incrementAndGet(); inner.findForFilmsChecked(ids) }
    def findAllChecked() = { all.incrementAndGet(); inner.findAllChecked() }
    def replaceFilm(id: String, s: Map[String, SourceData]) = inner.replaceFilm(id, s)
    def upsertSlot(id: String, k: String, s: SourceData) = inner.upsertSlot(id, k, s)
    def deleteSlot(id: String, k: String) = inner.deleteSlot(id, k)
    def deleteFilm(id: String) = inner.deleteFilm(id)
  }

  it should "page the side-collection reads instead of preloading them whole" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val prefix = "__scanpaging"
    val allReads     = new AtomicInteger(0)
    val batchedReads = new AtomicInteger(0)
    try {
      val screenings = new MongoScreeningsRepository(Some(db))
      val realSlots  = new MongoSlotsRepository(Some(db))
      val slots      = new CountingSlots(realSlots, allReads, batchedReads)
      // batchSize 2 so a handful of sentinels spans several pages
      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings),
        slots = Some(slots), findAllBatchSize = 2)

      (1 to 5).foreach { n =>
        repository.upsert(s"${prefix}-$n", Some(1900 + n), MovieRecord(tmdbId = Some(6000 + n),
          data = Map[Source, SourceData](Multikino -> SourceData(
            title = Some(s"scan $n"), showtimes = Seq(Showtime(when, None))))))
      }

      allReads.set(0); batchedReads.set(0)
      var seen = 0
      // Recognise the sentinels by tmdbId, not by the derived title: the title comes from
      // the stitched slot ("Scan 1"), not from the `_id` prefix, and tying the count to
      // either spelling makes this paging spec fail for a naming reason.
      val complete = repository.foreachRecord(r => if (r.record.tmdbId.exists(t => t > 6000 && t <= 6005)) seen += 1)

      complete shouldBe true
      withClue("the scan preloaded a whole side collection: ")(allReads.get() shouldBe 0)
      withClue("the scan never used the batched per-page read: ")(batchedReads.get() should be > 1)
      withClue(s"batched reads=${batchedReads.get()} for a 2-row page size: ")(seen should be >= 5)
    } finally {
      Await.ready(db.getCollection("movies")
        .deleteMany(Filters.regex("_id", s"^scanpaging")).toFuture(), 10.seconds)
      client.close()
    }
  }
}
