package services.movies

import integration.CountingSlotsRepository
import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{MongoClient, MongoDatabase}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

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

  /** The sentinels, as (title, year) — the pair both `upsert` and `delete` are addressed
   *  by, so the teardown cannot drift from what the setup wrote. */
  private val sentinels: Seq[(String, Option[Int])] = (1 to 5).map(n => (s"__scanpaging-$n", Some(1900 + n)))

  it should "page the side-collection reads instead of preloading them whole" in {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val screenings = new MongoScreeningsRepository(Some(db))
    val realSlots  = new MongoSlotsRepository(Some(db))
    val slots      = new CountingSlotsRepository(realSlots)
    // batchSize 2 so a handful of sentinels spans several pages
    val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings),
      slots = Some(slots), findAllBatchSize = 2, normalizer = titleNormalizer)
    try {
      sentinels.zipWithIndex.foreach { case ((title, year), index) =>
        repository.upsert(title, year, MovieRecord(tmdbId = Some(6001 + index),
          data = Map[Source, SourceData](Multikino -> SourceData(
            title = Some(s"scan ${index + 1}"), showtimes = Seq(Showtime(when, None))))))
      }

      slots.reset()
      var seen = 0
      // Recognise the sentinels by tmdbId, not by the derived title: the title comes from
      // the stitched slot ("Scan 1"), not from the `_id` prefix, and tying the count to
      // either spelling makes this paging spec fail for a naming reason.
      val complete = repository.foreachRecord(r => if (r.record.tmdbId.exists(t => t > 6000 && t <= 6005)) seen += 1)

      complete shouldBe true
      withClue("the scan preloaded a whole side collection: ")(slots.findAllCalls.get() shouldBe 0)
      withClue("the scan never used the batched per-page read: ")(slots.batchReadCalls.get() should be > 1)
      withClue(s"batched reads=${slots.batchReadCalls.get()} for a 2-row page size: ")(seen should be >= 5)

      // THE TEARDOWN IS PART OF THE TEST. This spec used to clear up with a raw
      // `deleteMany` on `movies` alone, which cannot reach the side collections — so every
      // run stranded five `screenings` rows and five `movie_slots` rows keyed to films that
      // no longer existed. An orphaned side row is a film the next scan's UNION invents,
      // which is the shape of the read-model incidents this corpus has already had. The
      // repository's own `delete` cascades (`screenings.deleteFilm` + `slots.deleteFilm`);
      // asserting on it here is what stops the teardown silently regressing again.
      removeSentinels(repository)
      withClue("the spec stranded screenings orphans: ")(sentinelRows(db, "screenings") shouldBe 0)
      withClue("the spec stranded movie_slots orphans: ")(sentinelRows(db, "movie_slots") shouldBe 0)
      withClue("the spec stranded movies rows: ")(sentinelRows(db, "movies") shouldBe 0)
    } finally {
      // A net for the failure path only — the happy path removed them above and asserted it.
      removeSentinels(repository)
      client.close()
    }
  }

  private def removeSentinels(repository: MongoMovieRepository): Unit =
    sentinels.foreach { case (title, year) => repository.delete(title, year) }

  /** Rows this spec is responsible for, in any of the three collections. The side rows key
   *  on `<filmId>\u001f<cinema>`, so the film id is a PREFIX of their `_id` rather than the
   *  whole of it — one `^scanpaging` match covers all three. */
  private def sentinelRows(db: MongoDatabase, collection: String): Long =
    Await.result(db.getCollection(collection).countDocuments(Filters.regex("_id", "^scanpaging")).toFuture(), 10.seconds)
}
