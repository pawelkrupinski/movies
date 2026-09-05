package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoMuranow, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoMovieRepository, MongoScreeningsRepository,
                        MongoSlotsRepository, StoredMovieRecord}
import tools.Env

/**
 * An `upsert` that leaves a film's showtimes exactly where they were must not rewrite the
 * film's screening rows.
 *
 * `replaceFilm` is ONE round trip, but it carries a `ReplaceOneModel` for every slot of
 * the film plus a delete vector — 471 documents for a film showing across the UK. `upsert`
 * is the whole-record path every scrape merge takes, so a tick re-wrote every screening
 * row of every film it touched, whether or not a single showtime had moved. A film at N
 * venues is written by N venues, so the tick's screening writes were O(N²) in a film's
 * venue count: 53 venues per film on average in the United States against Germany's 16.
 *
 * The slots half of the same method has had this guard since the read-split landed — it
 * pays a second indexed read to get it. The screenings half is FREE: `reStitchChecked`
 * already reads the film's stored screenings to refill cache-stripped slots, and threw
 * that read away. Comparing against it costs nothing and skips the bulk write.
 *
 * WHY A COUNTING DECORATOR AND NOT THE PROFILER. `StagingSiblingProjectionIntegrationSpec`
 * reads Mongo's profiler because the query it guards is issued deep inside a repository
 * with no seam to observe. Here there is one: `screenings` is a constructor parameter, so
 * the call can be counted at the abstraction it is made through — deterministic, and
 * immune to the database-wide profiler races that spec had to take its own database to
 * avoid.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ScreeningsRewriteOnUpsertIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  private val dbName = tools.IntegrationCorpusDatabase.named("screenings-rewrite")
  private val title  = "__screenings-rewrite-sentinel__"
  private val year   = Some(2026)
  private val when   =
    java.time.LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

  /** Two venues, so the rewrite this guards against is a MULTI-row one — a single-slot
   *  film would rewrite one document and prove nothing about the shape that hurts. */
  private def recordShowing(at: java.time.LocalDateTime): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some(title), showtimes = Seq(Showtime(at, None))),
      KinoMuranow -> SourceData(title = Some(title), showtimes = Seq(Showtime(at.plusHours(2), None)))))

  it should "not rewrite a film's screenings when the upsert leaves its showtimes unchanged" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val counting   = new CountingScreeningsRepository(new MongoScreeningsRepository(Some(db)))
    val repository = new MongoMovieRepository(Some(db),
      screenings = Some(counting), slots = Some(new MongoSlotsRepository(Some(db))),
      normalizer = titleNormalizer)
    val id         = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val record     = recordShowing(when)
    try {
      // The rows do not exist yet, so this one has to write them.
      repository.upsert(title, year, record)
      withClue("the first write must actually create the film's screening rows: ") {
        counting.replaceFilmCalls.get() shouldBe 1
      }
      counting.findForFilm(id).values.flatten should have size 2

      // The SAME record again. This is what a scrape tick does to a film whose listings
      // did not move: re-merge the record, write it back, showtimes byte-identical.
      repository.upsert(title, year, record)
      withClue("an upsert that leaves every showtime where it was must not rewrite the " +
               "film's screening rows — that write is what makes a tick O(venues^2) in a " +
               "film's venue count: ") {
        counting.replaceFilmCalls.get() shouldBe 1
      }

      // …and the guard must not over-skip. A showtime that genuinely moved still writes,
      // and the stored rows follow it — otherwise "skip when unchanged" would be
      // indistinguishable from "never write again".
      val moved = recordShowing(when.plusDays(1))
      repository.upsert(title, year, moved)
      withClue("a real showtime change must still be written: ") {
        counting.replaceFilmCalls.get() shouldBe 2
      }
      counting.findForFilm(id).values.flatten.map(_.dateTime).toSet shouldBe
        Set(when.plusDays(1), when.plusDays(1).plusHours(2))
    } finally {
      repository.delete(title, year)
      counting.deleteFilm(id)
      client.close()
    }
  }

  // THE ORPHAN PRUNE MUST NOT READ SHOWTIMES. It runs every 30 minutes per country and its
  // whole job is a set difference of ids — but through `foreachRecord` it pulled the entire
  // `screenings` collection through WiredTiger to do it: 177,676 rows and 129 MB across the
  // five countries (2026-09-05), against a 1.07 GB cache, twice an hour, every byte discarded.
  //
  // It still needs the SLOTS: `ReadModelProjection.filmIds` derives a film's display-title
  // variants from its cinemas, so a scan that dropped those too would compute FEWER live ids
  // than the read model holds and delete live cards — which is why the cheap
  // `foreachRecordWithoutShowtimes` is the wrong tool and this asserts the slots are read.
  it should "prune the read model without reading a single showtime" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val counting   = new CountingScreeningsRepository(new MongoScreeningsRepository(Some(db)))
    val countSlots = new CountingSlotsRepository(new MongoSlotsRepository(Some(db)))
    val repository = new MongoMovieRepository(Some(db),
      screenings = Some(counting), slots = Some(countSlots), normalizer = titleNormalizer)
    val readModel  = new services.readmodel.MongoReadModelRepository(Some(db))
    val projector  = new services.readmodel.ReadModelProjector(repository, readModel, readModel)
    try {
      repository.upsert(title, year, recordShowing(when))
      counting.reset(); countSlots.reset()

      projector.pruneOrphans()

      withClue("the prune read the screenings collection, which it never looks at: ") {
        counting.batchReadCalls.get() shouldBe 0
      }
      withClue("the prune must still read the SLOTS — `filmIds` derives a film's cards from " +
               "its cinemas, and a prune computing too few live ids deletes live cards: ") {
        countSlots.batchReadCalls.get() should be > 0
      }

      // …and the expensive sweep, which really does re-project, still gets its showtimes.
      counting.reset()
      projector.reconcile()
      withClue("the full reproject writes showtimes, so it must read them: ") {
        counting.batchReadCalls.get() should be > 0
      }
    } finally { repository.delete(title, year); repository.close(); client.close() }
  }
}
