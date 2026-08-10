package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{MovieRecord, Multikino, Showtime, Source, SourceData}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import services.readmodel.MongoReadModelRepository
import tools.Env

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * That `scripts.BackfillReadModel`, which PRUNES the read model, reads the corpus the
 * way the serving path does.
 *
 * On 2026-08-10 it did not. Showtimes live in `screenings` and slots in `movie_slots`;
 * `MongoMovieRepository` stitches them back only when handed the repositories that own
 * them, and the script constructed one with neither. Every row came back showtime-less,
 * the projection emitted no screenings, and the prune deleted the live `web_screenings`
 * — Poznań dropped from 209 films to 28. Every unit spec stayed green throughout,
 * because they drive `run` with an in-memory repository that has no side collections to
 * forget.
 *
 * So this asserts through the SEAM THAT BROKE — `BackfillReadModel.corpusReader`, the
 * one construction `main` also uses — rather than restating the wiring here, which would
 * pass just as happily while `main` read unstitched. Same reasoning as
 * `ConvergenceStorageIntegrationSpec`. Needs a real Mongo because the failure lives
 * entirely in the storage split: in memory there is nothing to stitch.
 *
 * Requires MONGODB_URI; skips otherwise. Sentinels are deliberately titled so they do
 * NOT sanitize to `integrationtest…` — `MovieRepositoryIntegrationSpec` purges that
 * prefix in its own before/afterAll and the it/ suites run in parallel against one db.
 */
class BackfillReadModelStitchIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  private val Title = "Backfill Stitch Probe"
  private val Year  = Some(1904)
  private val id    = StoredMovieRecord.idFor(Title, Year, titleNormalizer)

  private val screeningsRepository = new MongoScreeningsRepository(Some(db))
  private val slotsRepository      = new MongoSlotsRepository(Some(db))
  // The film is written through a fully-split repository, so its showtimes land in
  // `screenings` and its slot in `movie_slots` — the shape every prod film has.
  private val writer = new MongoMovieRepository(
    Some(db), screenings = Some(screeningsRepository), slots = Some(slotsRepository),
    normalizer = titleNormalizer
  )
  private val readModel = new MongoReadModelRepository(Some(db))

  private def purge(): Unit = {
    Seq("movies", "movie_slots", "screenings", "web_movies", "web_screenings").foreach { name =>
      val coll = db.getCollection(name)
      Await.ready(coll.deleteMany(Filters.or(
        Filters.regex("_id", s"^$id"), Filters.regex("filmId", s"^$id")
      )).toFuture(), 10.seconds)
    }
  }

  override protected def beforeAll(): Unit = { super.beforeAll(); purge() }
  override protected def afterAll(): Unit =
    try { purge(); readModel.close(); writer.close(); client.close() } finally super.afterAll()

  private val record = MovieRecord(imdbId = Some("tt0000904"), data = Map[Source, SourceData](
    Multikino -> SourceData(title = Some(Title), releaseYear = Year,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 12, 20, 0), bookingUrl = Some("https://book"))))
  ))

  "the corpus reader BackfillReadModel prunes against" should "carry showtimes stitched from the side collections" in {
    writer.upsert(Title, Year, record)
    withClue("premise — the film must be split for this to test anything: ") {
      screeningsRepository.findForFilm(id) should not be empty
    }

    val stitched = scripts.BackfillReadModel.corpusReader(db).findAll()
      .find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id)

    withClue(s"the reader `main` uses returned: ${stitched.map(_.record.data.view.mapValues(_.showtimes.size).toMap)}\n") {
      stitched.map(_.record.cinemaShowings.flatMap(_._2.showtimes)).getOrElse(Nil) should not be empty
    }
  }

  it should "keep a live screening a full run rewrites, rather than pruning it" in {
    writer.upsert(Title, Year, record)
    val reader = scripts.BackfillReadModel.corpusReader(db)

    scripts.BackfillReadModel.run(reader, readModel)
    val afterFirst = readModel.findAllScreenings().filter(_.filmId == id).map(_._id).toSet
    withClue("a stitched run must project this film's screening at all: ")(afterFirst should not be empty)

    // The run that wiped prod was the SECOND kind of run: one over a corpus whose
    // screenings the read model already holds. It must converge, not delete.
    scripts.BackfillReadModel.run(reader, readModel)

    readModel.findAllScreenings().filter(_.filmId == id).map(_._id).toSet shouldBe afterFirst
  }
}
