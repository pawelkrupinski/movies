package integration

import services.movies.ListedShowtimes

import models.{CityScreening, MovieRecord, Multikino, ResolvedMovie, ResolvedRatings, Showtime, Source, SourceData}
import org.mongodb.scala.{Document, MongoDatabase, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ChangeStreamDemand, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.readmodel.{DecodeFailureMetrics, MongoReadModelRepository}
import services.staging.{MongoStagingRepository, StagingRecord}
import tools.{IsolatedMongoDatabase, MalformedChangeEventProbe}

import scala.concurrent.Await
import scala.concurrent.duration._

/** ONE document a watched collection's codec cannot decode must not end that collection's
 *  change stream. Every watcher used to decode inside the driver's cursor, so a `screenings`
 *  row with no `filmId` (the 2026-09-24 itAll flake: a sibling spec's row ended another spec's
 *  stream) ended the cursor — and one resuming from a persisted token met the same document on
 *  every reopen, dead for good and silent. Each case writes the malformed document while the
 *  watcher runs, then a valid row, which must still be delivered. Each runs in its own database. */
class ChangeStreamMalformedDocumentIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {


  private def insertRaw(db: MongoDatabase, collection: String, doc: Document): Unit =
    Await.result(db.getCollection[Document](collection).insertOne(doc).toFuture(), 10.seconds)

  /** `watch(db, failures)` opens the watcher with `failures` as its decode-failure counter;
   *  the malformed document must be counted there, under its collection. */
  private def survives(collection: String)(watch: (MongoDatabase, DecodeFailureMetrics) => (String => Unit) => AutoCloseable)
                      (writeValid: MongoDatabase => Int => String)(malformed: Document): Unit =
    IsolatedMongoDatabase.withDatabase(mongoTarget, s"malformed-$collection") { db =>
      val counted  = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      val failures: DecodeFailureMetrics = c => { counted.add(c); () }
      MalformedChangeEventProbe.failure(watch(db, failures), writeValid(db), () => insertRaw(db, collection, malformed)) shouldBe None
      counted.toArray.toSeq shouldBe Seq(collection)
    }

  private val undecodableFilm =
    Document("_id" -> "__malformed__", "key" -> "malformed|2026", "title" -> "Malformed", "sourceData" -> "not a document")

  "the movies change stream" should "deliver a valid write after a movies document it cannot decode" in
    survives("movies") { (db, failures) => seen =>
      val repo = new MongoMovieRepository(Some(db), normalizer = titleNormalizer, decodeFailures = failures)
      repo.watchChanges(r => seen(r.id.value), _ => ()).get
    } { db =>
      val repo = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
      n => { repo.upsert(s"Valid $n", Some(2026), MovieRecord()); StoredMovieRecord.keyFor(s"Valid $n", Some(2026), titleNormalizer) }
    }(undecodableFilm)

  "the screenings change stream" should "deliver a valid row after a row with no filmId" in
    survives("screenings") { (db, failures) => seen =>
      new MongoScreeningsRepository(Some(db), decodeFailures = failures).watchApplied((filmId, applied) => { seen(filmId); applied() }, ChangeStreamDemand.unbounded).get
    } { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      n => { screenings.upsertSlot(s"film$n", "Cinema", ListedShowtimes(Seq(Showtime(java.time.LocalDateTime.of(2099, 1, 1, 10, 0), None)), None)); s"film$n" }
    }(Document("_id" -> "__malformed__"))

  "the movie_slots change stream" should "deliver a valid row after a row with no filmId" in
    survives("movie_slots") { (db, failures) => seen =>
      new MongoSlotsRepository(Some(db), decodeFailures = failures).watchApplied((filmId, applied) => { seen(filmId); applied() }, ChangeStreamDemand.unbounded).get
    } { db =>
      val slots = new MongoSlotsRepository(Some(db))
      n => { slots.upsertSlot(s"film$n", "Cinema", SourceData(title = Some(s"Valid $n"))); s"film$n" }
    }(Document("_id" -> "__malformed__"))

  private def film(id: String) = ResolvedMovie(_id = id, title = id, originalTitle = None, posterUrl = None,
    fallbackPosterUrls = Nil, runtimeMinutes = None, releaseYear = None, genres = Nil, countries = Nil, directors = Nil,
    cast = Nil, synopsis = None, trailerUrls = Nil, ratings = ResolvedRatings(None, None, None, "", None, "", None, ""),
    weightedRating = 0.0)

  "the web_movies change stream" should "deliver a valid film after a document it cannot decode" in
    survives(MongoReadModelRepository.MoviesCollection) { (db, failures) => seen =>
      new MongoReadModelRepository(Some(db), decodeFailures = failures).watchMovies(m => seen(m._id), _ => (), None).get
    } { db =>
      val readModel = new MongoReadModelRepository(Some(db))
      n => { readModel.upsertMovie(film(s"film$n")); s"film$n" }
    }(Document("_id" -> "__malformed__", "ratings" -> "not a ratings document")) // a missing field defaults; a wrong type cannot

  "the web_screenings change stream" should "deliver a valid screening after a document it cannot decode" in
    survives(MongoReadModelRepository.ScreeningsCollection) { (db, failures) => seen =>
      new MongoReadModelRepository(Some(db), decodeFailures = failures).watchScreenings(s => seen(s._id), _ => (), None).get
    } { db =>
      val readModel = new MongoReadModelRepository(Some(db))
      n => {
        readModel.upsertScreening(CityScreening(_id = s"screening$n", filmId = "film", city = "poznan", cinema = "Cinema",
          filmUrl = None, showtimes = Nil))
        s"screening$n"
      }
    }(Document("_id" -> "__malformed__", "showtimes" -> "not an array"))

  "the pending_movies change stream" should "deliver a valid row after a document it cannot decode" in
    survives("pending_movies") { (db, failures) => seen =>
      new MongoStagingRepository(Some(db), normalizer = titleNormalizer, decodeFailures = failures).watchChanges(r => seen(r.id), _ => ()).get
    } { db =>
      val staging = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)
      n => {
        val title = s"Valid $n"
        staging.upsert(Multikino, title, Some(2026), MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some(title)))))
        StagingRecord.idFor(Multikino, title, Some(2026), titleNormalizer)
      }
    }(undecodableFilm)
}
