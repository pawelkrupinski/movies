package integration

import tools.SpecTimeouts

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoMuranow, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.bson.types.ObjectId
import org.mongodb.scala.model.changestream.ChangeStreamDocument
import org.mongodb.scala.{Document, Observer, SingleObservableFuture, Subscription}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord, FilmId, WriteOutcome}

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.Await

/**
 * An `upsert` that changes nothing about a film must not rewrite the film's `movies`
 * document.
 *
 * `upsert` is the whole-record path every scrape merge takes, so each of a film's venues
 * wrote this document once per tick regardless of whether anything had changed — and Mongo
 * does not collapse that. A byte-identical `replaceOne` reports `modifiedCount: 1` and
 * writes a full oplog entry, which the change stream delivers, which re-decodes the film
 * document and re-dispatches it downstream. Measured on the steady-state shape (one film,
 * every one of its venues re-scraping it, nothing moved): 16 oplog entries for a German
 * film, 53 for an American one, all of them no-ops.
 *
 * The sibling guards on the same method — slots, then screenings — already covered their
 * collections, which is why those measured zero while `movies` did not.
 *
 * WHY THE CHANGE STREAM. There is no seam to count on: the `movies` write is issued directly
 * on the collection inside the repository, unlike `screenings`/`slots` which arrive as
 * injected collaborators. So the spec counts what the write costs — its oplog entry, as the
 * database's change stream delivers it — on a database of its own, where every change is
 * this spec's. It used to read the profiler, which missed a write that landed (Main #2164:
 * "must still be written: 0 was not greater than 0", green on the rerun): the profile level
 * is in-memory state of the database, and its entry is a best-effort side write. An oplog
 * entry is the write itself, and the stream delivers it in order before a marker written
 * after it, so reading up to the marker waits for nothing but what happened.
 *
 * Requires MONGODB_URI on a replica set (change streams); skips without one.
 */
class MoviesWriteSkippedWhenUnchangedIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolatedDb = tools.IsolatedMongoDatabase.open(mongoTarget, "movies-write-skip-spec")

  private val db = isolatedDb.database
  override protected def afterAll(): Unit = {
    isolatedDb.drop()
    super.afterAll()
  }

  private val repository = new MongoMovieRepository(Some(db), java.time.Clock.systemUTC(),
    screenings = Some(new MongoScreeningsRepository(Some(db), clock = java.time.Clock.systemUTC(), stopwatch = _root_.tools.Stopwatch.System)),
    slots      = Some(new MongoSlotsRepository(Some(db), clock = java.time.Clock.systemUTC(), stopwatch = _root_.tools.Stopwatch.System)),
    normalizer = titleNormalizer, stopwatch = _root_.tools.Stopwatch.System)

  private val title = "__movies-write-skip-sentinel__"
  private val year  = Some(2026)
  private val when  =
    java.time.LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

  private def recordShowing(at: java.time.LocalDateTime): MovieRecord =
    MovieRecord(tmdbId = Some(4242), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some(title), showtimes = Seq(Showtime(at, None))),
      KinoMuranow -> SourceData(title = Some(title), showtimes = Seq(Showtime(at.plusHours(2), None)))))

  /** `movies` writes `upsert` made: the changes the database's stream delivers from a time
   *  read just before it to a marker written just after it. Its outcome must be `Written` — a
   *  declined or failed upsert would otherwise read as a write the guard skipped. */
  private def moviesWritesDuring(upsert: => WriteOutcome): Int = {
    val session = Await.result(isolatedDb.client.startSession().toFuture(), SpecTimeouts.Io)
    try {
      val markers = db.getCollection[Document]("change_markers")
      Await.result(markers.insertOne(session, Document("at" -> "start")).toFuture(), SpecTimeouts.Io)
      val from = session.getOperationTime
      upsert shouldBe WriteOutcome.Written
      val end = new ObjectId()
      Await.result(markers.insertOne(Document("_id" -> end)).toFuture(), SpecTimeouts.Io)
      val changes = changesUntil(from, end)
      withClue(s"every change in the window, in order: ${changes.mkString(", ")}: ") {
        changes.filterNot(_.endsWith(" change_markers")).foreach(_ should (startWith("insert ") or startWith("replace ") or
          startWith("update ") or startWith("delete ")))
      }
      changes.count(_.endsWith(" movies"))
    } finally session.close()
  }

  /** `<operation> <collection>` of each change on the database from `from` up to the insert
   *  of the marker `end`, in oplog order. */
  private def changesUntil(from: org.bson.BsonTimestamp, end: ObjectId): Seq[String] = {
    val delivered    = new LinkedBlockingQueue[Either[Throwable, ChangeStreamDocument[Document]]]()
    val subscription = new java.util.concurrent.atomic.AtomicReference[Subscription]()
    db.watch[Document]().startAtOperationTime(from).subscribe(new Observer[ChangeStreamDocument[Document]] {
      override def onSubscribe(s: Subscription): Unit = { subscription.set(s); s.request(Long.MaxValue) }
      override def onNext(change: ChangeStreamDocument[Document]): Unit = delivered.put(Right(change))
      override def onError(e: Throwable): Unit = delivered.put(Left(e))
      override def onComplete(): Unit = ()
    })
    try {
      val seen = Seq.newBuilder[String]
      var done = false
      while (!done) {
        delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) match {
          case null          => fail(s"the change stream did not deliver the end marker within ${SpecTimeouts.Io}; saw ${seen.result()}")
          case Left(failure) => throw failure
          case Right(change) =>
            val collection = Option(change.getNamespace).map(_.getCollectionName).getOrElse("-")
            seen += s"${change.getOperationTypeString} $collection"
            done = collection == "change_markers" &&
              Option(change.getDocumentKey).exists(_.get("_id") == new org.bson.BsonObjectId(end))
        }
      }
      seen.result()
    } finally Option(subscription.get).foreach(_.unsubscribe())
  }

  "an upsert that changes nothing" should "not rewrite the film's movies document" in {
    val id     = StoredMovieRecord.keyFor(title, year, titleNormalizer)
    val record = recordShowing(when)
    try {
      // The film does not exist yet, so this one has to write it.
      withClue("the first write must actually create the film document: ") {
        moviesWritesDuring(repository.upsert(title, year, record)) should be > 0
      }

      // The SAME record again — what a scrape tick does to a film nothing has changed
      // about. Every venue of the film takes this path, once each, every tick.
      withClue("a re-merge that changes nothing must not rewrite the document — each " +
               "rewrite is an oplog entry, a change-stream delivery and a re-decode of " +
               "the film: ") {
        moviesWritesDuring(repository.upsert(title, year, record)) shouldBe 0
      }

      // A SHOWTIME change writes nothing here either — and that is correct, not the guard
      // over-skipping. Under the read-split the `movies` document carries no showtimes and,
      // once its slots have landed, no `sourceData`; the change belongs to `screenings`.
      // Asserted rather than assumed, because it is the reason nearly every `movies` write
      // the scrape path made was redundant: the collection holds none of what a scrape
      // usually changes.
      val moved = recordShowing(when.plusDays(1))
      withClue("a showtimes-only change belongs to `screenings`, not `movies`: ") {
        moviesWritesDuring(repository.upsert(title, year, moved)) shouldBe 0
      }
      repository.findByIdChecked(FilmId(id)).answered.map(_.record.data.values.flatMap(_.showtimes).map(_.dateTime).toSet)
        .getOrElse(Set.empty) shouldBe Set(when.plusDays(1), when.plusDays(1).plusHours(2))

      // …and the guard must not over-skip. A change to a field `movies` DOES own still
      // writes, and is stored — otherwise "skip when unchanged" would be indistinguishable
      // from "never write again".
      val rated = moved.copy(imdbRating = Some(7.4))
      withClue("a change to a field the movies document owns must still be written: ") {
        moviesWritesDuring(repository.upsert(title, year, rated)) should be > 0
      }
      repository.findByIdChecked(FilmId(id)).answered.flatMap(_.record.imdbRating) shouldBe Some(7.4)
    } finally repository.delete(title, year)
  }
}
