package services.movies

import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.MovieRecord
import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.{Observer, Subscription}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.collection.mutable

/**
 * The `movies` change-stream subscription, driven through a hand-fed source instead of a
 * replica set: the extracted class — not the repository — is what decodes an event once,
 * fans it out to every listener, and turns a post-image-less DELETE into its `_id`.
 * (The same contract against real Mongo is `MovieRepositoryIntegrationSpec`.)
 */
class MovieChangeStreamSpec extends AnyFlatSpec with Matchers {

  /** Hands the observer back so the spec can push events; counts the opens, since ONE
   *  shared cursor for any number of listeners is the point of the fan-out. */
  private final class HandFedSource extends MovieChangeStream.Source {
    val opens    = mutable.Buffer.empty[Option[BsonDocument]]
    private var observer: Observer[ChangeStreamDocument[StoredMovieDto]] = null
    override def open(resumeAfter: Option[BsonDocument], o: Observer[ChangeStreamDocument[StoredMovieDto]]): Unit = {
      opens += resumeAfter
      observer = o
      o.onSubscribe(new Subscription {
        override def request(n: Long): Unit  = ()
        override def unsubscribe(): Unit     = ()
        override def isUnsubscribed: Boolean = false
      })
    }
    def emit(change: ChangeStreamDocument[StoredMovieDto]): Unit = observer.onNext(change)
  }

  private def event(op: String, id: String, fullDocument: StoredMovieDto) =
    new ChangeStreamDocument[StoredMovieDto](op, new BsonDocument("_data", new BsonString(s"token-$id")),
      null, null, null, fullDocument, null, new BsonDocument("_id", new BsonString(id)),
      null, null, null, null, null, null, null)

  private def stream(source: HandFedSource) = new MovieChangeStream(
    source              = source,
    screenings          = None,
    decode              = dto => Some(StoredMovieRecord(dto._id, None, MovieRecord(imdbId = dto.imdbId), id = FilmId(dto._id))),
    reread              = _ => None,
    resumeToken         = new ChangeStreamResumeToken("movies", database = None, enabled = false),
    changeStreamMetrics = ChangeStreamMetrics.noop,
    screeningsMetrics   = ScreeningsMetrics.noop,
    changeDemandWindow  = ChangeStreamDemand.DefaultWindow)

  "MovieChangeStream" should "open one cursor for two listeners and fan each decoded upsert out to both" in {
    val source = new HandFedSource
    val under  = stream(source)
    val gotA, gotB = mutable.Buffer.empty[StoredMovieRecord]
    val delivered  = new CountDownLatch(2)

    val handleA = under.watch(r => { gotA += r; delivered.countDown() }, _ => ())
    val handleB = under.watch(r => { gotB += r; delivered.countDown() }, _ => ())
    try {
      source.opens shouldBe Seq(None) // one shared cursor, opened at "now" with no persisted token
      under.isWatching shouldBe true

      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(imdbId = Some("tt0000001")), Instant.EPOCH)))

      delivered.await(5, TimeUnit.SECONDS) shouldBe true
      gotA.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotB.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotA.head.record.imdbId shouldBe Some("tt0000001") // decoded through the injected decode, once
    } finally { handleA.close(); handleB.close(); under.close() }

    under.isWatching shouldBe false // last listener gone — cursor stopped
  }

  it should "surface a delete, which carries no post-image, to every listener by its _id" in {
    val source = new HandFedSource
    val under  = stream(source)
    val deletedA, deletedB = mutable.Buffer.empty[String]
    val delivered          = new CountDownLatch(2)

    val handleA = under.watch(_ => (), id => { deletedA += id; delivered.countDown() })
    val handleB = under.watch(_ => (), id => { deletedB += id; delivered.countDown() })
    try {
      source.emit(event("delete", "film|2024", fullDocument = null))

      delivered.await(5, TimeUnit.SECONDS) shouldBe true
      deletedA shouldBe Seq("film|2024")
      deletedB shouldBe Seq("film|2024")
    } finally { handleA.close(); handleB.close(); under.close() }
  }
}
