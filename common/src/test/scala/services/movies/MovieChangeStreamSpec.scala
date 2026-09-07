package services.movies

import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.{MovieRecord, SourceData}
import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.{Observer, Subscription}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/**
 * The `movies` change-stream subscription, driven through a hand-fed source instead of a
 * replica set: the extracted class — not the repository — is what decodes an event once,
 * fans it out to every listener, and turns a post-image-less DELETE into its `_id`.
 * (The same contract against real Mongo is `MovieRepositoryIntegrationSpec`.)
 *
 * The side cursors are driven through the in-memory stores, whose `watch` rings
 * synchronously: a `movie_slots` or `screenings` change must re-read the film and fan it
 * out as an upsert, and a burst on one film must collapse onto one re-read.
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

  /** Counts what a side cursor's apply coalesced away — `ScreeningsMetrics` so one class
   *  serves both cursor parameters. */
  private final class RecordingSideMetrics extends ScreeningsMetrics {
    val coalesced = new AtomicInteger(0)
    def recordChangeEvent(op: String): Unit            = ()
    def recordWrite(outcome: String, count: Int): Unit = ()
    def recordCoalescedChange(): Unit                  = coalesced.incrementAndGet()
  }

  private def event(op: String, id: String, fullDocument: StoredMovieDto) =
    new ChangeStreamDocument[StoredMovieDto](op, new BsonDocument("_data", new BsonString(s"token-$id")),
      null, null, null, fullDocument, null, new BsonDocument("_id", new BsonString(id)),
      null, null, null, null, null, null, null)

  private def decodeOf(dto: StoredMovieDto): Option[StoredMovieRecord] =
    Some(StoredMovieRecord(dto._id, None, MovieRecord(imdbId = dto.imdbId), id = FilmId(dto._id)))

  private def stream(
    source:            HandFedSource,
    screenings:        Option[ScreeningsRepository]        = None,
    slots:             Option[SlotsRepository]             = None,
    decode:            StoredMovieDto => Option[StoredMovieRecord] = decodeOf,
    reread:            String => Option[StoredMovieRecord] = _ => None,
    screeningsMetrics: SideCollectionChangeMetrics         = ScreeningsMetrics.noop,
    slotsMetrics:      SideCollectionChangeMetrics         = SideCollectionChangeMetrics.noop
  ) = new MovieChangeStream(
    source              = source,
    screenings          = screenings,
    slots               = slots,
    decode              = decode,
    reread              = reread,
    resumeToken         = new ChangeStreamResumeToken("movies", database = None, enabled = false),
    changeStreamMetrics = ChangeStreamMetrics.noop,
    screeningsMetrics   = screeningsMetrics,
    slotsMetrics        = slotsMetrics,
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

  // THE PROD DEFECT (2026-09-07): a venue's slot row lands in `movie_slots` after the film's
  // last projection, with no `movies` write (the document is unchanged) and no `screenings`
  // write (that row landed earlier) — and nothing re-projected the film. 63 UK and 33 PL
  // (film, venue) pairs, every one a venue missing from the site.
  it should "re-read a film and fan the upsert out when one of its movie_slots rows changes" in {
    val source = new HandFedSource
    val slots  = new InMemorySlotsRepository
    val reread = mutable.Buffer.empty[String]
    val under  = stream(source, slots = Some(slots), reread = id => {
      reread += id
      Some(StoredMovieRecord(id, None, MovieRecord(imdbId = Some("tt0000002")), id = FilmId(id)))
    })
    val got       = mutable.Buffer.empty[StoredMovieRecord]
    val delivered = new CountDownLatch(1)

    val handle = under.watch(r => { got += r; delivered.countDown() }, _ => ())
    try {
      slots.upsertSlot("film|2024", "Kino␟film", SourceData(title = Some("Film")))

      delivered.await(5, TimeUnit.SECONDS) shouldBe true
      reread shouldBe Seq("film|2024")                // the film was re-read by id, once
      got.map(_.id) shouldBe Seq(FilmId("film|2024")) // and fanned out as an upsert
      got.head.record.imdbId shouldBe Some("tt0000002")
    } finally { handle.close(); under.close() }
  }

  // A side cursor rings once per changed DOCUMENT — once per (film, cinema slot) — and every
  // ring would otherwise cost a stitch read plus a full projection OF THE SAME FILM. The
  // apply re-reads the film's CURRENT state, so one read after the burst sees all of it.
  // The slots and screenings cursors share ONE pending set: a film's slot row and its
  // screenings row arriving together are one re-read, and each coalesced event is counted
  // against the cursor that delivered it.
  it should "coalesce a burst of slot changes on one film onto one re-read, shared with the screenings cursor" in {
    val source     = new HandFedSource
    val slots      = new InMemorySlotsRepository
    val screenings = new InMemoryScreeningsRepository
    val slotsSeen, screeningsSeen = new RecordingSideMetrics
    val reread     = new AtomicInteger(0)
    // Hold the apply thread on a `movies` event first, so the whole burst lands while its
    // one apply is still QUEUED — the only state an event can coalesce into. Without the
    // gate how many coalesce would depend on thread timing, not on the mechanism.
    val gate       = new CountDownLatch(1)
    val under      = stream(source, screenings = Some(screenings), slots = Some(slots),
      decode            = dto => { gate.await(5, TimeUnit.SECONDS); decodeOf(dto) },
      reread            = id => { reread.incrementAndGet(); Some(StoredMovieRecord(id, None, MovieRecord(), id = FilmId(id))) },
      screeningsMetrics = screeningsSeen,
      slotsMetrics      = slotsSeen)
    val dispatched = new AtomicInteger(0)
    val drained    = new CountDownLatch(2) // the gated movies upsert, then the one side-collection re-read

    val handle = under.watch(_ => { dispatched.incrementAndGet(); drained.countDown() }, _ => ())
    try {
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))
      val Burst = 20
      (0 until Burst).foreach(i => slots.upsertSlot("film|2024", s"Venue$i␟film", SourceData(title = Some(s"Film $i"))))
      screenings.upsertSlot("film|2024", "Venue0␟film", Seq(models.Showtime(LocalDateTime.of(2099, 1, 1, 20, 0), None)))
      gate.countDown()

      drained.await(5, TimeUnit.SECONDS) shouldBe true
      reread.get()               shouldBe 1          // one re-read for the whole burst
      dispatched.get()           shouldBe 2          // the gated movies upsert + that one re-read
      slotsSeen.coalesced.get()  shouldBe Burst - 1  // every slot event after the first rode the queued apply
      screeningsSeen.coalesced.get() shouldBe 1      // …and so did the screenings event, counted on ITS cursor
    } finally { handle.close(); under.close() }
  }
}
