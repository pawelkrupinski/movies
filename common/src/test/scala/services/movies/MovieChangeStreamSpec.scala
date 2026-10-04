package services.movies

import tools.SpecTimeouts

import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.{MovieRecord, SourceData}
import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.{Observer, Subscription}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, LocalDateTime}
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
class MovieChangeStreamSpec extends AnyFlatSpec with Matchers with org.scalatest.concurrent.Eventually {

  /** Hands the observer back so the spec can push events; counts the opens, since ONE
   *  shared cursor for any number of listeners is the point of the fan-out. */
  private final class HandFedSource extends MovieChangeStream.Source {
    val opens    = mutable.Buffer.empty[Option[BsonDocument]]
    @volatile var unsubscribed = false
    val requested = new java.util.concurrent.atomic.AtomicLong(0)
    private var observer: Observer[ChangeStreamDocument[BsonDocument]] = null
    override def open(resumeAfter: Option[BsonDocument], o: Observer[ChangeStreamDocument[BsonDocument]]): Unit = {
      opens += resumeAfter
      observer = o
      o.onSubscribe(new Subscription {
        override def request(n: Long): Unit  = { requested.addAndGet(n); () }
        override def unsubscribe(): Unit     = unsubscribed = true
        override def isUnsubscribed: Boolean = unsubscribed
      })
    }
    def emit(change: ChangeStreamDocument[BsonDocument]): Unit = observer.onNext(change)
    def fail(e: Throwable): Unit                                  = observer.onError(e)
  }

  /** Counts what a side cursor's apply coalesced away — `ScreeningsMetrics` so one class
   *  serves both cursor parameters. */
  private final class RecordingSideMetrics extends ScreeningsMetrics {
    val coalesced = new AtomicInteger(0)
    def recordChangeEvent(op: String): Unit            = ()
    def recordWrite(outcome: String, count: Int): Unit = ()
    def recordCoalescedChange(): Unit                  = coalesced.incrementAndGet()
  }

  // The source hands post-images over UNDECODED (the stream decodes them), so encode the dto
  // the way the collection stores it.
  private def event(op: String, id: String, fullDocument: StoredMovieDto): ChangeStreamDocument[BsonDocument] =
    rawEvent(op, id, Option(fullDocument).map { dto =>
      val out = new BsonDocument()
      MovieCodecs.registry.get(classOf[StoredMovieDto]).encode(new org.bson.BsonDocumentWriter(out), dto,
        org.bson.codecs.EncoderContext.builder().build())
      out
    }.orNull)

  private def rawEvent(op: String, id: String, fullDocument: BsonDocument) =
    new ChangeStreamDocument[BsonDocument](op, new BsonDocument("_data", new BsonString(s"token-$id")),
      null, null, null, fullDocument, null, new BsonDocument("_id", new BsonString(id)),
      null, null, null, null, null, null, null)

  private def recordOf(id: String, imdbId: Option[String] = None): StoredMovieRecord =
    StoredMovieRecord(id, None, MovieRecord(imdbId = imdbId), id = FilmId(id))

  /** A `reread` that blocks on `gate` before answering — the warm-up/coalescing-window gate
   *  the burst tests below share — and counts calls for `forId` only, so a warm-up call on
   *  an unrelated film doesn't pollute the burst's own count. */
  private def gatedReread(gate: CountDownLatch, count: AtomicInteger, forId: String): String => Option[StoredMovieRecord] =
    id => {
      gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
      if (id == forId) count.incrementAndGet()
      Some(StoredMovieRecord(id, None, MovieRecord(), id = FilmId(id)))
    }

  /** A resume token whose advance stalls, as a loaded machine can stall the apply thread between
   *  the listener it has just run and the position it moves next. A wait on that position must be
   *  a happens-before with the apply thread, not a bet on how long the gap lasts: this is the gap
   *  a 150 ms `eventually` lost under load. */
  private final class StallingResumeToken extends ChangeStreamResumeToken("movies", database = None, enabled = false) {
    override def advance(token: BsonDocument, deliveredAt: Long): Unit = { Thread.sleep(400); super.advance(token, deliveredAt) }
  }

  private def stream(
    source:            HandFedSource,
    screenings:        Option[ScreeningsRepository]        = None,
    slots:             Option[SlotsRepository]             = None,
    reread:            String => Option[StoredMovieRecord] = id => Some(recordOf(id)),
    // The checked form production passes: `(row, read succeeded)`. Defaults to `reread`,
    // which cannot fail.
    rereadChecked:     Option[String => (Option[StoredMovieRecord], Boolean)] = None,
    screeningsMetrics:   SideCollectionChangeMetrics = ScreeningsMetrics.noop,
    slotsMetrics:        SideCollectionChangeMetrics = SideCollectionChangeMetrics.noop,
    changeStreamMetrics: ChangeStreamMetrics         = ChangeStreamMetrics.noop,
    clock:               Clock                       = Clock.systemUTC(),
    decodeFailures:      services.readmodel.DecodeFailureMetrics = services.readmodel.DecodeFailureMetrics.noop,
    resumeToken:         ChangeStreamResumeToken     = new ChangeStreamResumeToken("movies", database = None, enabled = false),
    rereadRetryMillis:   Long                        = 20L,
    fence:               FilmWriteFence              = new FilmWriteFence(),
    debounce:            Option[MovieChangeStream.Debounce] = None,
    readVenues:          Option[(String, Set[models.Cinema]) => Option[VenueSlots]] = None,
    venueWaitMillis:     Long = MovieChangeStream.VenueWaitMillis,
    debounceScheduler:   Option[java.util.concurrent.ScheduledExecutorService] = None
  ) = new MovieChangeStream(
    source              = source,
    screenings          = screenings,
    slots               = slots,
    // The specs say what a re-read found as `(row, readOk)`; the stream takes the outcome it is.
    reread              = id => rereadChecked.getOrElse((id: String) => (reread(id), true))(id) match {
                            case (_, false)    => tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new java.io.IOException(s"$id unreadable")))
                            case (Some(r), _)  => tools.ReadOutcome.Answered(r)
                            case (None, _)     => tools.ReadOutcome.none(id)
                          },
    fence               = fence,
    resumeToken         = resumeToken,
    changeStreamMetrics = changeStreamMetrics,
    screeningsMetrics   = screeningsMetrics,
    slotsMetrics        = slotsMetrics,
    changeDemandWindow  = ChangeStreamDemand.DefaultWindow,
    clock               = clock,
    stamps              = new tools.MonotonicStampSequence(clock),
    rereadRetryMillis   = rereadRetryMillis,
    decodeFailures      = decodeFailures,
    debounce            = debounce,
    readVenues          = readVenues,
    venueRetryMillis    = 20L,
    venueWaitMillis     = venueWaitMillis,
    debounceScheduler   = debounceScheduler.fold(() => tools.DaemonExecutors.scheduler("movie-change-debounce"))(timer => () => timer))

  "MovieChangeStream" should "open one cursor for two listeners and fan each re-read upsert out to both" in {
    val source = new HandFedSource
    val under  = stream(source, reread = id => Some(recordOf(id, imdbId = Some("tt0000001"))))
    val gotA, gotB = mutable.Buffer.empty[StoredMovieRecord]
    val delivered  = new CountDownLatch(2)

    val handleA = under.watch(r => { gotA += r; delivered.countDown() }, _ => ())
    val handleB = under.watch(r => { gotB += r; delivered.countDown() }, _ => ())
    try {
      source.opens shouldBe Seq(None) // one shared cursor, opened at "now" with no persisted token
      under.isWatching shouldBe true

      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))

      delivered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      gotA.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotB.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotA.head.record.imdbId shouldBe Some("tt0000001") // re-read through the injected reread, once
    } finally { handleA.close(); handleB.close(); under.close() }

    under.isWatching shouldBe false // last listener gone — cursor stopped
  }

  // A listener attached before the cursor opens sees every event from its first — what the worker's
  // read-model projector needs from a cursor the cache opens: attached only once its boot reads
  // were done, it missed every event applied in between, and the resume position moved past them
  // (2026-09-30's first-sweep heals, and a Chicago venue served for 22 hours after it closed).
  it should "let a listener attach without opening the cursor, and give it every event once another opens it" in {
    val source   = new HandFedSource
    val under    = stream(source)
    val attached = new java.util.concurrent.LinkedBlockingQueue[FilmId]()
    val early    = under.attachFenced((r, _) => attached.add(r.id), _ => ())
    try {
      source.opens shouldBe empty      // attaching alone opens nothing
      under.isWatching shouldBe false
      val opener = under.watch(_ => (), _ => ())
      try {
        source.opens shouldBe Seq(None)
        source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
        attached.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe FilmId("film|2024")
      } finally opener.close()
      under.isWatching shouldBe true   // the attached listener still holds it open
    } finally { early.close(); under.close() }
    under.isWatching shouldBe false
  }

  it should "open the cursor for its attached listeners when asked, and only once" in {
    val source = new HandFedSource
    val under  = stream(source)
    val early  = under.attachFenced((_, _) => (), _ => ())
    try {
      under.open(); under.open()
      source.opens shouldBe Seq(None)
    } finally { early.close(); under.close() }
  }

  // One document the codec refuses used to END the cursor: the driver decoded post-images, and
  // a stream resuming from a persisted token met the same document on every reopen. Now it is
  // one skipped event — counted, its demand released, and acknowledged at once: there is nothing
  // to apply, a restart would skip it the same way, and left unacknowledged it would hold every
  // later event's position for good ([[AppliedPrefix]]) — and the next event is applied as usual.
  it should "skip a post-image it cannot decode, counting it, and keep applying the events after it" in {
    val source    = new HandFedSource
    val counted   = mutable.Buffer.empty[String]
    val token     = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val under     = stream(source, resumeToken = token, decodeFailures = collection => { counted.synchronized(counted += collection); () })
    val delivered = new CountDownLatch(1)
    val handle    = under.watch(r => if (r.id.value == "good|2024") delivered.countDown(), _ => ())
    try {
      val before = source.requested.get()
      source.emit(rawEvent("insert", "bad|2024", new BsonDocument("_id", new BsonString("bad|2024"))
        .append("sourceData", new BsonString("not a document"))))
      counted.synchronized(counted.toSeq) shouldBe Seq("movies")
      source.requested.get() shouldBe before + 1 // its unit of demand released — no apply will release it
      token.current.map(_.getString("_data").getValue) shouldBe Some("token-bad|2024") // …and it is past

      source.emit(event("insert", "good|2024", StoredMovieDto.fromDomain("good|2024", MovieRecord(), Instant.EPOCH)))
      delivered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    } finally { handle.close(); under.close() }
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

      delivered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      deletedA shouldBe Seq("film|2024")
      deletedB shouldBe Seq("film|2024")
    } finally { handleA.close(); handleB.close(); under.close() }
  }

  // The stream's half of `FilmWriteFence`: the mark is taken BEFORE the re-read, so a local
  // write landing while the film is being read disturbs that read — the cache then keeps its
  // own write instead of rolling it back to the older snapshot (the 2026-09-25
  // `RetryResolveServingIntegrationSpec` flake).
  it should "hand a fenced listener the fence mark taken before its re-read" in {
    val source    = new HandFedSource
    val fence     = new FilmWriteFence()
    val film      = FilmId("film|2024")
    val firstRead = new java.util.concurrent.atomic.AtomicBoolean(true)
    val under     = stream(source, fence = fence, reread = id => {
      if (firstRead.getAndSet(false)) fence.writing(film)(()) // a local write lands mid-read
      Some(recordOf(id))
    })
    val marks  = new java.util.concurrent.LinkedBlockingQueue[java.lang.Long]()
    val handle = under.watchFenced((_, mark) => marks.put(mark), _ => ())
    try {
      source.emit(event("update", film.value, StoredMovieDto.fromDomain(film.value, MovieRecord(), Instant.EPOCH)))
      val raced = marks.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
      withClue("a read a local write overtook must be refused: ") {
        fence.ifUndisturbed(film.value, raced)(()) shouldBe false
      }
      source.emit(event("update", film.value, StoredMovieDto.fromDomain(film.value, MovieRecord(), Instant.EPOCH)))
      val quiet = marks.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
      withClue("a read nothing overtook must apply: ") {
        fence.ifUndisturbed(film.value, quiet)(()) shouldBe true
      }
    } finally { handle.close(); under.close() }
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

      delivered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
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
    // Hold the apply thread on the `movies` event's own re-read first, so the whole burst
    // lands while its one apply is still QUEUED — the only state an event can coalesce into.
    // Without the gate how many coalesce would depend on thread timing, not on the mechanism.
    val gate       = new CountDownLatch(1)
    val under      = stream(source, screenings = Some(screenings), slots = Some(slots),
      reread            = gatedReread(gate, reread, "film|2024"), // counts only the side burst's own re-read
      screeningsMetrics = screeningsSeen,
      slotsMetrics      = slotsSeen)
    val dispatched = new AtomicInteger(0)
    val drained    = new CountDownLatch(2) // the gated movies upsert, then the one side-collection re-read

    val handle = under.watch(_ => { dispatched.incrementAndGet(); drained.countDown() }, _ => ())
    try {
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))
      val Burst = 20
      (0 until Burst).foreach(i => slots.upsertSlot("film|2024", s"Venue$i␟film", SourceData(title = Some(s"Film $i"))))
      screenings.upsertSlot("film|2024", "Venue0␟film", ListedShowtimes(Seq(models.Showtime(LocalDateTime.of(2099, 1, 1, 20, 0), None)), None))
      gate.countDown()

      drained.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      reread.get()               shouldBe 1          // one re-read for the whole burst
      dispatched.get()           shouldBe 2          // the gated movies upsert + that one re-read
      slotsSeen.coalesced.get()  shouldBe Burst - 1  // every slot event after the first rode the queued apply
      screeningsSeen.coalesced.get() shouldBe 1      // …and so did the screenings event, counted on ITS cursor
    } finally { handle.close(); under.close() }
  }

  // Coalescing only while an apply is QUEUED misses a burst whose rows land seconds apart — a
  // venue scrape writing a film's rows, a Flicks venue landing in day-chunks — and each of those
  // events bought a full re-read of the film. The debounce holds the film's re-read until its
  // burst goes quiet, so the rest of the burst finds it pending, whatever the apply thread is doing.
  private def millis(n: Int) = scala.concurrent.duration.Duration(n.toLong, "millis")

  /** Re-reads of one film over `rows` slot writes `spacing` apart, under `debounce`. */
  /** How many times one film is re-read for `rows` slot writes `spacing` ms apart, on a hand-moved
   *  clock: each write is applied before the clock moves, and the debounce's timer fires only as the
   *  clock passes it — so the count is exact, never a race between sleeps and the timer thread. */
  private def spacedBurst(debounce: Option[MovieChangeStream.Debounce], rows: Int, spacing: Int): Int = {
    val clock  = new tools.MutableClock(Instant.parse("2026-01-01T00:00:00Z"))
    val timer  = new tools.ManualScheduler(clock)
    val slots  = new InMemorySlotsRepository
    val reread = new AtomicInteger(0)
    val under  = stream(new HandFedSource, slots = Some(slots), debounce = debounce, clock = clock, debounceScheduler = Some(timer),
      reread = id => { if (id == "film|2024") reread.incrementAndGet(); Some(recordOf(id)) })
    val handle = under.watch(_ => (), _ => ())
    try {
      (0 until rows).foreach { i =>
        slots.upsertSlot("film|2024", s"Venue$i␟film", SourceData(title = Some(s"Film $i")))
        timer.advance(java.time.Duration.ofMillis(spacing.toLong))
        under.awaitQueuedApplies() shouldBe true
      }
      timer.advance(java.time.Duration.ofMillis(debounce.fold(0L)(_.cap.toMillis) + 1))
      under.awaitQueuedApplies() shouldBe true
      reread.get()
    } finally { handle.close(); under.close() }
  }


  it should "re-read a film once for a burst whose rows keep landing within its quiet time, however long it runs" in {
    // 8 rows 150 ms apart: 1.05 s of burst, far past the 300 ms quiet time — each row pushes it on
    spacedBurst(Some(MovieChangeStream.Debounce(millis(300), millis(5000))), rows = 8, spacing = 150) shouldBe 1
  }

  it should "re-read a film per row of a spaced burst when nothing debounces" in {
    spacedBurst(None, rows = 6, spacing = 30) shouldBe 6
  }

  it should "re-read a film whose burst never goes quiet once its cap is reached, not when the burst ends" in {
    // 20 rows 100 ms apart (2 s) under a 600 ms cap: one re-read per 600 ms the burst runs (at 600,
    // 1200 and 1800), then the tail's — never one at the end of the whole burst
    spacedBurst(Some(MovieChangeStream.Debounce(millis(300), millis(600))), rows = 20, spacing = 100) shouldBe 4
  }

  it should "apply a film still held by its debounce when it closes, not drop it" in {
    val slots  = new InMemorySlotsRepository
    val reread = new AtomicInteger(0)
    val hour   = scala.concurrent.duration.Duration(1, "hour")
    val under  = stream(new HandFedSource, slots = Some(slots), debounce = Some(MovieChangeStream.Debounce(hour, hour)),
      reread = id => { reread.incrementAndGet(); Some(recordOf(id)) })
    val handle = under.watch(_ => (), _ => ())
    slots.upsertSlot("film|2024", "Venue0␟film", SourceData(title = Some("Film")))
    under.awaitQueuedApplies() shouldBe true // whatever was handed to the apply thread has run…
    reread.get()    shouldBe 0            // …and nothing was: held
    under.held      shouldBe 1
    handle.close(); under.close()
    reread.get()    shouldBe 1            // released and drained at close
  }

  it should "apply what it holds at once when a caller releases it" in {
    val slots  = new InMemorySlotsRepository
    val reread = new AtomicInteger(0)
    val hour   = scala.concurrent.duration.Duration(1, "hour")
    val under  = stream(new HandFedSource, slots = Some(slots), debounce = Some(MovieChangeStream.Debounce(hour, hour)),
      reread = id => { reread.incrementAndGet(); Some(recordOf(id)) })
    val handle = under.watch(_ => (), _ => ())
    try {
      slots.upsertSlot("film|2024", "Venue0␟film", SourceData(title = Some("Film")))
      under.releaseHeld()
      eventually(reread.get() shouldBe 1)
      under.held shouldBe 0
    } finally { handle.close(); under.close() }
  }

  // A prune sweep's heal verdict (`ReadModelProjector.awaitStreamApplied`) waits until the stream has
  // applied everything up to `liveness.lastTicket`. A re-read the debounce still holds was delivered
  // and is not applied, so it must hold that wait open: when it took no ticket until its hand-off, a
  // sweep healing a film whose burst the debounce was holding called the heal a stream miss, and
  // `ReadModelHealsRecurring` fired in exactly the three debounced countries from the day the
  // debounce shipped (2026-09-30). The hold is not apply LAG, though: that gauge still starts at the
  // hand-off, so its alerts keep their thresholds.
  it should "count a re-read its debounce still holds as not yet applied, until it runs" in {
    val slots  = new InMemorySlotsRepository
    val hour   = scala.concurrent.duration.Duration(1, "hour")
    val under  = stream(new HandFedSource, slots = Some(slots), debounce = Some(MovieChangeStream.Debounce(hour, hour)))
    val handle = under.watch(_ => (), _ => ())
    try {
      slots.upsertSlot("film|2024", "Venue0␟film", SourceData(title = Some("Film")))
      eventually(under.held shouldBe 1)
      val inFlight = under.liveness.lastTicket
      under.liveness.appliedThrough(inFlight) shouldBe false
      under.liveness.pendingApplies(ChangeStreamLiveness.Slots) shouldBe 0
      under.releaseHeld()
      eventually(under.liveness.appliedThrough(inFlight) shouldBe true)
    } finally { handle.close(); under.close() }
  }

  it should "debounce where a country's films' changes cluster — the US, the UK and Germany — and nowhere else" in {
    import models.Country.*
    Seq(UnitedStates, UnitedKingdom, Germany).map(MovieChangeStream.Debounce.forCountry).distinct shouldBe
      Seq(Some(MovieChangeStream.Debounce.Worker))
    Seq(Poland, Spain).flatMap(MovieChangeStream.Debounce.forCountry) shouldBe empty
    models.Country.all.toSet shouldBe Set(UnitedStates, UnitedKingdom, Germany, Poland, Spain) // a new country decides too
  }

  // A change confined to some venues' showtimes is applied from those venues alone when every
  // listener can take it — a wide film's other venues are never read — and whole otherwise.
  private final class VenueWorld(declines: Boolean = false, venueRead: Boolean = true,
                                 notYetFor: Int = 0, venueWaitMillis: Long = MovieChangeStream.VenueWaitMillis) {
    @volatile var ring: (String, () => Unit) => Unit = null
    val screenings = new InMemoryScreeningsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand) = {
        ring = onChange; Some(new AutoCloseable { def close(): Unit = () }) }
    }
    @volatile var slotRing: (String, () => Unit) => Unit = null
    val slots = new InMemorySlotsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand) = {
        slotRing = onChange; Some(new AutoCloseable { def close(): Unit = () }) }
    }
    val rereads, venueReads, venueApplies, upserts = new AtomicInteger(0)
    val applies  = new java.util.concurrent.ConcurrentLinkedQueue[(String, String)]()
    val declinedFor = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val metrics = new ChangeStreamMetrics {
      def recordEvent(op: String): Unit = (); def recordUpdateKind(kind: String): Unit = (); def recordCoalescedChange(): Unit = ()
      override def recordApply(path: String, reason: String): Unit = { applies.add(path -> reason); () }
      override def recordVenueDecline(reason: String): Unit       = { declinedFor.add(reason); () }
    }
    val acks   = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val under  = stream(new HandFedSource, screenings = Some(screenings), slots = Some(slots), changeStreamMetrics = metrics,
      venueWaitMillis = venueWaitMillis,
      reread     = id => { rereads.incrementAndGet(); Some(recordOf(id)) },
      readVenues = Some((id, at) => { venueReads.incrementAndGet(); Option.when(venueRead)(VenueSlots(FilmId(id), at.map(_ -> Nil).toMap)) }))
    val handle = under.watchFenced((_, _) => { upserts.incrementAndGet(); () }, _ => (),
      (_, _) => { val asked = venueApplies.incrementAndGet()
        if (declines) VenueVerdict.Declined("spec") else if (asked <= notYetFor) VenueVerdict.NotYet else VenueVerdict.Applied })
    val venue  = models.KinoApollo
    def row(film: String, at: models.Cinema) = SlotKeyed.idOf(film, s"${at.displayName}${models.CinemaShowing.Separator}film")
    def settle(): Unit = eventually(under.applyBacklog shouldBe 0)
  }

  it should "apply a burst of showtime changes at one venue from that venue alone" in {
    val world = new VenueWorld
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a")); ring(row("film|2024", venue), () => acks.add("b"))
      eventually(acks.size shouldBe 2)
      venueReads.get() should be >= 1; venueApplies.get() shouldBe venueReads.get()
      rereads.get() shouldBe 0;        upserts.get() shouldBe 0
      import scala.jdk.CollectionConverters.*
      applies.asScala.toSet shouldBe Set("venues" -> "applied")
    } finally { handle.close(); under.close() }
  }

  // After a boot the projector has still to learn each film; a change at its venues WAITS for it
  // rather than re-reading the whole film.
  it should "ask a listener not ready for the venues again, and apply them from the venues alone once it is" in {
    val world = new VenueWorld(notYetFor = 3)
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a"))
      eventually(acks.size shouldBe 1)
      venueApplies.get() shouldBe 4; rereads.get() shouldBe 0; upserts.get() shouldBe 0
      import scala.jdk.CollectionConverters.*
      applies.asScala.toSeq shouldBe Seq("venues" -> "applied")
    } finally { handle.close(); under.close() }
  }

  it should "hold the change unacknowledged, and the stream unsettled, while it waits" in {
    val world = new VenueWorld(notYetFor = Int.MaxValue)
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a"))
      eventually(org.scalatest.concurrent.PatienceConfiguration.Timeout(SpecTimeouts.Settle))(venueApplies.get() should be >= 3)
      acks.size shouldBe 0
      under.held should be >= 1
    } finally { handle.close(); under.close() }
  }

  it should "re-read the film whole once the wait for a listener runs out" in {
    val world = new VenueWorld(notYetFor = Int.MaxValue, venueWaitMillis = 200L)
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a"))
      eventually(org.scalatest.concurrent.PatienceConfiguration.Timeout(SpecTimeouts.Settle))(acks.size shouldBe 1)
      rereads.get() shouldBe 1; upserts.get() shouldBe 1
      import scala.jdk.CollectionConverters.*
      applies.asScala.toSeq should contain("film" -> "wait_expired")
    } finally { handle.close(); under.close() }
  }

  it should "re-read the film whole when a listener declines the venues" in {
    val world = new VenueWorld(declines = true)
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a"))
      eventually(acks.size shouldBe 1)
      venueApplies.get() shouldBe 1; rereads.get() shouldBe 1; upserts.get() shouldBe 1
      import scala.jdk.CollectionConverters.*
      applies.asScala.toSeq shouldBe Seq("film" -> "declined")
      declinedFor.asScala.toSeq shouldBe Seq("spec")
    } finally { handle.close(); under.close() }
  }

  it should "re-read the film whole when the venues cannot be read alone" in {
    val world = new VenueWorld(venueRead = false)
    import world.*
    try {
      ring(row("film|2024", venue), () => acks.add("a"))
      eventually(acks.size shouldBe 1)
      venueApplies.get() shouldBe 0; rereads.get() shouldBe 1; upserts.get() shouldBe 1
      import scala.jdk.CollectionConverters.*
      applies.asScala.toSeq shouldBe Seq("film" -> "venue_read_failed")
    } finally { handle.close(); under.close() }
  }

  it should "re-read the film whole when a slot change rides with the showtime changes" in {
    val hour  = scala.concurrent.duration.Duration(1, "hour")
    val world = new VenueWorld
    import world.*
    val held  = stream(new HandFedSource, screenings = Some(screenings), slots = Some(slots), debounce = Some(MovieChangeStream.Debounce(hour, hour)),
      reread = id => { rereads.incrementAndGet(); Some(recordOf(id)) },
      readVenues = Some((id, at) => { venueReads.incrementAndGet(); Some(VenueSlots(FilmId(id), at.map(_ -> Nil).toMap)) }))
    val heldHandle = held.watchFenced((_, _) => { upserts.incrementAndGet(); () }, _ => (), (_, _) => { venueApplies.incrementAndGet(); VenueVerdict.Applied })
    try {
      ring(row("film|2024", venue), () => acks.add("showtimes")); slotRing(row("film|2024", venue), () => acks.add("slot"))
      held.releaseHeld()
      eventually(acks.size shouldBe 2)
      venueReads.get() shouldBe 0; rereads.get() shouldBe 1
    } finally { heldHandle.close(); held.close(); handle.close(); under.close() }
  }

  // RESTART RESILIENCE. A cursor has one resume position and a restart replays only what lies
  // after it. With the debounce a film's re-read finishes out of delivery order — a later event on
  // a quiet film is applied while an earlier one waits out a busy film's burst — and moving the
  // position to each event as it was applied moved it PAST the one still waiting: a crash then
  // resumed after a change that was never applied, and lost it.
  it should "keep its resume position before an event still held, even once a later event is applied" in {
    val source = new HandFedSource
    val slots  = new InMemorySlotsRepository
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val hour   = scala.concurrent.duration.Duration(1, "hour")
    val quiet  = mutable.Buffer.empty[String]
    val under  = stream(source, slots = Some(slots), resumeToken = token,
      debounce = Some(MovieChangeStream.Debounce(hour, hour)))
    val handle = under.watch(r => quiet.synchronized(quiet += r.id.value), _ => ())
    try {
      slots.upsertSlot("busy|2024", "Venue0␟busy", SourceData(title = Some("Busy")))   // holds busy's re-read
      source.emit(event("update", "busy|2024", StoredMovieDto.fromDomain("busy|2024", MovieRecord(), Instant.EPOCH))) // rides it
      source.emit(event("update", "quiet|2024", StoredMovieDto.fromDomain("quiet|2024", MovieRecord(), Instant.EPOCH))) // due at once
      eventually(quiet.synchronized(quiet.toSeq) should contain("quiet|2024"))
      under.awaitQueuedApplies() shouldBe true                      // quiet's apply has moved what it moves
      token.current shouldBe None                                   // NOT past busy's waiting event
      under.releaseHeld()
      eventually(token.current.map(_.getString("_data").getValue) shouldBe Some("token-quiet|2024"))
    } finally { handle.close(); under.close() }
  }

  // THE THIRD CURSOR'S BLIND SPOT (2026-09-15). `dropCinemaSlots` writes `retainedSynopses`
  // to `movies` in the SAME tick it deletes the dropped venue's screenings/movie_slots rows —
  // one logical event, three collections, three cursors. Before this fix the movies cursor
  // bought its own apply regardless of what the side cursors were doing, so this one event
  // cost the film TWO re-projections (one from `movies`, one from the coalesced side burst)
  // instead of one. All three cursors now share the same pending set, and the movies apply's
  // own re-read (not a captured document — see the `reread(dto._id)` fix in MovieChangeStream)
  // covers itself and everything the side burst did in one fresh read.
  it should "coalesce a same-film side-collection burst onto an already-queued movies apply" in {
    val source     = new HandFedSource
    val slots      = new InMemorySlotsRepository
    val screenings = new InMemoryScreeningsRepository
    val slotsSeen, screeningsSeen = new RecordingSideMetrics
    val reread     = new AtomicInteger(0)
    // Hold the SHARED single-threaded apply executor on an UNRELATED film's re-read first —
    // exactly the warm-up the screenings/slots-only burst test above uses. Without it, the
    // "film|2024" movies apply below would remove itself from the pending set (the very first
    // line inside its `applyOffLoop` block, BEFORE its own `reread` even reaches the gate) the
    // instant the single idle executor thread picks the task up — microseconds before this
    // test's main thread gets to call `slots.upsertSlot`/`screenings.upsertSlot`, so the
    // coalescing window would already be closed by the time there is anything to coalesce.
    val gate       = new CountDownLatch(1)
    val under      = stream(source, screenings = Some(screenings), slots = Some(slots),
      reread            = gatedReread(gate, reread, "film|2024"), // counts only film|2024's own re-read
      screeningsMetrics = screeningsSeen,
      slotsMetrics      = slotsSeen)
    val dispatched = new AtomicInteger(0)
    val drained    = new CountDownLatch(2) // the warm-up's own dispatch, then film|2024's one dispatch

    val handle = under.watch(_ => { dispatched.incrementAndGet(); drained.countDown() }, _ => ())
    try {
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))
      // The movies-doc half of the event: `dropCinemaSlots`'s `retainedSynopses` write. Queued
      // behind the warm-up (still gated), so it stays in the pending set — added, not yet
      // removed — for the side burst below to find.
      source.emit(event("update", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      // The SAME tick's side-collection writes — the venue's row actually moving, not noise.
      // `updateIfPresent` writes `movies` BEFORE `screenings`/`movie_slots` (see
      // `MovieRepository.scala`), so the movies event arriving first, as above, is the real order.
      slots.upsertSlot("film|2024", "Venue0␟film", SourceData(title = Some("Film")))
      screenings.upsertSlot("film|2024", "Venue0␟film", ListedShowtimes(Seq(models.Showtime(LocalDateTime.of(2099, 1, 1, 20, 0), None)), None))
      gate.countDown()

      drained.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      dispatched.get()              shouldBe 2 // the warm-up's own dispatch + film|2024's ONE dispatch
      reread.get()                  shouldBe 1 // film|2024's own movies apply re-read covers the whole burst
      slotsSeen.coalesced.get()     shouldBe 1 // the movie_slots write rode the queued movies apply
      screeningsSeen.coalesced.get() shouldBe 1 // …and so did the screenings write
    } finally { handle.close(); under.close() }
  }

  // THE RESIDUAL RISK THE COALESCING COMMENT USED TO ACCEPT AS UNAVOIDABLE (2026-09-20): two
  // SEPARATE real `movies`-doc writes to the same film landing in the same tiny window, where
  // the first one's apply is still queued (added to the pending set, not yet removed) when the
  // second one arrives. The second write must coalesce onto a FRESH read of the film's current
  // state — not the first event's now-stale captured document — or its content silently
  // vanishes until the next real event or the nightly content-check sweep.
  it should "not lose a second movies-doc write that lands while the first one's apply is still queued" in {
    val source        = new HandFedSource
    val latestByFilm  = new java.util.concurrent.ConcurrentHashMap[String, String]()
    val gate          = new CountDownLatch(1)
    // Same warm-up as the tests above: hold the shared single-threaded apply executor on an
    // unrelated film's re-read first, so "film|2024"'s own apply stays QUEUED — added to the
    // pending set, not yet removed — for the second write below to find still pending.
    val under = stream(source, reread = id => {
      gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
      Some(recordOf(id, imdbId = Option(latestByFilm.get(id))))
    })
    val got       = mutable.Buffer.empty[StoredMovieRecord]
    val drained   = new CountDownLatch(2) // the warm-up's own dispatch, then film|2024's one dispatch

    val handle = under.watch(r => { got += r; drained.countDown() }, _ => ())
    try {
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))

      latestByFilm.put("film|2024", "tt0000001")
      source.emit(event("update", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(imdbId = Some("tt0000001")), Instant.EPOCH)))
      // A second, independent movies-doc write for the SAME film, landing while the first is
      // still queued behind the warm-up.
      latestByFilm.put("film|2024", "tt0000002")
      source.emit(event("update", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(imdbId = Some("tt0000002")), Instant.EPOCH)))
      gate.countDown()

      drained.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      got.filter(_.id == FilmId("film|2024")).map(_.record.imdbId) shouldBe Seq(Some("tt0000002")) // not lost, and not the stale first write
    } finally { handle.close(); under.close() }
  }

  // A DELETE IS A COALESCING BARRIER. Ids are derived from the key a film is created under
  // (`FilmId.fresh`), so a film deleted (merged away, pruned) and then re-created under the same
  // key comes back with the SAME id. If the re-insert coalesced onto an apply queued BEFORE the
  // delete, that apply's fresh read would dispatch the re-created film first and the delete
  // after it — every listener ends on "deleted" for a film that exists (the projector wipes its
  // read-model rows) until an unrelated write or the backstop happens by.
  it should "not let a re-insert coalesce across a delete of the same film" in {
    val source = new HandFedSource
    val gate   = new CountDownLatch(1)
    val under  = stream(source, reread = id => { gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Some(recordOf(id)) })
    val ops     = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val drained = new CountDownLatch(1) // the sentinel: the apply thread is FIFO, so everything before it ran

    val handle = under.watch(r => { ops.add(s"upsert ${r.id}"); if (r.id == FilmId("sentinel|2024")) drained.countDown() },
                             id => ops.add(s"delete $id"))
    try {
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))
      source.emit(event("update", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      source.emit(event("delete", "film|2024", fullDocument = null))
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      source.emit(event("insert", "sentinel|2024", StoredMovieDto.fromDomain("sentinel|2024", MovieRecord(), Instant.EPOCH)))
      gate.countDown()

      drained.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      import scala.jdk.CollectionConverters._
      ops.asScala.filter(_.endsWith("film|2024")).last shouldBe "upsert film|2024" // the film exists — the last word must say so
    } finally { handle.close(); under.close() }
  }

  // `close()` is "tear the subscription down for good" — the side cursors included. A Mongo
  // side cursor left open past close keeps its own reopen driver alive: once the client is
  // closed under it, every reopen fails and reschedules, for the life of the JVM.
  it should "close the side cursors and the movies subscription when the stream is closed" in {
    val source = new HandFedSource
    val closedSides = mutable.Buffer.empty[String]
    val slots = new InMemorySlotsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
        Some(new AutoCloseable { override def close(): Unit = closedSides += "slots" })
    }
    val screenings = new InMemoryScreeningsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
        Some(new AutoCloseable { override def close(): Unit = closedSides += "screenings" })
    }
    val under = stream(source, screenings = Some(screenings), slots = Some(slots))
    under.watch(_ => (), _ => ()) // a listener still attached: close must not rely on it detaching

    under.close()

    closedSides.toSet shouldBe Set("slots", "screenings")
    source.unsubscribed shouldBe true
    under.isWatching shouldBe false
  }

  // THE RESUME POSITION IS WHAT HAS BEEN APPLIED, NOT WHAT HAS BEEN DELIVERED. The apply
  // queue can hold a window's worth of delivered events per cursor; a position persisted at
  // delivery (a throttled save, or the forced one at shutdown) points past every one of them,
  // so a restart resumes after events that were never applied — and never replays them.
  it should "advance the resume position only once the event's apply has run" in {
    val source = new HandFedSource
    val token  = new StallingResumeToken
    val gate   = new CountDownLatch(1)
    val under  = stream(source, resumeToken = token, reread = id => { gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Some(recordOf(id)) })
    val applied = new CountDownLatch(1)

    val handle = under.watch(_ => applied.countDown(), _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      token.current shouldBe None // delivered, still queued behind the gate — not applied

      gate.countDown()
      applied.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      under.awaitQueuedApplies() shouldBe true
      token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024")))
    } finally { handle.close(); under.close() }
  }

  // A saved position that could not be READ (Mongo slow or briefly gone) opened the cursor at
  // "now", past every change since the last save — recovered only by the 6-hour backstop. A
  // failed read is retried on the reopen backoff instead, and the cursor opens from the position.
  it should "retry an open whose saved position could not be read, rather than open past it at now" in {
    val saved  = new BsonDocument("_data", new BsonString("saved-position"))
    val reads  = new AtomicInteger(0)
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false) {
      override def load(): tools.ReadOutcome[BsonDocument] =
        if (reads.incrementAndGet() == 1) tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new com.mongodb.MongoTimeoutException("slow")))
        else tools.ReadOutcome.Answered(saved)
    }
    val source = new HandFedSource
    val under  = stream(source, resumeToken = token)
    val handle = under.watch(_ => (), _ => ())
    try {
      source.opens shouldBe empty
      withClue("the deferred open never retried: ")(_root_.tools.Eventually.poll()(source.opens.nonEmpty) shouldBe true)
      source.opens.toSeq shouldBe Seq(Some(saved))
    } finally { handle.close(); under.close() }
  }

  // The deferrals are meant to be spaced by the reopen backoff (1 s, 5 s, 15 s): the worker's boot
  // registers its consumers back to back, and each registration re-read the unreadable position,
  // spending a deferral in milliseconds — the cursor opened past the saved position at now after
  // ~6 s of a blip instead of ~20 s.
  it should "leave a deferred open to its scheduled reopen when another consumer registers meanwhile" in {
    val reads  = new AtomicInteger(0)
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false) {
      override def load(): tools.ReadOutcome[BsonDocument] = {
        reads.incrementAndGet(); tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new com.mongodb.MongoTimeoutException("slow")))
      }
    }
    val source = new HandFedSource
    val under  = stream(source, resumeToken = token)
    val first  = under.watch(_ => (), _ => ())
    val second = under.watch(_ => (), _ => ())
    try {
      reads.get shouldBe 1
      source.opens shouldBe empty
    } finally { first.close(); second.close(); under.close() }
  }

  // An event is APPLIED once its fan-out has run, not once its re-read has: a position that
  // moves before the listeners do is persisted by a shutdown that cuts the fan-out short.
  it should "not move the resume position while the event's fan-out is still running" in {
    val source   = new HandFedSource
    val token    = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val under    = stream(source, resumeToken = token)
    val entered  = new CountDownLatch(1)
    val release  = new CountDownLatch(1)

    val handle = under.watch(_ => { entered.countDown(); release.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) }, _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      entered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      token.current shouldBe None // the listener has not finished — the event is not applied yet
      release.countDown()
      under.awaitQueuedApplies() shouldBe true
      token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024")))
    } finally { release.countDown(); handle.close(); under.close() }
  }

  // A re-read that FAILED is not a film that is gone (reference_failed_read_is_not_data): the
  // event was not applied, so neither its position nor any LATER one may be persisted — a later
  // acknowledgement moves the same single position past it. Held, a restart replays from before
  // the failure; advanced, it is lost for good.
  it should "hold the resume position once an event's re-read fails, even past later applied events" in {
    val source    = new HandFedSource
    val token     = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val delivered = new java.util.concurrent.LinkedBlockingQueue[String]()
    val under     = stream(source, resumeToken = token,
      rereadChecked = Some(id => if (id == "broken|2024") (None, false) else (Some(recordOf(id)), true)))

    val handle = under.watch(r => delivered.put(r.id.value), _ => ())
    try {
      source.emit(event("insert", "good|2024", StoredMovieDto.fromDomain("good|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "good|2024"
      under.awaitQueuedApplies() shouldBe true
      token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-good|2024")))

      source.emit(event("insert", "broken|2024", StoredMovieDto.fromDomain("broken|2024", MovieRecord(), Instant.EPOCH)))
      source.emit(event("insert", "later|2024", StoredMovieDto.fromDomain("later|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "later|2024" // the failed read fanned nothing out

      token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-good|2024")))
    } finally { handle.close(); under.close() }
  }

  // Held is not applied. The position waits for a restart to replay the change, but a worker
  // runs for days: until then the film's card kept whatever the failed event should have
  // replaced — a showtime change the id-only sweeps never see. So the film is re-read again,
  // later, until a read answers, and only the POSITION stays held.
  it should "re-read a film whose re-read failed again later, and apply it once a read answers" in {
    val source    = new HandFedSource
    val token     = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val delivered = new java.util.concurrent.LinkedBlockingQueue[String]()
    val failures  = new AtomicInteger(0)
    val under     = stream(source, resumeToken = token, rereadChecked = Some { id =>
      // Fails past the apply's own quick retries, then recovers.
      if (id == "broken|2024" && failures.incrementAndGet() <= MovieChangeStream.RereadAttempts + 2) (None, false)
      else (Some(recordOf(id)), true)
    })

    val handle = under.watch(r => delivered.put(r.id.value), _ => ())
    try {
      source.emit(event("insert", "broken|2024", StoredMovieDto.fromDomain("broken|2024", MovieRecord(), Instant.EPOCH)))
      withClue("the failed film must reach the listeners once a later read answers: ")(
        delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "broken|2024")
      withClue("the retry read the film's current state, so the event is applied and the position may pass it: ")(
        eventually(token.current.map(_.getString("_data").getValue) shouldBe Some("token-broken|2024")))
    } finally { handle.close(); under.close() }
  }

  // The hold exists for the failed film alone. Once a later read has applied it, nothing is
  // left for a restart to replay — kept, one blip froze the persisted position for the rest of
  // the process, and the next deploy replayed days of events, or found them out of the oplog.
  it should "release the held position once every film whose re-read failed has been applied" in {
    val source    = new HandFedSource
    val token     = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val delivered = new java.util.concurrent.LinkedBlockingQueue[String]()
    val failures  = new AtomicInteger(0)
    val under     = stream(source, resumeToken = token, rereadChecked = Some { id =>
      if (id == "broken|2024" && failures.incrementAndGet() <= MovieChangeStream.RereadAttempts + 2) (None, false)
      else (Some(recordOf(id)), true)
    })

    val handle = under.watch(r => delivered.put(r.id.value), _ => ())
    try {
      source.emit(event("insert", "broken|2024", StoredMovieDto.fromDomain("broken|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "broken|2024"
      source.emit(event("insert", "later|2024", StoredMovieDto.fromDomain("later|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "later|2024"
      under.awaitQueuedApplies() shouldBe true
      withClue("the failed film is applied, so the next applied event moves the position again: ")(
        token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-later|2024"))))
    } finally { handle.close(); under.close() }
  }

  // Acknowledging "later" is safe: its cursor's position moves only over a contiguous run of
  // acknowledged events ([[AppliedPrefix]]), so it cannot pass the unacknowledged "broken".
  it should "not acknowledge a side-collection event whose re-read failed" in {
    val source = new HandFedSource
    @volatile var ring: (String, () => Unit) => Unit = null
    val slots = new InMemorySlotsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] = {
        ring = onChange
        Some(new AutoCloseable { override def close(): Unit = () })
      }
    }
    val acks      = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val delivered = new java.util.concurrent.LinkedBlockingQueue[String]()
    // Its first apply and two retries (20 ms, then 40 ms later) have each failed every read they made.
    val brokenReads = new CountDownLatch(3 * MovieChangeStream.RereadAttempts)
    val under = stream(source, slots = Some(slots),
      rereadChecked = Some(id =>
        if (id == "broken|2024") { brokenReads.countDown(); (None, false) } else (Some(recordOf(id)), true)))

    val handle = under.watch(r => delivered.put(r.id.value), _ => ())
    try {
      ring("broken|2024", () => acks.add("broken"))
      ring("later|2024", () => acks.add("later"))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "later|2024"
      import scala.jdk.CollectionConverters._
      eventually(acks.asScala.toSeq shouldBe Seq("later"))
      brokenReads.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      under.awaitQueuedApplies() shouldBe true // the second retry's apply has finished
      acks.asScala.toSeq should not contain "broken"
    } finally { handle.close(); under.close() }
  }

  // A clean shutdown persists the final position. Taken while an apply is still running, it
  // either misses that event (saved before it) or — worse, were the token to move first — keeps
  // a position whose event the dying JVM never finished. Close waits for the in-flight apply.
  it should "let an in-flight apply finish before close() returns" in {
    val source   = new HandFedSource
    val token    = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val under    = stream(source, resumeToken = token)
    val entered  = new CountDownLatch(1)
    @volatile var finished = false

    under.watch(_ => { entered.countDown(); Thread.sleep(300); finished = true }, _ => ())
    source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
    entered.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true

    under.close()
    finished shouldBe true
    token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024")))
  }

  it should "not re-arm a token that an invalid-token error cleared while its event was still queued" in {
    val source = new HandFedSource
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val gate   = new CountDownLatch(1)
    val under  = stream(source, resumeToken = token, reread = id => { gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Some(recordOf(id)) })
    val applied = new CountDownLatch(1)

    val handle = under.watch(_ => applied.countDown(), _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      source.fail(new com.mongodb.MongoCommandException(
        new BsonDocument("ok", new org.bson.BsonInt32(0)).append("code", new org.bson.BsonInt32(286))
          .append("errmsg", new BsonString("ChangeStreamHistoryLost")),
        new com.mongodb.ServerAddress()))

      gate.countDown()
      applied.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      token.current shouldBe None
    } finally { handle.close(); under.close() }
  }

  // The side cursors' half of the same rule: each delivered side event hands over an
  // `applied` acknowledgement, and it is called only once an apply that read the film AFTER the
  // event's write has run — never at delivery. An event that rode a queued apply is covered by
  // that apply's read (the apply takes the film's pending entry just before reading), so it is
  // acknowledged with it; its cursor's [[AppliedPrefix]] keeps the order.
  it should "acknowledge a side-collection event only once an apply covering it has run" in {
    val source = new HandFedSource
    @volatile var ring: (String, () => Unit) => Unit = null
    val slots = new InMemorySlotsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] = {
        ring = onChange
        Some(new AutoCloseable { override def close(): Unit = () })
      }
    }
    val gate  = new CountDownLatch(1)
    val under = stream(source, slots = Some(slots), reread = id => { gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Some(recordOf(id)) })
    val acks  = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val drained = new CountDownLatch(1)

    val handle = under.watch(r => if (r.id == FilmId("sentinel|2024")) drained.countDown(), _ => ())
    try {
      // Warm-up: hold the apply thread on an unrelated film so film|2024's apply stays QUEUED.
      source.emit(event("insert", "other|2024", StoredMovieDto.fromDomain("other|2024", MovieRecord(), Instant.EPOCH)))
      ring("film|2024", () => acks.add("first"))
      ring("film|2024", () => acks.add("coalesced"))
      acks.isEmpty shouldBe true // delivered, not applied
      source.emit(event("insert", "sentinel|2024", StoredMovieDto.fromDomain("sentinel|2024", MovieRecord(), Instant.EPOCH)))

      gate.countDown()
      drained.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      import scala.jdk.CollectionConverters._
      acks.asScala.toSeq shouldBe Seq("first", "coalesced")
    } finally { handle.close(); under.close() }
  }

  // THE SILENT CURSOR. A terminal error is reopened on a backoff; a cursor that is OPEN and
  // delivering nothing — a server-side stall, a stale resume position — was detected by nothing:
  // the event counters simply stop moving, which is also what a quiet night looks like. The age
  // of the last DELIVERED event is the signal that tells the two apart, per cursor, and it must
  // keep growing while nothing arrives.
  it should "age each cursor from its last delivered event, growing while silent and reset by a delivery" in {
    import ChangeStreamLiveness.{Movies, Slots}
    val source = new HandFedSource
    val slots  = new InMemorySlotsRepository
    val clock  = new tools.MutableClock(Instant.parse("2026-09-07T10:00:00Z"))
    val opened = clock.instant()
    val under  = stream(source, slots = Some(slots), clock = clock,
      reread = id => Some(StoredMovieRecord(id, None, MovieRecord(), id = FilmId(id))))
    val delivered = new java.util.concurrent.LinkedBlockingQueue[StoredMovieRecord]()

    val handle = under.watch(delivered.put, _ => ())
    try {
      // Nothing delivered yet: the age counts from the stream's creation, not from zero.
      under.liveness.lastDelivered(Movies) shouldBe None
      under.liveness.ageSeconds(Movies, opened.plusSeconds(90)) shouldBe 90.0

      clock.advanceSeconds(60)
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) should not be null

      under.liveness.lastDelivered(Movies) shouldBe Some(opened.plusSeconds(60))
      under.liveness.ageSeconds(Movies, opened.plusSeconds(60))  shouldBe 0.0
      under.liveness.ageSeconds(Movies, opened.plusSeconds(600)) shouldBe 540.0 // silent since → still growing
      // The slots cursor delivered nothing, so its age is the movies delivery's neighbour only
      // by coincidence of the clock — it counts from the open, per cursor.
      under.liveness.lastDelivered(Slots) shouldBe None
      under.liveness.ageSeconds(Slots, opened.plusSeconds(600)) shouldBe 600.0

      clock.advanceSeconds(300)
      slots.upsertSlot("film|2024", "Kino␟film", SourceData(title = Some("Film")))
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) should not be null

      under.liveness.lastDelivered(Slots)  shouldBe Some(opened.plusSeconds(360))
      under.liveness.lastDelivered(Movies) shouldBe Some(opened.plusSeconds(60)) // a slot delivery is not a movies one
    } finally { handle.close(); under.close() }
  }

  // THE APPLY LAG. The resume token used to be saved at DELIVERY, so a restart could skip every
  // event still queued for the apply thread (fixed 2026-09-23) — and nothing showed how far behind
  // that thread ran. Per cursor, what is queued and not yet applied, and how long the OLDEST of it
  // has waited: counted from delivery, still growing while the apply is stuck, zero once it runs.
  it should "count each cursor's queued, unapplied events and age the oldest until its apply runs" in {
    import ChangeStreamLiveness.{Movies, Slots}
    val source = new HandFedSource
    val slots  = new InMemorySlotsRepository
    val clock  = new tools.MutableClock(Instant.parse("2026-09-23T10:00:00Z"))
    val gate   = new CountDownLatch(1)
    val under  = stream(source, slots = Some(slots), clock = clock,
      reread = id => { gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Some(recordOf(id)) })
    val delivered = new java.util.concurrent.LinkedBlockingQueue[StoredMovieRecord]()

    val handle = under.watch(delivered.put, _ => ())
    try {
      under.liveness.pendingApplies(Movies) shouldBe 0
      under.liveness.applyLagSeconds(Movies, clock.instant()) shouldBe 0.0

      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      clock.advanceSeconds(120)
      slots.upsertSlot("other|2024", "Kino␟other", SourceData(title = Some("Other")))
      clock.advanceSeconds(30)

      under.liveness.pendingApplies(Movies) shouldBe 1
      under.liveness.pendingApplies(Slots)  shouldBe 1
      under.liveness.applyLagSeconds(Movies, clock.instant()) shouldBe 150.0 // delivered 150s ago, still not applied
      under.liveness.applyLagSeconds(Slots, clock.instant())  shouldBe 30.0

      gate.countDown()
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) should not be null
      delivered.poll(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) should not be null
      under.awaitQueuedApplies() shouldBe true
      under.liveness.pendingApplies(Movies) + under.liveness.pendingApplies(Slots) shouldBe 0
      under.liveness.applyLagSeconds(Movies, clock.instant()) shouldBe 0.0
      under.liveness.applyLagSeconds(Slots, clock.instant())  shouldBe 0.0
    } finally { handle.close(); under.close() }
  }
}
