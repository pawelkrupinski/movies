package services.movies

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
      gate.await(5, TimeUnit.SECONDS)
      if (id == forId) count.incrementAndGet()
      Some(StoredMovieRecord(id, None, MovieRecord(), id = FilmId(id)))
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
    fence:               FilmWriteFence              = new FilmWriteFence()
  ) = new MovieChangeStream(
    source              = source,
    screenings          = screenings,
    slots               = slots,
    reread              = rereadChecked.getOrElse(id => (reread(id), true)),
    fence               = fence,
    resumeToken         = resumeToken,
    changeStreamMetrics = changeStreamMetrics,
    screeningsMetrics   = screeningsMetrics,
    slotsMetrics        = slotsMetrics,
    changeDemandWindow  = ChangeStreamDemand.DefaultWindow,
    clock               = clock,
    rereadRetryMillis   = rereadRetryMillis,
    decodeFailures      = decodeFailures)

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

      delivered.await(5, TimeUnit.SECONDS) shouldBe true
      gotA.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotB.map(_.id) shouldBe Seq(FilmId("film|2024"))
      gotA.head.record.imdbId shouldBe Some("tt0000001") // re-read through the injected reread, once
    } finally { handleA.close(); handleB.close(); under.close() }

    under.isWatching shouldBe false // last listener gone — cursor stopped
  }

  // One document the codec refuses used to END the cursor: the driver decoded post-images, and
  // a stream resuming from a persisted token met the same document on every reopen. Now it is
  // one skipped event — counted, its demand released, its position NOT acknowledged (it was
  // not applied) — and the next event is applied as usual.
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
      token.current shouldBe None                 // …and its position not acknowledged

      source.emit(event("insert", "good|2024", StoredMovieDto.fromDomain("good|2024", MovieRecord(), Instant.EPOCH)))
      delivered.await(5, TimeUnit.SECONDS) shouldBe true
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

      delivered.await(5, TimeUnit.SECONDS) shouldBe true
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
      val raced = marks.poll(5, TimeUnit.SECONDS)
      withClue("a read a local write overtook must be refused: ") {
        fence.ifUndisturbed(film.value, raced)(()) shouldBe false
      }
      source.emit(event("update", film.value, StoredMovieDto.fromDomain(film.value, MovieRecord(), Instant.EPOCH)))
      val quiet = marks.poll(5, TimeUnit.SECONDS)
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

      drained.await(5, TimeUnit.SECONDS) shouldBe true
      reread.get()               shouldBe 1          // one re-read for the whole burst
      dispatched.get()           shouldBe 2          // the gated movies upsert + that one re-read
      slotsSeen.coalesced.get()  shouldBe Burst - 1  // every slot event after the first rode the queued apply
      screeningsSeen.coalesced.get() shouldBe 1      // …and so did the screenings event, counted on ITS cursor
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

      drained.await(5, TimeUnit.SECONDS) shouldBe true
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
      gate.await(5, TimeUnit.SECONDS)
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

      drained.await(5, TimeUnit.SECONDS) shouldBe true
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
    val under  = stream(source, reread = id => { gate.await(5, TimeUnit.SECONDS); Some(recordOf(id)) })
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

      drained.await(5, TimeUnit.SECONDS) shouldBe true
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
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val gate   = new CountDownLatch(1)
    val under  = stream(source, resumeToken = token, reread = id => { gate.await(5, TimeUnit.SECONDS); Some(recordOf(id)) })
    val applied = new CountDownLatch(1)

    val handle = under.watch(_ => applied.countDown(), _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      token.current shouldBe None // delivered, still queued behind the gate — not applied

      gate.countDown()
      applied.await(5, TimeUnit.SECONDS) shouldBe true
      eventually(token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024"))))
    } finally { handle.close(); under.close() }
  }

  // An event is APPLIED once its fan-out has run, not once its re-read has: a position that
  // moves before the listeners do is persisted by a shutdown that cuts the fan-out short.
  it should "not move the resume position while the event's fan-out is still running" in {
    val source   = new HandFedSource
    val token    = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val under    = stream(source, resumeToken = token)
    val entered  = new CountDownLatch(1)
    val release  = new CountDownLatch(1)

    val handle = under.watch(_ => { entered.countDown(); release.await(5, TimeUnit.SECONDS) }, _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      entered.await(5, TimeUnit.SECONDS) shouldBe true
      token.current shouldBe None // the listener has not finished — the event is not applied yet
      release.countDown()
      eventually(token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024"))))
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
      delivered.poll(5, TimeUnit.SECONDS) shouldBe "good|2024"
      eventually(token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-good|2024"))))

      source.emit(event("insert", "broken|2024", StoredMovieDto.fromDomain("broken|2024", MovieRecord(), Instant.EPOCH)))
      source.emit(event("insert", "later|2024", StoredMovieDto.fromDomain("later|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(5, TimeUnit.SECONDS) shouldBe "later|2024" // the failed read fanned nothing out

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
        delivered.poll(10, TimeUnit.SECONDS) shouldBe "broken|2024")
      withClue("the position stays held — the retry is not the event, and a restart still replays it: ")(
        token.current shouldBe None)
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
      delivered.poll(10, TimeUnit.SECONDS) shouldBe "broken|2024"
      source.emit(event("insert", "later|2024", StoredMovieDto.fromDomain("later|2024", MovieRecord(), Instant.EPOCH)))
      delivered.poll(5, TimeUnit.SECONDS) shouldBe "later|2024"
      withClue("the failed film is applied, so the next applied event moves the position again: ")(
        eventually(token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-later|2024")))))
    } finally { handle.close(); under.close() }
  }

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
    val under = stream(source, slots = Some(slots),
      rereadChecked = Some(id => if (id == "broken|2024") (None, false) else (Some(recordOf(id)), true)))

    val handle = under.watch(r => delivered.put(r.id.value), _ => ())
    try {
      ring("broken|2024", () => acks.add("broken"))
      ring("later|2024", () => acks.add("later"))
      delivered.poll(5, TimeUnit.SECONDS) shouldBe "later|2024"
      import scala.jdk.CollectionConverters._
      acks.asScala.toSeq shouldBe empty // held: acknowledging "later" would move the position past "broken"
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
    entered.await(5, TimeUnit.SECONDS) shouldBe true

    under.close()
    finished shouldBe true
    token.current shouldBe Some(new BsonDocument("_data", new BsonString("token-film|2024")))
  }

  it should "not re-arm a token that an invalid-token error cleared while its event was still queued" in {
    val source = new HandFedSource
    val token  = new ChangeStreamResumeToken("movies", database = None, enabled = false)
    val gate   = new CountDownLatch(1)
    val under  = stream(source, resumeToken = token, reread = id => { gate.await(5, TimeUnit.SECONDS); Some(recordOf(id)) })
    val applied = new CountDownLatch(1)

    val handle = under.watch(_ => applied.countDown(), _ => ())
    try {
      source.emit(event("insert", "film|2024", StoredMovieDto.fromDomain("film|2024", MovieRecord(), Instant.EPOCH)))
      source.fail(new com.mongodb.MongoCommandException(
        new BsonDocument("ok", new org.bson.BsonInt32(0)).append("code", new org.bson.BsonInt32(286))
          .append("errmsg", new BsonString("ChangeStreamHistoryLost")),
        new com.mongodb.ServerAddress()))

      gate.countDown()
      applied.await(5, TimeUnit.SECONDS) shouldBe true
      token.current shouldBe None
    } finally { handle.close(); under.close() }
  }

  // The side cursors' half of the same rule: each delivered side event hands over an
  // `applied` acknowledgement (which advances THAT cursor's position), and it is called
  // only by the apply the event queued — never at delivery, and never for an event that
  // coalesced onto an apply queued before it (acknowledging that one would move the
  // position past events queued in between).
  it should "acknowledge a side-collection event only when its own apply has run" in {
    val source = new HandFedSource
    @volatile var ring: (String, () => Unit) => Unit = null
    val slots = new InMemorySlotsRepository {
      override def watchApplied(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] = {
        ring = onChange
        Some(new AutoCloseable { override def close(): Unit = () })
      }
    }
    val gate  = new CountDownLatch(1)
    val under = stream(source, slots = Some(slots), reread = id => { gate.await(5, TimeUnit.SECONDS); Some(recordOf(id)) })
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
      drained.await(5, TimeUnit.SECONDS) shouldBe true
      import scala.jdk.CollectionConverters._
      acks.asScala.toSeq shouldBe Seq("first")
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
      delivered.poll(5, TimeUnit.SECONDS) should not be null

      under.liveness.lastDelivered(Movies) shouldBe Some(opened.plusSeconds(60))
      under.liveness.ageSeconds(Movies, opened.plusSeconds(60))  shouldBe 0.0
      under.liveness.ageSeconds(Movies, opened.plusSeconds(600)) shouldBe 540.0 // silent since → still growing
      // The slots cursor delivered nothing, so its age is the movies delivery's neighbour only
      // by coincidence of the clock — it counts from the open, per cursor.
      under.liveness.lastDelivered(Slots) shouldBe None
      under.liveness.ageSeconds(Slots, opened.plusSeconds(600)) shouldBe 600.0

      clock.advanceSeconds(300)
      slots.upsertSlot("film|2024", "Kino␟film", SourceData(title = Some("Film")))
      delivered.poll(5, TimeUnit.SECONDS) should not be null

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
      reread = id => { gate.await(5, TimeUnit.SECONDS); Some(recordOf(id)) })
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
      delivered.poll(5, TimeUnit.SECONDS) should not be null
      delivered.poll(5, TimeUnit.SECONDS) should not be null
      eventually(under.liveness.pendingApplies(Movies) + under.liveness.pendingApplies(Slots) shouldBe 0)
      under.liveness.applyLagSeconds(Movies, clock.instant()) shouldBe 0.0
      under.liveness.applyLagSeconds(Slots, clock.instant())  shouldBe 0.0
    } finally { handle.close(); under.close() }
  }
}
