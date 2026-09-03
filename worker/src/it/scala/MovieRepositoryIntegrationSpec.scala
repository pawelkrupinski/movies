package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Helios, HeliosOstrowWlkp, KinoMuranow, MovieRecord, Multikino, Showtime, Source, SourceData, Tmdb}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import services.movies.{ChangeStreamMetrics, MongoMovieRepository, StoredMovieRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of MovieRepository against real MongoDB Atlas. Requires MONGODB_URI
 * to be set (in `.env.local` or the environment). Skips otherwise so CI doesn't
 * fail without secrets.
 *
 * Writes a sentinel record under a deterministic id, reads it back, and cleans
 * up. Run-isolated so it won't interfere with the production collection of
 * real movies.
 */
class MovieRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val repository = new MongoMovieRepository(normalizer = titleNormalizer)

  // Every fake imdbId this spec writes. These are the STABLE handle: the worker
  // re-keys a row's `_id` (e.g. settles `__integration-test-dotted-cinema__` to
  // `dotted|1902` off its sourceData title), so an `_id`-only purge can miss a
  // re-keyed sentinel — but `imdbId` never changes.
  private val sentinelImdbIds = Seq(
    "tt0000001", "tt0000002", "tt0000003", "tt0000004", "tt0000006",
    "tt0000005", "tt0000010", "tt0000011", "tt0000012", "tt0000013", "tt0000014", "tt0000015", "tt0000077", "tt0000099",
    "tt0000078", "tt0000079", "tt0000080", "tt0000081", "tt0000024", "tt0000025"
  )

  // Delete every sentinel this spec could have written. Matches BOTH the
  // sanitized `_id` shape the documents are actually stored under (`integrationtest…`
  // — `documentId` strips non-alphanumerics, so the raw `__integration-test-` form is
  // never what lands in Mongo) AND the fake imdbIds (robust to worker re-keying).
  private def purgeSentinels(): Unit = {
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
      .getCollection("movies")
    try Await.ready(
      coll.deleteMany(Filters.or(
        Filters.regex("_id", "^integrationtest"),
        Filters.in("imdbId", sentinelImdbIds*)
      )).toFuture(),
      10.seconds
    ) finally client.close()
  }

  // Purge at the START too, not only at the end: a run interrupted before its
  // `afterAll` (a killed `IntegrationTest/test`, a CI timeout, an OOM) leaves
  // its sentinels behind and nothing else removes them — they strand on /debug
  // as stuck "Dotted (1902)" rows (`dotted|1892` + `integrationtestdotted…`
  // were found sitting in prod). `purgeSentinels` keys off the stable imdbId +
  // sanitized _id, so the next run sweeps a PRIOR run's residue regardless of
  // how that one ended. NOTE: this guards the interrupted-run case, which a
  // completing test can't reach (the assertion that would catch it is in the
  // run that died); the purge mechanism itself is covered by the
  // "purge its sentinels by the sanitized _id" test below.
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    purgeSentinels()
  }

  // Tidy sentinel rows so they don't leak into the production positive cache
  // at the next app startup (the service hydrates *everything* from Mongo).
  override protected def afterAll(): Unit = try {
    purgeSentinels()
    repository.close()
  } finally super.afterAll()

  "MovieRepository" should "be enabled when MONGODB_URI is set" in {
    repository.enabled shouldBe true
  }

  it should "round-trip an MovieRecord: upsert → findAll → match" in {
    val sentinelTitle = "__integration-test-sentinel__"
    val sentinelYear  = Some(1900)
    val toStore = MovieRecord(
      imdbId         = Some("tt0000001"),
      imdbRating     = Some(7.5),
      metascore      = Some(80),
      filmwebUrl     = Some("https://www.filmweb.pl/film/Test-1900-1"),
      filmwebRating  = Some(7.2),
      rottenTomatoes = Some(91),
      tmdbId            = Some(424242),
      metacriticUrl     = Some("https://www.metacritic.com/movie/integration-test"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/integration_test"),
      data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Integration Test")))
    )

    repository.upsert(sentinelTitle, sentinelYear, toStore)

    // Locate by imdbId, not title: the stored document no longer carries a `title`
    // column — `findAll` derives the display title from `sourceData`, and this
    // record has no cinema slot so the derived title is the sanitized _id, not
    // the raw sentinel. imdbId is the stable round-trip handle.
    val all   = repository.findAll()
    val found = all.find(r => r.record.imdbId.contains("tt0000001"))
    found should not be empty
    val e = found.get.record
    e.imdbId         shouldBe Some("tt0000001")
    e.imdbRating     shouldBe Some(7.5)
    e.metascore      shouldBe Some(80)
    e.originalTitle  shouldBe Some("Integration Test")
    e.filmwebUrl     shouldBe Some("https://www.filmweb.pl/film/Test-1900-1")
    e.filmwebRating  shouldBe Some(7.2)
    e.rottenTomatoes shouldBe Some(91)
    e.tmdbId            shouldBe Some(424242)
    e.metacriticUrl     shouldBe Some("https://www.metacritic.com/movie/integration-test")
    e.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/integration_test")
  }

  // The keyset scan behind `findAll` is a plain `find` — unlike `findAllForListing`,
  // which defends server-side with `$ifNull: ["$sourceData", {}]`. So it was the path
  // that met the migration head-on: one `$unset` row in a batch threw
  // `Missing field: sourceData`, the batch failed all its retries, and the scan
  // reported INCOMPLETE — silently, since a failed scan only logs. Written against
  // the real `$unset` rather than a hand-built document so it tracks whatever
  // `RetireEmbeddedSlots` actually leaves behind.
  it should "scan a film whose embedded sourceData the slot migration retired" in {
    val record = MovieRecord(
      imdbId = Some("tt0000078"),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Retired Slots"))))
    repository.upsert("__integration-test-retired-slots__", Some(1903), record)

    val client = MongoClient(Env.get("MONGODB_URI").get)
    try Await.ready(
      client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo")).getCollection("movies")
        .updateOne(Filters.eq("imdbId", "tt0000078"),
          org.mongodb.scala.model.Updates.unset("sourceData")).toFuture(),
      10.seconds)
    finally client.close()

    val found = repository.findAll().find(_.record.imdbId.contains("tt0000078"))
    found should not be empty
    found.get.record.data shouldBe Map.empty  // slots live in `movie_slots` now
  }

  it should "derive a migrated film's title from its movie_slots, not from the empty embedded map" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    // The title a cinema reports, and the `_id` that title sanitizes to. The gap between
    // them is the whole point: recasing the id gives "Allyouneediskill", nothing like it.
    val title  = "All You Need Is Kill"
    val year   = Some(1909)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      // Written through the split repository, so the slots land in `movie_slots` and the
      // `movies` document is left with no `sourceData` of its own — the fully-migrated
      // shape every film converges to.
      split.upsert(title, year, MovieRecord(imdbId = Some("tt0000079"),
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some(title),
          showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 2, 19, 0), None))))))
      withClue("premise — the film must be fully migrated for this to test anything: ")(
        slots.findForFilm(id).keySet shouldBe Set(Multikino.displayName))

      val found = split.findById(id)
      withClue(s"stitched record: ${found.map(r => (r.title, r.record.data.keySet.map(_.displayName)))}\n") {
        found.map(_.record.data.keySet.map(_.displayName)) shouldBe Some(Set(Multikino.displayName))
        found.map(_.title) shouldBe Some(title)
      }
    } finally {
      slots.deleteFilm(id); scr.deleteFilm(id); split.delete(title, year); client.close()
    }
  }

  // `findAllForListing` is the /debug corpus-table read: it strips each source's
  // `showtimes` SERVER-SIDE (an aggregation, since `sourceData`'s dynamic cinema
  // keys defy a plain field-exclusion projection) so the ~58%-of-bytes showtimes
  // never cross the wire. This proves the aggregation strips them yet leaves the
  // rest intact AND still decodes through the normal codec — while `findById`
  // (the lazy per-row details path) keeps the showtimes.
  it should "drop showtimes in findAllForListing but keep them in findById" in {
    import services.movies.StoredMovieRecord
    val title = "__integration-test-listing__"
    val year  = Some(1902)
    val slot  = SourceData(
      title     = Some("Listing Sentinel"),
      showtimes = Seq(
        Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 30), Some("https://book/it-1")),
        Showtime(java.time.LocalDateTime.of(2026, 6, 1, 21, 0),  Some("https://book/it-2"))))
    repository.upsert(title, year, MovieRecord(imdbId = Some("tt0000011"), data = Map[Source, SourceData](Multikino -> slot)))

    val listed = repository.findAllForListing().find(_.record.imdbId.contains("tt0000011"))
    listed should not be empty
    val lslot = listed.get.record.cinemaData(Multikino)
    lslot.showtimes shouldBe empty                 // stripped server-side…
    lslot.title shouldBe Some("Listing Sentinel")  // …but the rest of the slot survives

    // The full-fidelity reads still carry the showtimes (the /debug details path).
    val full = repository.findById(StoredMovieRecord.idOf(listed.get, titleNormalizer)) // imdbId tt0000011 — distinct from the casededupe sentinel
    full.flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes.size) shouldBe Some(2)
  }

  // The /debug live view needs the change stream to surface DELETEs (a merge
  // removes the losing row), not just upserts. A delete carries no post-image,
  // so the impl reads its `documentKey._id` — exercised here against a replica
  // set (CI starts Mongo as a single-node RS; `$changeStream` needs one),
  // the only shape this path is reachable in.
  it should "surface an out-of-band upsert AND delete (by _id) on the change stream" in {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    import services.movies.StoredMovieRecord

    val title = "__integration-test-changestream__"
    val year  = Some(1901)
    val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val gotUpsert = new CountDownLatch(1)
    val gotDelete = new CountDownLatch(1)

    val handle = repository.watchChanges(
      onUpsert = r   => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) gotUpsert.countDown(),
      onDelete = did => if (did == id) gotDelete.countDown()
    )
    handle should not be empty // requires a replica set (a single-node RS counts)

    try {
      Thread.sleep(1500) // let the stream establish before the writes
      repository.upsert(title, year, MovieRecord(imdbId = Some("tt0000099")))
      gotUpsert.await(15, TimeUnit.SECONDS) shouldBe true

      repository.delete(title, year)
      gotDelete.await(15, TimeUnit.SECONDS) shouldBe true
    } finally handle.foreach(_.close())
  }

  // The change-stream apply does a blocking stitch read + the synchronized read-model
  // projection. Running that on the Mongo driver's Netty I/O event loops made the two
  // loops contend the projection monitor and busy-spin their wakeup eventfds (~24cc,
  // ~0 voluntary ctx-switches — proven on-box), flooring the worker's CPU credit. So
  // the fanout listeners must fire on the dedicated `movie-change-apply` thread, NEVER
  // on a Mongo I/O thread. Before the offload the listener ran on the driver thread
  // (nioEventLoop / epollEventLoop / InnocuousThread); this asserts it now doesn't.
  it should "apply change-stream events off the Mongo I/O loop, on the movie-change-apply thread" in {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    import java.util.concurrent.atomic.AtomicReference

    val title       = "__integration-test-apply-thread__"
    val year        = Some(1903)
    val id          = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val applied     = new CountDownLatch(1)
    val applyThread = new AtomicReference[String]("")

    val handle = repository.watchChanges(
      onUpsert = r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) { applyThread.set(Thread.currentThread().getName); applied.countDown() },
      onDelete = _ => ()
    )
    handle should not be empty
    try {
      Thread.sleep(1500)
      repository.upsert(title, year, MovieRecord(imdbId = Some("tt0000077")))
      applied.await(15, TimeUnit.SECONDS) shouldBe true
      applyThread.get              should startWith ("movie-change-apply")
      applyThread.get.toLowerCase  should not include "eventloop"      // not a Netty I/O loop
      applyThread.get              should not include "InnocuousThread" // not the NIO2 async pool
    } finally handle.foreach(_.close())
  }

  // The worker attaches two change-stream consumers (MovieCache + ReadModelProjector).
  // They now share ONE underlying cursor (ChangeStreamFanout) instead of one cursor
  // each — the CPU optimization. Prove it against real Mongo: a single write reaches
  // BOTH listeners, and the shared cursor stays up until the LAST listener detaches.
  it should "feed two listeners from a single shared cursor, stopping it only when the last detaches" in {
    import java.util.concurrent.{CountDownLatch, TimeUnit}

    val title = "__integration-test-shared-cursor__"
    val year  = Some(1902)
    val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val gotA  = new CountDownLatch(1)
    val gotB  = new CountDownLatch(1)

    val handleA = repository.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) gotA.countDown(), _ => ())
    val handleB = repository.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) gotB.countDown(), _ => ())
    handleA should not be empty
    handleB should not be empty
    repository.isWatchingChangeStream shouldBe true

    try {
      Thread.sleep(1500) // let the stream establish before the write
      repository.upsert(title, year, MovieRecord(imdbId = Some("tt0000077")))
      gotA.await(15, TimeUnit.SECONDS) shouldBe true // one write reached BOTH consumers
      gotB.await(15, TimeUnit.SECONDS) shouldBe true

      handleA.foreach(_.close())
      repository.isWatchingChangeStream shouldBe true // B still attached — cursor stays up
    } finally handleB.foreach(_.close())

    repository.isWatchingChangeStream shouldBe false // last listener gone — cursor stopped
  }

  // Resume-token persistence: the shared cursor reopens from the last persisted token
  // (a WORKER RESTART / terminal error) instead of "now", so writes that landed while
  // this process was DOWN are REPLAYED — closing the downtime gap the consumers' periodic
  // backstops (cache rehydrate / projector reconcile) exist for. Simulated with two repo
  // instances sharing the token collection: repo1 sees A then "dies"; B and C are written
  // while nothing watches; a fresh repo2 resumes from the token and replays B and C.
  it should "resume the change stream from the persisted token, replaying events missed while down" in {
    import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}
    import scala.jdk.CollectionConverters._

    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    def clearToken(): Unit = Await.ready(
      db.getCollection("change_stream_tokens").deleteOne(Filters.eq("_id", "movies")).toFuture(), 10.seconds)
    clearToken() // start clean → repo1 opens at "now", not a stale prior-run token

    val repo1   = new MongoMovieRepository(Some(db), persistResumeToken = true, normalizer = titleNormalizer)
    val idA     = StoredMovieRecord.idFor("__integration-test-resume-A__", Some(1909), titleNormalizer)
    val gotA    = new CountDownLatch(1)
    val handle1 = repo1.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == idA) gotA.countDown(), _ => ())
    handle1 should not be empty
    try {
      Thread.sleep(1500) // let the stream establish before the write
      repo1.upsert("__integration-test-resume-A__", Some(1909), MovieRecord(imdbId = Some("tt0000013")))
      gotA.await(15, TimeUnit.SECONDS) shouldBe true
      handle1.foreach(_.close()) // last listener gone → stopWatchingIfIdle force-saves the token (position: after A)

      // "Down": B and C land while nothing is watching the stream.
      repo1.upsert("__integration-test-resume-B__", Some(1909), MovieRecord(imdbId = Some("tt0000013")))
      repo1.upsert("__integration-test-resume-C__", Some(1909), MovieRecord(imdbId = Some("tt0000013")))

      // A fresh process (empty in-memory state) resumes from the persisted token.
      val repo2   = new MongoMovieRepository(Some(db), persistResumeToken = true, normalizer = titleNormalizer)
      val idB     = StoredMovieRecord.idFor("__integration-test-resume-B__", Some(1909), titleNormalizer)
      val idC     = StoredMovieRecord.idFor("__integration-test-resume-C__", Some(1909), titleNormalizer)
      val seen    = ConcurrentHashMap.newKeySet[String]()
      val gotBC   = new CountDownLatch(2)
      val handle2 = repo2.watchChanges(r => {
        val id = StoredMovieRecord.idOf(r, titleNormalizer)
        if ((id == idB || id == idC) && seen.add(id)) gotBC.countDown()
      }, _ => ())
      try {
        // No fresh write: B and C are delivered purely by resuming past the token.
        gotBC.await(15, TimeUnit.SECONDS) shouldBe true
        seen.asScala should contain allOf (idB, idC)
      } finally { handle2.foreach(_.close()); repo2.close() }
    } finally {
      Seq("A", "B", "C").foreach(s => repo1.delete(s"__integration-test-resume-${s}__", Some(1909)))
      clearToken()
      repo1.close(); client.close()
    }
  }

  // THE MIGRATION SHAPE — the one that took the read model down on 2026-08-29. A Mongo
  // dump-and-restore DROPS every collection, so a token persisted before the restore is
  // unusable: resuming from it replays into the drop (which invalidates the cursor), and
  // resuming from the invalidate token itself gets `ChangeStreamFatalError` (280) "cannot
  // resume stream; the resume token was not found". Both halves of the recovery are under
  // test here — recognising that the token must be discarded, and REOPENING the cursor
  // afterwards, which nothing used to do once the boot registrations were spent. Without
  // either, repo2's stream stays dead and D is never delivered: all three workers ran for
  // hours taking zero read-model projections and only prune deletes.
  it should "recover the change stream when a collection drop invalidated the persisted token" in {
    import java.util.concurrent.{CountDownLatch, TimeUnit}

    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    def clearToken(): Unit = Await.ready(
      db.getCollection("change_stream_tokens").deleteOne(Filters.eq("_id", "movies")).toFuture(), 10.seconds)
    clearToken() // start clean → repo1 opens at "now", not a stale prior-run token

    val repo1   = new MongoMovieRepository(Some(db), persistResumeToken = true, normalizer = titleNormalizer)
    val idA     = StoredMovieRecord.idFor("__integration-test-dropped-A__", Some(1911), titleNormalizer)
    val gotA    = new CountDownLatch(1)
    val handle1 = repo1.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == idA) gotA.countDown(), _ => ())
    handle1 should not be empty
    var writer: Thread = null
    try {
      Thread.sleep(1500) // let the stream establish before the write
      repo1.upsert("__integration-test-dropped-A__", Some(1911), MovieRecord(imdbId = Some("tt0000025")))
      gotA.await(15, TimeUnit.SECONDS) shouldBe true
      handle1.foreach(_.close()) // token now persisted at "just after A"
      repo1.close()

      // The restore: drop the watched collection out from under the saved token.
      Await.ready(db.getCollection(services.movies.MovieRepository.Collection).drop().toFuture(), 15.seconds)

      val repo2 = new MongoMovieRepository(Some(db), persistResumeToken = true, normalizer = titleNormalizer)
      val idD   = StoredMovieRecord.idFor("__integration-test-dropped-D__", Some(1911), titleNormalizer)
      val gotD  = new CountDownLatch(1)
      val handle2 = repo2.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == idD) gotD.countDown(), _ => ())
      // Keep writing D while the cursor works its way through invalidate → clear → reopen,
      // so the assertion is "the stream came back", not "it came back within one write".
      // Each pass must CHANGE something. `upsert` skips a write that would store the
      // document Mongo already holds, so a loop re-writing ONE identical record produces a
      // single change event and then silence — and this test needs a STREAM of them, to
      // catch the cursor whenever it finishes working through invalidate → clear → reopen.
      // The moving field is arbitrary; that it moves is the point.
      val tick = new java.util.concurrent.atomic.AtomicInteger(0)
      writer = new Thread(() => try while (!Thread.currentThread().isInterrupted) {
        repo2.upsert("__integration-test-dropped-D__", Some(1911),
          MovieRecord(imdbId = Some("tt0000025"), imdbRating = Some(tick.incrementAndGet() / 10.0)))
        Thread.sleep(2000)
      } catch { case _: InterruptedException => () }) // the finally-interrupt is how this thread ends
      writer.setDaemon(true)
      try {
        writer.start()
        gotD.await(45, TimeUnit.SECONDS) shouldBe true
      } finally { handle2.foreach(_.close()); repo2.close() }
    } finally {
      Option(writer).foreach(_.interrupt())
      val cleanup = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
      try Seq("A", "D").foreach(s => cleanup.delete(s"__integration-test-dropped-${s}__", Some(1911)))
      finally cleanup.close()
      clearToken()
      client.close()
    }
  }

  // The SCREENINGS stream now persists its own resume token too — the asymmetry that
  // kept the full reproject non-redundant: a showtime change writes only `screenings`, so
  // without this a restart (frequent on the worker) dropped the showtime edits made while
  // down and ONLY the projector's whole-corpus reproject caught them. Same two-instance
  // simulation as the movies test: repo1 sees slot A, then "dies"; slots B and C are written
  // while nothing watches; a fresh repo2 resumes from the token and replays B and C.
  it should "resume the screenings change stream from the persisted token, replaying showtime changes missed while down" in {
    import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}
    import java.time.LocalDateTime
    import services.movies.MongoScreeningsRepository
    import scala.jdk.CollectionConverters._

    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    def clearToken(): Unit = Await.ready(
      db.getCollection("change_stream_tokens").deleteOne(Filters.eq("_id", "screenings")).toFuture(), 10.seconds)
    def at(h: Int): Seq[Showtime] = Seq(Showtime(LocalDateTime.of(2099, 1, 1, h, 0), bookingUrl = Some("https://book")))
    clearToken() // start clean → repo1 opens at "now", not a stale prior-run token

    val filmA = "__it-screenings-resume-A__"
    val filmB = "__it-screenings-resume-B__"
    val filmC = "__it-screenings-resume-C__"
    val repo1 = new MongoScreeningsRepository(Some(db), persistResumeToken = true)
    val gotA  = new CountDownLatch(1)
    val handle1 = repo1.watch(fid => if (fid == filmA) gotA.countDown())
    handle1 should not be empty
    try {
      Thread.sleep(1500) // let the stream establish before the write
      repo1.upsertSlot(filmA, "Multikino␟A", at(10))
      gotA.await(15, TimeUnit.SECONDS) shouldBe true
      handle1.foreach(_.close()) // watcher gone → force-saves the token (position: after A)

      // "Down": B and C land while nothing is watching the screenings stream.
      repo1.upsertSlot(filmB, "Multikino␟B", at(11))
      repo1.upsertSlot(filmC, "Multikino␟C", at(12))

      // A fresh process (empty in-memory state) resumes from the persisted token.
      val repo2   = new MongoScreeningsRepository(Some(db), persistResumeToken = true)
      val seen    = ConcurrentHashMap.newKeySet[String]()
      val gotBC   = new CountDownLatch(2)
      val handle2 = repo2.watch(fid => if ((fid == filmB || fid == filmC) && seen.add(fid)) gotBC.countDown())
      try {
        // No fresh write: B and C are delivered purely by resuming past the token.
        gotBC.await(15, TimeUnit.SECONDS) shouldBe true
        seen.asScala should contain allOf (filmB, filmC)
      } finally { handle2.foreach(_.close()); repo2.close() }
    } finally {
      Seq(filmA, filmB, filmC).foreach(repo1.deleteFilm)
      clearToken()
      repo1.close(); client.close()
    }
  }

  // End-to-end: the MovieCache now applies change-stream DELETES incrementally
  // (`applyDelete`), so a removed source row leaves the cache the moment the delete lands
  // — no waiting for the 30-min backstop rehydrate. Real stream against a replica set.
  it should "drop a MovieCache row when its source is deleted on the change stream" in {
    import services.movies.CaffeineMovieCache
    def eventually(timeoutMs: Long)(cond: => Boolean): Boolean = {
      val end = System.currentTimeMillis + timeoutMs
      while (System.currentTimeMillis < end && !cond) Thread.sleep(100)
      cond
    }
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val repo   = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
    val cache  = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    val title  = "__integration-test-cache-delete__"
    val year   = Some(1910)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    def present = cache.snapshot().exists(r => StoredMovieRecord.idOf(r, titleNormalizer) == id)
    try {
      cache.start()
      Thread.sleep(1500) // let the stream establish
      repo.upsert(title, year, MovieRecord(imdbId = Some("tt0000013")))
      eventually(15000)(present) shouldBe true  // applied via the stream (applyUpsert)
      repo.delete(title, year)
      eventually(15000)(!present) shouldBe true  // dropped via applyDelete, not the backstop
    } finally { cache.stop(); repo.delete(title, year); client.close() }
  }

  // The shared cursor's onNext feeds the change-stream stats sink (op + update-field
  // kind). Prove it fires against a real event with a recording sink.
  it should "record change-stream event stats onto the injected sink" in {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    val recorded = scala.collection.mutable.ListBuffer.empty[String]
    val sink = new ChangeStreamMetrics {
      def recordEvent(op: String): Unit        = recorded.synchronized(recorded += op)
      def recordUpdateKind(kind: String): Unit = ()
    }
    val repo  = new MongoMovieRepository(changeStreamMetrics = sink, normalizer = titleNormalizer)
    val title = "__integration-test-changestream-stats__"
    val year  = Some(1903)
    val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val seen  = new CountDownLatch(1)
    val handle = repo.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) seen.countDown(), _ => ())
    handle should not be empty
    try {
      Thread.sleep(1500)
      repo.upsert(title, year, MovieRecord(imdbId = Some("tt0000012"))) // own sentinel — no collision with other specs' imdbId queries
      seen.await(15, TimeUnit.SECONDS) shouldBe true
      recorded.synchronized(recorded.toList) should not be empty // an event was counted for the write
    } finally { handle.foreach(_.close()); repo.close() }
  }

  it should "handle Enrichments with all-None optional fields" in {
    val title = "__integration-test-sparse__"
    val toStore = MovieRecord(
      imdbId         = Some("tt0000002")
    )
    repository.upsert(title, None, toStore)
    val found = repository.findAll().find(r => r.record.imdbId.contains("tt0000002"))
    found should not be empty
    val e = found.get.record
    e.imdbId         shouldBe Some("tt0000002")
    e.imdbRating     shouldBe None
    e.metascore      shouldBe None
    e.originalTitle  shouldBe None
    e.filmwebUrl     shouldBe None
    e.filmwebRating  shouldBe None
    e.rottenTomatoes    shouldBe None
    e.metacriticUrl     shouldBe None
    e.rottenTomatoesUrl shouldBe None
  }

  // Cinema slots are persisted under sourceData.<cinemaName>. Round-trip
  // every Option field plus a co-production country list to confirm decode
  // matches encode for the per-cinema sub-document.
  it should "round-trip a SourceData slot including the production countries" in {
    val title = "__integration-test-sourcedata-country__"
    val year  = Some(2026)
    val slot  = SourceData(
      title          = Some(title),
      originalTitle  = Some("Original"),
      synopsis       = Some("synopsis"),
      cast           = Seq("cast list"),
      director       = Seq("dir"),
      runtimeMinutes = Some(123),
      releaseYear    = Some(2025),
      countries      = Seq("Polska", "Francja"),
      posterUrl      = Some("https://example/poster.jpg"),
      filmUrl        = Some("https://example/film"),
      showtimes      = Seq.empty
    )
    val toStore = MovieRecord(
      imdbId = Some("tt0000003"),
      data   = Map[Source, SourceData](Helios -> slot)
    )
    repository.upsert(title, year, toStore)

    val found = repository.findAll().find(r => r.title == title && r.year == year)
    found should not be empty
    val e = found.get.record
    e.cinemaData.keySet shouldBe Set(Helios)
    e.cinemaData(Helios).countries shouldBe Seq("Polska", "Francja")
    e.cinemaData(Helios).filmUrl shouldBe Some("https://example/film")
    // Merged accessor surfaces the only cinema's countries.
    e.countries shouldBe Seq("Polska", "Francja")
  }

  // Regression: a cinema whose displayName contains a dot ("Helios Ostrów Wlkp.")
  // can't be written via the per-source `$set sourceData.<name>` diff path —
  // MongoDB reads the dot as a nesting separator and rejects the update (code 56,
  // "empty field name"). updateIfPresent must fall back to a full-document replace
  // so the slot still persists. Fails before the fix (the $set is rejected →
  // updateIfPresent returns false and nothing is written).
  it should "persist a per-source slot whose cinema displayName contains a dot" in {
    val title  = "__integration-test-dotted-cinema__"
    val year   = Some(1902)
    val before = MovieRecord(
      imdbId = Some("tt0000004"),
      data   = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dotted"))))
    repository.upsert(title, year, before) // create the row
    val after = before.copy(data = before.data +
      (HeliosOstrowWlkp -> SourceData(title = Some("Dotted"), synopsis = Some("from Ostrów"))))

    repository.updateIfPresent(title, year, before, after) shouldBe true

    val found = repository.findAll().find(r => r.record.imdbId.contains("tt0000004"))
    found should not be empty
    found.get.record.cinemaData.get(HeliosOstrowWlkp).flatMap(_.synopsis) shouldBe Some("from Ostrów")
  }

  // Regression: the dotted-name fallback above does a WHOLE-document replace. Writing the
  // in-memory cache row verbatim NULLS any Mongo-owned field the cache lacks — a rating not
  // yet rehydrated after a restart, or an out-of-band FilmwebUrlAudit edit — on EVERY scrape
  // tick for EVERY dotted-name cinema (common in Poland: "Helios Ostrów Wlkp."). The fallback
  // must apply the field-level diff to the PERSISTED doc, so an unrelated slot change leaves
  // the rating intact. Fails before the fix (the replace nulls imdbRating); passes after.
  it should "preserve a Mongo-owned rating when a dotted-name slot changes (no full-replace null)" in {
    val title  = "__integration-test-dotted-rating__"
    val year   = Some(1904)
    val stored = MovieRecord(
      imdbId = Some("tt0000006"), imdbRating = Some(7.5), metascore = Some(80),
      data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dotted"))))
    repository.upsert(title, year, stored) // Mongo holds the rating

    // The scrape tick's cache row lost the rating (evicted, not yet rehydrated) and adds a
    // dotted-name slot — the exact case that drives the full-replace fallback.
    val before = stored.copy(imdbRating = None, metascore = None)
    val after  = before.copy(data = before.data +
      (HeliosOstrowWlkp -> SourceData(title = Some("Dotted"), synopsis = Some("from Ostrów"))))
    repository.updateIfPresent(title, year, before, after) shouldBe true

    val found = repository.findAll().find(r => r.record.imdbId.contains("tt0000006"))
    found should not be empty
    val e = found.get.record
    e.imdbRating shouldBe Some(7.5) // survived the dotted-name full replace…
    e.metascore  shouldBe Some(80)
    e.cinemaData.get(HeliosOstrowWlkp).flatMap(_.synopsis) shouldBe Some("from Ostrów") // …and the slot change landed
  }

  // The split is on whenever a screenings repo is wired: `movies` is written WITHOUT
  // showtimes, reads stitch them from `screenings`, a showtimes-only change leaves
  // `movies` untouched, and a `screenings` change fans out a stitched upsert (so the
  // projector re-projects). Verified against a real replica set.
  it should "split reads: strip showtimes from movies, stitch from screenings, and fan out screenings changes" in {
    import services.movies.{MongoScreeningsRepository, StoredMovieRecord}
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val repo   = new MongoMovieRepository(Some(db), screenings = Some(scr), normalizer = titleNormalizer)
    val plain  = new MongoMovieRepository(Some(db), normalizer = titleNormalizer) // no stitch → sees the raw movies doc
    try {
      val title = "__integration-test-splitreads__"
      val year  = Some(1905)
      val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
      val key   = Multikino.displayName
      val slot  = SourceData(title = Some("SR"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 0), Some("https://book/sr-1"))))
      val base  = MovieRecord(imdbId = Some("tt0000014"), data = Map[Source, SourceData](Multikino -> slot))

      repo.upsert(title, year, base)
      scr.findForFilm(id).get(key).map(_.size) shouldBe Some(1)                                   // showtimes in screenings
      plain.findById(id).flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes.size) shouldBe Some(0) // …stripped from movies
      repo.findById(id).flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes.size) shouldBe Some(1)  // …stitched back on read

      // showtimes-only change → screenings grows, movies stays put, read reflects it
      val after = base.copy(data = Map[Source, SourceData](Multikino ->
        slot.copy(showtimes = slot.showtimes :+ Showtime(java.time.LocalDateTime.of(2026, 6, 1, 21, 0), Some("https://book/sr-2")))))
      repo.updateIfPresent(title, year, base, after) shouldBe true
      scr.findForFilm(id).get(key).map(_.size) shouldBe Some(2)
      repo.findById(id).flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes.size) shouldBe Some(2)

      // foreachRecord (the read-model RECONCILE + showtime metrics) must ALSO stitch —
      // else the reconcile projects empty showtimes and prunes web_screenings for every
      // film not re-scraped since boot (the 2026-07-02 served-films drop).
      var seen = 0
      repo.foreachRecord(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id)
        seen = r.record.cinemaData.get(Multikino).map(_.showtimes.size).getOrElse(0))
      seen shouldBe 2

      // a screenings change fans out a (stitched) upsert on the movies change stream
      val got    = new CountDownLatch(1)
      val handle = repo.watchChanges(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) got.countDown(), _ => ())
      try {
        Thread.sleep(1500)
        val after2 = after.copy(data = Map[Source, SourceData](Multikino ->
          after.data(Multikino).copy(showtimes = after.data(Multikino).showtimes :+
            Showtime(java.time.LocalDateTime.of(2026, 6, 1, 22, 0), Some("https://book/sr-3")))))
        repo.updateIfPresent(title, year, after, after2) // showtimes-only → screenings write → fanout
        got.await(15, TimeUnit.SECONDS) shouldBe true
      } finally handle.foreach(_.close())

      repo.delete(title, year)
      scr.findForFilm(id) shouldBe empty
    } finally { plain.close(); client.close() }
  }

  // Dual write into `movie_slots`: wiring a SlotsRepository mirrors each film's slots
  // into their own rows WITHOUT changing what `movies` stores or what reads return.
  // That reversibility is the whole point of this phase — the read flip comes later,
  // and only after a backfill. Verified against a real replica set.
  it should "dual-write cinema slots into movie_slots while movies stays the read authority" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val repo   = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    try {
      val title = "__integration-test-slotsplit__"
      val year  = Some(1907)
      val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
      val key   = Multikino.displayName
      val slot  = SourceData(title = Some("SS"), posterUrl = Some("https://poster/ss.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 0), Some("https://book/ss-1"))))
      val base  = MovieRecord(imdbId = Some("tt0000015"), data = Map[Source, SourceData](Multikino -> slot))

      repo.upsert(title, year, base)
      // mirrored into movie_slots, with showtimes left to `screenings`
      slots.findForFilm(id).get(key).flatMap(_.posterUrl) shouldBe Some("https://poster/ss.png")
      slots.findForFilm(id)(key).showtimes                shouldBe empty
      // …and `movies` is still the read authority: the record reads back unchanged
      repo.findById(id).flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe
        Some("https://poster/ss.png")

      // a metadata change mirrors through updateIfPresent
      val after = base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(posterUrl = Some("https://poster/ss2.png"))))
      repo.updateIfPresent(title, year, base, after) shouldBe true
      slots.findForFilm(id).get(key).flatMap(_.posterUrl) shouldBe Some("https://poster/ss2.png")

      // a showtimes-only change must NOT rewrite the slot — that is screenings' job,
      // and the two side collections have to stay independent
      val after2 = after.copy(data = Map[Source, SourceData](Multikino ->
        after.data(Multikino).copy(showtimes = slot.showtimes :+
          Showtime(java.time.LocalDateTime.of(2026, 6, 1, 21, 0), Some("https://book/ss-2")))))
      repo.updateIfPresent(title, year, after, after2) shouldBe true
      scr.findForFilm(id).get(key).map(_.size)           shouldBe Some(2)
      slots.findForFilm(id).get(key).flatMap(_.posterUrl) shouldBe Some("https://poster/ss2.png")
      slots.findForFilm(id)(key).showtimes                shouldBe empty

      // a slot that leaves the film is pruned by the whole-record write
      repo.upsert(title, year, base.copy(data = Map.empty[Source, SourceData]))
      slots.findForFilm(id) shouldBe empty

      // deleting the film clears its slots
      repo.upsert(title, year, base)
      slots.findForFilm(id)  should not be empty
      repo.delete(title, year)
      slots.findForFilm(id) shouldBe empty
    } finally { slots.deleteFilm(StoredMovieRecord.idFor("__integration-test-slotsplit__", Some(1907), titleNormalizer)); client.close() }
  }

  // Read flip: `movie_slots` wins when the film has rows there, and the embedded
  // `movies.sourceData` still serves films the lazy migration has not reached. Both
  // halves matter — the first is the point of the split, the second is what stops it
  // blanking every film that has not been rewritten yet.
  it should "read slots from movie_slots when present and fall back to the embedded map when not" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val legacy = new MongoMovieRepository(Some(db), screenings = Some(scr), normalizer = titleNormalizer) // writes the embedded map only
    val title  = "__integration-test-slotread__"
    val year   = Some(1908)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      // Carries a showtime so `screenings` is non-empty: scanStitched treats an empty
      // screenings load as a FAILED scan (prune-safety), which would otherwise stop
      // step (3) below from ever seeing a batch.
      val slot = SourceData(title = Some("SR"), posterUrl = Some("https://poster/embedded.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 2, 19, 0), Some("https://book/sr-1"))))
      val base = MovieRecord(imdbId = Some("tt0000016"), data = Map[Source, SourceData](Multikino -> slot))

      // (1) written by a repository with NO slots repo — nothing in movie_slots
      legacy.upsert(title, year, base)
      slots.findForFilm(id) shouldBe empty
      // …the split-aware reader still sees the film, from the embedded map
      split.findById(id).flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe
        Some("https://poster/embedded.png")

      // (2) now write through the split repo — movie_slots gains the row and wins the read
      val moved = base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(posterUrl = Some("https://poster/split.png"))))
      split.upsert(title, year, moved)
      slots.findForFilm(id) should not be empty
      split.findById(id).flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe
        Some("https://poster/split.png")

      // (3) the corpus scan agrees with the per-film read — the divergence that once
      // dropped 129 films is exactly what a second stitch site can reintroduce
      var scanned: Option[String] = None
      split.foreachRecord(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id)
        scanned = r.record.cinemaData.get(Multikino).flatMap(_.posterUrl))
      scanned shouldBe Some("https://poster/split.png")
    } finally { split.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // The DISPLAY TITLE has to survive the slot split too. `StoredMovieDto.toDomain`
  // derives it from the document's own `sourceData` — which the split leaves EMPTY —
  // so every read fell back to the sanitized `_id` prefix ("Integrationtestslottitledrno")
  // and the stitch that repairs the record never re-derived the name. The cache then
  // hydrated the whole corpus under mangled keys and the settle 16 minutes later
  // "re-spelled" each one back, rewriting 1240 of 1603 UK rows per boot under
  // byte-identical `_id`s (prod, 2026-07-28 06:29Z). The title must come from the
  // STITCHED record, not the hollow document.
  it should "derive the display title from the stitched slots, not the sanitized _id prefix" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    // Spaces and a dot, so `sanitize` (which strips both) cannot round-trip the title —
    // exactly the shape 78% of the corpus has, and the reason single-word films
    // ("Interstellar") were the only ones the settle left alone.
    val title  = "Integration Test Slottitle Dr. No"
    val year   = Some(1909)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val slot = SourceData(title = Some(title), posterUrl = Some("https://poster/st.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 3, 19, 0), Some("https://book/st-1"))))
      split.upsert(title, year, MovieRecord(imdbId = Some("tt0000024"),
        data = Map[Source, SourceData](Multikino -> slot)))
      // the slots really did move out of `movies`, so this is the split's read path
      slots.findForFilm(id) should not be empty

      split.findById(id).map(_.title)                              shouldBe Some(title)
      split.findAll().find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id).map(_.title) shouldBe Some(title)
      var scanned: Option[String] = None
      split.foreachRecord(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) scanned = Some(r.title))
      scanned shouldBe Some(title)
    } finally { split.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // The payoff: once the slots have landed, `movies` stops carrying sourceData at all,
  // so the document the change stream re-decodes on every write is a fraction of its
  // former size. Guarded by the rule that the embedded copy is only dropped after the
  // slot write is CONFIRMED — the one way this migration could lose a film.
  it should "drop the embedded sourceData once the slots have landed, and keep it when they have not" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val title  = "__integration-test-slotretire__"
    val year   = Some(1909)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    // Sees the RAW movies doc — no stitching — so it can prove what is actually stored.
    val raw    = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
    try {
      val slot = SourceData(title = Some("SR"), posterUrl = Some("https://poster/retire.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 3, 19, 0), Some("https://book/rt-1"))))
      val base = MovieRecord(imdbId = Some("tt0000017"), data = Map[Source, SourceData](Multikino -> slot))

      // slots land → movies carries NO sourceData, and the film still reads complete
      val split = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
      split.upsert(title, year, base)
      raw.findById(id).map(_.record.data.size)                                         shouldBe Some(0)
      slots.findForFilm(id)                                                            should not be empty
      split.findById(id).flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe
        Some("https://poster/retire.png")

      // a slots store that FAILS to write must leave the embedded copy in place, or the
      // film would have no cinemas in either collection
      val degraded = new MongoMovieRepository(Some(db), screenings = Some(scr),
        slots = Some(new services.movies.UnwritableSlotsRepository), normalizer = titleNormalizer)
      degraded.upsert(title, year, base)
      raw.findById(id).map(_.record.data.size) shouldBe Some(1)   // embedded copy retained
    } finally { raw.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // A patch must not put the slots back. `upsert` drops the embedded map once the slots
  // land; if `updateIfPresent` still wrote `sourceData.<slot>`, the very next metadata
  // change would rebuild it field by field and the document would creep back to full size.
  it should "not resurrect the embedded sourceData through a later patch" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val raw    = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)   // sees the stored doc, unstitched
    val title  = "__integration-test-slotpatch__"
    val year   = Some(1910)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val slot = SourceData(title = Some("SP"), posterUrl = Some("https://poster/p1.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 4, 19, 0), Some("https://book/sp-1"))))
      val base = MovieRecord(imdbId = Some("tt0000018"), data = Map[Source, SourceData](Multikino -> slot))

      split.upsert(title, year, base)
      raw.findById(id).map(_.record.data.size) shouldBe Some(0)   // stripped by the upsert

      // a metadata-only patch: slots move, `movies` stays empty
      val after = base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(posterUrl = Some("https://poster/p2.png"))))
      split.updateIfPresent(title, year, base, after) shouldBe true
      raw.findById(id).map(_.record.data.size) shouldBe Some(0)   // still no embedded copy
      slots.findForFilm(id).get(Multikino.displayName).flatMap(_.posterUrl) shouldBe Some("https://poster/p2.png")
      // …and the film still reads complete through the split-aware repository
      split.findById(id).flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe
        Some("https://poster/p2.png")
    } finally { raw.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // The projector only learns a film changed from the `movies` change stream. Once slots
  // moved out, a metadata-only change writes nothing but `movie_slots` — and `movie_slots`
  // has no watcher on purpose, since a second stream would fan out twice for one logical
  // change. So the write has to touch `movies` anyway, or the read model silently stops
  // updating for every title/poster/synopsis edit. It did; this is the guard.
  it should "still fan out a change event when only the slots changed" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val title  = "__integration-test-slotfanout__"
    val year   = Some(1911)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val slot = SourceData(title = Some("SF"), posterUrl = Some("https://poster/f1.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 5, 19, 0), Some("https://book/sf-1"))))
      val base = MovieRecord(imdbId = Some("tt0000019"), data = Map[Source, SourceData](Multikino -> slot))
      split.upsert(title, year, base)

      val fanouts = new java.util.concurrent.atomic.AtomicInteger(0)
      val got     = new CountDownLatch(1)
      val handle  = split.watchChanges(r =>
        if (StoredMovieRecord.idOf(r, titleNormalizer) == id) { fanouts.incrementAndGet(); got.countDown() }, _ => ())
      try {
        Thread.sleep(1500)
        // metadata only: no showtime change, and `movies` no longer stores the slot
        val after = base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(posterUrl = Some("https://poster/f2.png"))))
        split.updateIfPresent(title, year, base, after) shouldBe true
        got.await(15, TimeUnit.SECONDS) shouldBe true
        // …and EXACTLY once. One logical change must not re-project twice: that is why
        // `movie_slots` has no watcher of its own and the write touches `movies` instead.
        Thread.sleep(3000)   // leave room for a second event to arrive if one were coming
        fanouts.get() shouldBe 1
      } finally handle.foreach(_.close())
    } finally { split.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // findAllForListing reads `movies.sourceData` straight out of an aggregation, so it is
  // the one reader that does not go through the shared stitch — and a migrated film would
  // list with no cinemas at all.
  it should "stitch slots into the listing read as well" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val title  = "__integration-test-slotlisting__"
    val year   = Some(1912)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val slot = SourceData(title = Some("SL"), posterUrl = Some("https://poster/l1.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 6, 19, 0), Some("https://book/sl-1"))))
      split.upsert(title, year, MovieRecord(imdbId = Some("tt0000020"), data = Map[Source, SourceData](Multikino -> slot)))

      val listed = split.findAllForListing().find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id)
      listed.map(_.record.cinemaData.keySet)                         shouldBe Some(Set(Multikino))
      listed.flatMap(_.record.cinemaData.get(Multikino)).flatMap(_.posterUrl) shouldBe Some("https://poster/l1.png")
      // …still without showtimes, which is the whole point of this read
      listed.flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes) shouldBe Some(Seq.empty)
    } finally { split.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // Showtimes EXPIRING is what most re-scrapes look like: same film, same metadata, one
  // fewer session because a screening has passed. That must stay a screenings-only write.
  // If the slots split made it also write `movie_slots` — or touch `movies` — every scrape
  // of every film would re-project for nothing, on a clock rather than on a real change.
  it should "keep an expiring-showtimes re-scrape a screenings-only write, with one fanout and no slot write" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    import java.util.concurrent.atomic.AtomicInteger
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val raw    = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
    val title  = "__integration-test-slotexpiry__"
    val year   = Some(1913)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val early = Showtime(java.time.LocalDateTime.of(2026, 6, 7, 14, 0), Some("https://book/e-1"))
      val late  = Showtime(java.time.LocalDateTime.of(2026, 6, 7, 20, 0), Some("https://book/e-2"))
      val slot  = SourceData(title = Some("EX"), posterUrl = Some("https://poster/e.png"),
        showtimes = Seq(early, late))
      val base  = MovieRecord(imdbId = Some("tt0000021"), data = Map[Source, SourceData](Multikino -> slot))
      split.upsert(title, year, base)

      val slotsBefore   = slots.findForFilm(id)
      val slotUpdatedAt = Option(Await.result(
        db.withCodecRegistry(services.movies.MovieCodecs.registry)
          .getCollection[services.movies.StoredSlotDto]("movie_slots")
          .find(org.mongodb.scala.model.Filters.eq("filmId", id)).first().toFuture(), 10.seconds)).map(_.updatedAt)

      val fanouts = new AtomicInteger(0)
      val got     = new CountDownLatch(1)
      val handle  = split.watchChanges(r =>
        if (StoredMovieRecord.idOf(r, titleNormalizer) == id) { fanouts.incrementAndGet(); got.countDown() }, _ => ())
      try {
        Thread.sleep(1500)
        // the 14:00 screening has passed — the scrape returns only the 20:00 one
        val after = base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(showtimes = Seq(late))))
        split.updateIfPresent(title, year, base, after) shouldBe true

        got.await(15, TimeUnit.SECONDS) shouldBe true   // the read model DOES need the change
        Thread.sleep(3000)
        fanouts.get() shouldBe 1                        // …but exactly once, not twice

        scr.findForFilm(id).get(Multikino.displayName).map(_.size) shouldBe Some(1)
        slots.findForFilm(id) shouldBe slotsBefore      // slot CONTENT untouched
        // and not even rewritten — an expiring showtime must not churn movie_slots rows
        val slotUpdatedAfter = Option(Await.result(
          db.withCodecRegistry(services.movies.MovieCodecs.registry)
            .getCollection[services.movies.StoredSlotDto]("movie_slots")
            .find(org.mongodb.scala.model.Filters.eq("filmId", id)).first().toFuture(), 10.seconds)).map(_.updatedAt)
        slotUpdatedAfter shouldBe slotUpdatedAt
      } finally handle.foreach(_.close())
    } finally { raw.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // `upsert` is the whole-record path every scrape merge takes, and replaceFilm rewrites
  // EVERY row of the film. A re-scrape whose slots are unchanged must not churn them —
  // for a film across 471 UK venues that is 471 pointless row writes per scrape, and
  // Mongo write throughput has been the binding constraint on this system before.
  it should "not rewrite unchanged slot rows on a repeat upsert" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredSlotDto, MovieCodecs, StoredMovieRecord}
    import org.mongodb.scala.ObservableFuture
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val raw    = new MongoMovieRepository(Some(db), normalizer = titleNormalizer)
    val title  = "__integration-test-slotchurn__"
    val year   = Some(1914)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    def rowStamps() = Await.result(
      db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredSlotDto]("movie_slots")
        .find(org.mongodb.scala.model.Filters.eq("filmId", id)).toFuture(), 10.seconds)
      .map(d => d.slotKey -> d.updatedAt).toMap
    try {
      val early = Showtime(java.time.LocalDateTime.of(2026, 6, 8, 14, 0), Some("https://book/c-1"))
      val late  = Showtime(java.time.LocalDateTime.of(2026, 6, 8, 20, 0), Some("https://book/c-2"))
      val slot  = SourceData(title = Some("CH"), posterUrl = Some("https://poster/c.png"), showtimes = Seq(early, late))
      val base  = MovieRecord(imdbId = Some("tt0000022"), data = Map[Source, SourceData](Multikino -> slot))
      split.upsert(title, year, base)
      val before = rowStamps()
      before should not be empty

      // a re-scrape that changed only showtimes — slots identical
      Thread.sleep(50)
      split.upsert(title, year, base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(showtimes = Seq(late)))))
      rowStamps() shouldBe before          // untouched

      // …but a real slot change still writes
      Thread.sleep(50)
      split.upsert(title, year, base.copy(data = Map[Source, SourceData](Multikino -> slot.copy(posterUrl = Some("https://poster/c2.png")))))
      rowStamps() should not be before
      slots.findForFilm(id).get(Multikino.displayName).flatMap(_.posterUrl) shouldBe Some("https://poster/c2.png")
    } finally { raw.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // Slots read-path parity — the generalisation of the bug that shipped. `findAllForListing`
  // reads `movies.sourceData` straight out of an aggregation and so bypassed the shared
  // stitch, listing a migrated film with NO cinemas. Every reader is pinned here rather
  // than just that one, because the failure mode is "a reader forgot to stitch" and the
  // next one added will forget too. The showtimes equivalent below exists because exactly
  // this class of divergence once dropped 129 films.
  it should "return identical slots from findById, findAll, foreachRecord and findAllForListing" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    import models.SourceData
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val split  = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
    val title  = "__integration-test-slotparity__"
    val year   = Some(1915)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val slot = SourceData(title = Some("PA"), posterUrl = Some("https://poster/pa.png"),
        showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 9, 19, 0), Some("https://book/pa-1"))))
      split.upsert(title, year, MovieRecord(imdbId = Some("tt0000023"), data = Map[Source, SourceData](Multikino -> slot)))

      def slotsOf(r: Option[StoredMovieRecord]) =
        r.map(_.record.cinemaData.map { case (src, sd) => src -> sd.posterUrl })

      val viaFindById   = slotsOf(split.findById(id))
      val viaFindAll    = slotsOf(split.findAll().find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id))
      var scanned: Option[StoredMovieRecord] = None
      split.foreachRecord(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) scanned = Some(r))
      val viaForeach    = slotsOf(scanned)
      val viaListing    = slotsOf(split.findAllForListing().find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id))

      val expected = Some(Map[Source, Option[String]](Multikino -> Some("https://poster/pa.png")))
      withClue("findById: ")          { viaFindById shouldBe expected }
      withClue("findAll: ")           { viaFindAll  shouldBe expected }
      withClue("foreachRecord: ")     { viaForeach  shouldBe expected }
      withClue("findAllForListing: ") { viaListing  shouldBe expected }
    } finally { split.delete(title, year); slots.deleteFilm(id); client.close() }
  }

  // (C) Read-path parity: every corpus reader must agree on a film's showtimes under
  // the split. findAll and foreachRecord diverging (one forgot to stitch) is what
  // dropped 129 films; this guards against ANY future divergence between the readers.
  it should "return identical showtimes from findAll, findById and foreachRecord (read-path parity)" in {
    import services.movies.{MongoScreeningsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val repo   = new MongoMovieRepository(Some(db), screenings = Some(scr), normalizer = titleNormalizer)
    try {
      val title = "__integration-test-readpath-parity__"
      val year  = Some(1906)
      val id    = StoredMovieRecord.idFor(title, year, titleNormalizer)
      val slot  = SourceData(title = Some("Parity"), showtimes = Seq(
        Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 0), Some("https://book/p-1")),
        Showtime(java.time.LocalDateTime.of(2026, 6, 1, 21, 0), Some("https://book/p-2"))))
      repo.upsert(title, year, MovieRecord(imdbId = Some("tt0000015"), data = Map[Source, SourceData](Multikino -> slot)))

      def showtimesVia(r: Option[StoredMovieRecord]) = r.flatMap(_.record.cinemaData.get(Multikino)).map(_.showtimes).getOrElse(Seq.empty)
      val viaFindById = showtimesVia(repo.findById(id))
      val viaFindAll  = showtimesVia(repo.findAll().find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id))
      var viaForeach  = Seq.empty[Showtime]
      repo.foreachRecord(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) viaForeach = r.record.cinemaData.get(Multikino).map(_.showtimes).getOrElse(Seq.empty))

      viaFindById.size shouldBe 2
      viaFindAll  shouldBe viaFindById
      viaForeach  shouldBe viaFindById // all three agree — no reader silently strips

      // The count-only scan deliberately does NOT stitch (empty showtimes) — it skips
      // the screenings load. Contract guard so a future "fix" to stitch it (and re-add
      // the per-scan cost) is caught.
      var viaNoStitch = Seq.empty[Showtime]
      repo.foreachRecordWithoutShowtimes(r => if (StoredMovieRecord.idOf(r, titleNormalizer) == id) viaNoStitch = r.record.cinemaData.get(Multikino).map(_.showtimes).getOrElse(Seq.empty))
      viaNoStitch shouldBe empty

      repo.delete(title, year)
    } finally client.close()
  }

  // `ScreeningsRepository.findAll` now keyset-pages by `_id` (via KeysetScan) instead of
  // pulling the whole `screenings` collection through ONE unbounded `find().toFuture()`.
  // That single cursor recursed the async Mongo driver's read-completion chain
  // (`AsyncSupplier.finish` → `AsyncCompletionHandler` → `SingleResultCallback`) into a
  // StackOverflowError once the collection grew (Sentry KINOWO-19) — and because
  // `MovieRepository.scanStitched` calls `screenings.findAll()` FIRST, that crash killed
  // the worker's cold-cache rehydrate (findAll reported empty, the pages served no films).
  // The StackOverflow only reproduces against the real driver under a large buffered read,
  // so this guards the fix MECHANISM: with batchSize 2 forcing several page boundaries,
  // findAll returns every seeded slot exactly once, grouped by film — the keyset-paging
  // correctness the refactor introduces.
  it should "page screenings.findAll by _id across batch boundaries, returning every slot exactly once" in {
    import services.movies.{MongoScreeningsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val writer = new MongoScreeningsRepository(Some(db))
    // batchSize 2 forces several page boundaries over the 5 seeded slots.
    val paged  = new MongoScreeningsRepository(Some(db), findAllBatchSize = 2)
    try {
      val filmA = StoredMovieRecord.idFor("__integration-test-scr-page-A__", Some(1908), titleNormalizer)
      val filmB = StoredMovieRecord.idFor("__integration-test-scr-page-B__", Some(1908), titleNormalizer)
      val st    = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 0), Some("https://book/p")))
      writer.replaceFilm(filmA, Map("aa" -> st, "bb" -> st, "cc" -> st))
      writer.replaceFilm(filmB, Map("dd" -> st, "ee" -> st))

      val all = paged.findAll()
      all.getOrElse(filmA, Map.empty).keySet shouldBe Set("aa", "bb", "cc") // every slot, no skip
      all.getOrElse(filmB, Map.empty).keySet shouldBe Set("dd", "ee")       // …across the boundary
      all.getOrElse(filmA, Map.empty).values.flatten.toSeq shouldBe Seq.fill(3)(st).flatten // no duplicate slot

      writer.deleteFilm(filmA); writer.deleteFilm(filmB)
    } finally client.close()
  }

  // (B) The exact regression, end to end: the read-model RECONCILE reads the corpus
  // via foreachRecord under the split; if that read doesn't stitch, reconcile projects
  // empty showtimes and DELETES the film's web_screenings. Wire the real projector to a
  // split repo + read model, reconcile, and assert the film's screening is RETAINED.
  it should "not prune a split film's web_screenings on reconcile (foreachRecord stitches)" in {
    import services.movies.{MongoScreeningsRepository, StoredMovieRecord}
    import services.readmodel.{MongoReadModelRepository, ReadModelProjector}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val repo   = new MongoMovieRepository(Some(db), screenings = Some(scr), normalizer = titleNormalizer)
    val rm     = new MongoReadModelRepository(Some(db))
    val title  = "__integration-test-reconcile-noprune__"
    val year   = Some(1907)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    var expectedScrIds = Set.empty[String]
    var expectedMovIds = Set.empty[String]
    try {
      // A split film: showtimes go to screenings, movies is stripped. KinoMuranow maps
      // to a city (warszawa) so projection yields a CityScreening; tmdbId makes it
      // readyToProject (the reconcile only projects ready rows).
      repo.upsert(title, year, MovieRecord(imdbId = Some("tt0000012"), tmdbId = Some(500123),
        data = Map[Source, SourceData](KinoMuranow -> SourceData(title = Some("Reconcile"),
          showtimes = Seq(Showtime(java.time.LocalDateTime.of(2026, 6, 1, 18, 0), Some("https://book/rc-1")))))))

      // What the film SHOULD project to (via the stitched findById read path) — the
      // screening ids the reconcile must WRITE and RETAIN. Non-empty proves the film
      // is projectable at all; the reconcile then must produce the same set.
      val projected  = services.readmodel.ReadModelProjection.projectAll(repo.findById(id).get, titleNormalizer)
      expectedScrIds = projected.flatMap(_._2).map(_._id).toSet
      expectedMovIds = projected.map(_._1._id).toSet
      expectedScrIds should not be empty

      new ReadModelProjector(repo, rm, rm).reconcile() // full re-project from foreachRecord (stitched)

      // The reconcile RETAINED the film's screenings (pre-fix, foreachRecord returned
      // empty showtimes → projectAll produced 0 → the screenings were pruned/never written).
      rm.findAllScreeningRefs().map(_._id).toSet should contain allElementsOf expectedScrIds
    } finally {
      // Tidy the web_* the projector wrote (keyed by the projection-derived ids).
      expectedScrIds.foreach(rm.deleteScreening)
      expectedMovIds.foreach(rm.deleteMovie)
      repo.delete(title, year)
      rm.close(); client.close()
    }
  }

  it should "leave countries empty when a slot was written without them" in {
    val title = "__integration-test-sourcedata-no-country__"
    val slot  = SourceData()
    repository.upsert(title, None, MovieRecord(
      imdbId = Some("tt0000005"),
      data   = Map[Source, SourceData](Multikino -> slot)
    ))
    val found = repository.findAll().find(r => r.record.imdbId.contains("tt0000005"))
    found should not be empty
    found.get.record.cinemaData(Multikino).countries shouldBe Seq.empty
    found.get.record.countries shouldBe Seq.empty
  }

  // Regression for "Tom i Jerry: Przygoda w muzeum" / "Tom i jerry: przygoda w
  // muzeum": case-only variants of the same Polish title accumulated as
  // separate Mongo rows because documentId was case-preserved. The hourly refresh
  // walks the Caffeine cache (which collapses them) and only ever wrote back
  // to one row, leaving the other(s) frozen at whatever they were when first
  // upserted — including with metacriticUrl/rottenTomatoesUrl set to None for
  // records created before that feature shipped.
  it should "collapse case-variant cleanTitle upserts into a single Mongo row" in {
    val titleCaps = "__integration-test-CASEDEDUPE__"
    val titleLow  = "__integration-test-casededupe__"
    val withUrls = MovieRecord(
      imdbId            = Some("tt0000010"),
      imdbRating        = Some(7.5),
      metascore         = Some(80),
      metacriticUrl     = Some("https://www.metacritic.com/movie/case-dedupe-test"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/case_dedupe_test"),
      data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Case Dedupe Test")))
    )
    val withoutUrls = withUrls.copy(
      metacriticUrl     = None,
      rottenTomatoesUrl = None
    )

    // Upsert UPPER first (with URLs), then LOWER (without). With normalized
    // documentId both writes target the same _id, so the second overwrites.
    repository.upsert(titleCaps, Some(2025), withUrls)
    repository.upsert(titleLow,  Some(2025), withoutUrls)

    val rows = repository.findAll().filter(_.record.imdbId.contains("tt0000010"))
    rows                                  should have size 1
    // Second upsert wins: URLs nulled, which is exactly what made the
    // production case observable.
    rows.head.record.metacriticUrl     shouldBe None
    rows.head.record.rottenTomatoesUrl shouldBe None
  }

  // Regression: legacy documents in prod were written with an older `documentId`
  // formula (whitespace-preserving), and `repository.delete` — which builds the
  // `_id` from the *current* formula — silently failed to delete them
  // (`deleteOne` matched zero documents, no warning). On every restart the
  // mergeAll pass picked the same losers and tried to delete them, but
  // their old-formula `_id`s never matched. Fix: delete by `title` + `year`
  // instead of by `_id`, so any past or future `_id` drift can't defeat the
  // cleanup.
  it should "delete every document matching (title, year), regardless of its _id formula" in {
    import org.mongodb.scala.bson._
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
      .getCollection[org.mongodb.scala.bson.collection.immutable.Document]("movies")
    try {
      // Seed two documents at the same (title, year) but with different `_id`s —
      // one matching the current formula, one with a stale "old-formula"
      // shape that the current `MovieRepository.documentId` wouldn't compute.
      val title = "__integration-test-stale-id__"
      val year  = Some(2099)
      val freshId = s"${title.toLowerCase.replaceAll("[^a-z0-9]+", "")}|2099"
      val staleId = s"${title.toLowerCase}|2099"  // stale formula keeps spaces/underscores
      Seq(freshId, staleId).foreach { id =>
        val document = org.mongodb.scala.bson.collection.immutable.Document(
          "_id"   -> BsonString(id),
          "title" -> BsonString(title),
          "year"  -> BsonInt32(2099)
        )
        Await.ready(coll.insertOne(document).toFuture(), 10.seconds)
      }
      // Sanity: both documents exist.
      val before = Await.result(coll.countDocuments(Filters.eq("title", title)).toFuture(), 10.seconds)
      before shouldBe 2

      repository.delete(title, year)

      val after = Await.result(coll.countDocuments(Filters.eq("title", title)).toFuture(), 10.seconds)
      after shouldBe 0
    } finally client.close()
  }

  // Regression: the delete filter `$or(_id, title+year)` used to COLLSCAN the
  // entire `movies` collection on every delete — the `(title, year)` branch was
  // unindexed, so Mongo scanned all ~1100 documents (~400ms, max ~6s under load) just
  // to evaluate it, the single largest source of `movies` read-lock time on the
  // self-hosted box. `MongoMovieRepository` now creates a `(title, year)` index at
  // init, so the branch is a 1-key IXSCAN and the whole `$or` is an index union.
  // Fails before the index (winning plan is a COLLSCAN); passes after.
  it should "resolve the delete filter by index, not a collection scan" in {
    repository.enabled shouldBe true // force lazy init → ensureIndexes() runs

    val client = MongoClient(Env.get("MONGODB_URI").get)
    val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
      .getCollection[org.mongodb.scala.bson.collection.immutable.Document]("movies")
    try {
      val title  = "__integration-test-delete-plan__"
      val year   = Some(2099)
      val filter = Filters.or(
        Filters.eq("_id", StoredMovieRecord.idFor(title, year, titleNormalizer)),
        Filters.and(Filters.eq("title", title), Filters.eq("year", 2099)))
      val plan = Await.result(coll.find(filter).explain().toFuture(), 10.seconds).toJson()
      plan        should include ("title_1_year_1")
      plan should not include ("COLLSCAN")
    } finally client.close()
  }

  // Regression: `afterAll` deleted `_id` matching `^__integration-test-`, but
  // `documentId` sanitizes the id (strips non-alphanumerics) so the stored `_id` is
  // `integrationtest…` — the regex never matched and EVERY run leaked its
  // sentinels into the prod corpus forever (8 fixtures were found sitting on
  // /debug). The cleanup must target the sanitized id (and the stable imdbId).
  // Fails before the fix (the sentinel survives `purgeSentinels`); passes after.
  it should "purge its sentinels by the sanitized _id they are actually stored under" in {
    repository.upsert("__integration-test-purge-check__", Some(1903), MovieRecord(imdbId = Some("tt0000077")))
    repository.findAll().exists(_.record.imdbId.contains("tt0000077")) shouldBe true
    purgeSentinels()
    repository.findAll().exists(_.record.imdbId.contains("tt0000077")) shouldBe false
  }

  // Regression: `findAll` ran an UNSORTED scan (`c.find()`). Over a collection
  // the worker writes concurrently (resolving TMDB, clearing `detailPending`,
  // re-keying years), an unsorted scan can return the same document more than
  // once — and skip others — when an intervening write relocates it mid-scan.
  // On /debug that surfaced as phantom duplicate rows (the same `_id` rendered
  // twice, one a stale pre-write image) that never cleared, plus silently
  // dropped rows. The fix sorts by the immutable, unique `_id` index, whose
  // key-ordered walk returns each document exactly once. The duplication itself only
  // reproduces under live concurrent write load (not deterministically here), so
  // this guards the fix MECHANISM: with the sort, three out-of-order sentinels
  // come back `_id`-ascending; without it the scan yields them in insertion
  // (natural) order.
  //
  // Assert ONLY on these sentinels, not the whole corpus: `findAll` sorts by the
  // STORED `_id`, but `idOf` RE-DERIVES the id from the (possibly programme-
  // decorated) display title — for a real row keyed `follement…|2025` under the
  // "Cykl…" programme, `idOf` yields `cyklzawsze…follement…`, so the two diverge
  // and a corpus-wide order assertion is meaningless. These sentinels carry no
  // sourceData, so their display title IS the id prefix and `idOf` == stored `_id`.
  it should "return rows in _id order (the _id-indexed scan that can't duplicate or skip)" in {
    Seq("c", "a", "b").foreach(s =>
      repository.upsert(s"__integration-test-order-${s}__", None, MovieRecord()))
    val ids = repository.findAll().map(StoredMovieRecord.idOf(_, titleNormalizer)).filter(_.startsWith("integrationtestorder"))
    ids        should have size 3 // all three returned — no skip
    ids shouldBe ids.sorted
  }

  // `foreachRecord` pages the cursor by `_id` (keyset pagination) so the read-model
  // reconcile never holds the whole corpus on the heap — the transient that OOM'd the
  // worker's 320m heap. The page boundary is the risk: `_id > lastSeen` must continue
  // without skipping or re-visiting the boundary row. Batch size 2 forces several
  // boundaries over the seeded rows regardless of how many the collection holds (CI
  // seeds only a handful, prod has ~1000), and the keyset invariant is global: every
  // row is visited exactly once (no duplicate even at a boundary), and the seeded
  // sentinels (no skip) all appear, in `_id` order across the boundaries.
  it should "stream every row exactly once across keyset page boundaries (foreachRecord)" in {
    Seq("a", "b", "c", "d", "e").foreach(s =>
      repository.upsert(s"__integration-test-stream-${s}__", None, MovieRecord()))

    val paged    = new MongoMovieRepository(findAllBatchSize = 2, normalizer = titleNormalizer)
    val streamed = scala.collection.mutable.ListBuffer.empty[String]
    try paged.foreachRecord(r => streamed += StoredMovieRecord.idOf(r, titleNormalizer)) finally paged.close()

    streamed.size shouldBe streamed.distinct.size        // no row re-visited at a boundary
    val sentinels = streamed.filter(_.startsWith("integrationteststream"))
    sentinels          should have size 5                // every seeded row visited — no skip/dup
    sentinels.distinct should have size 5
    sentinels          shouldBe sentinels.sorted          // …in _id order across the boundaries
  }

  // `findAll` now pages the cursor by `_id` (the same keyset scan `foreachRecord` uses)
  // instead of pulling the whole corpus through ONE unbounded `find().toFuture()` — that
  // single cursor recursed the async Mongo driver's per-message read-completion chain
  // (`AsyncSupplier.finish` → `AsyncCompletionHandler` → `SingleResultCallback`) deep
  // enough to StackOverflow on a driver I/O thread once the corpus grew large (Sentry
  // KINOWO-19), which crash-looped the worker's cold-cache rehydrate on boot.
  //
  // The StackOverflow itself only reproduces against the real driver under a LARGE
  // buffered corpus — a stack-depth/timing-dependent driver-internal symptom no test
  // layer can force deterministically (matching the `_id`-order test above, whose
  // duplication "only reproduces under live concurrent write load"). So this guards the
  // fix MECHANISM: with batchSize 2 forcing several page boundaries, findAll returns
  // every seeded row exactly once, in `_id` order — the keyset-paging correctness the
  // refactor introduces to findAll.
  it should "page findAll by _id across batch boundaries, returning every row exactly once" in {
    Seq("a", "b", "c", "d", "e").foreach(s =>
      repository.upsert(s"__integration-test-findall-page-${s}__", None, MovieRecord()))

    val paged = new MongoMovieRepository(findAllBatchSize = 2, normalizer = titleNormalizer)
    val ids   = try paged.findAll().map(StoredMovieRecord.idOf(_, titleNormalizer)) finally paged.close()

    ids.size shouldBe ids.distinct.size          // no row re-visited at a page boundary
    val sentinels = ids.filter(_.startsWith("integrationtestfindallpage"))
    sentinels          should have size 5        // every seeded row returned — no skip/dup
    sentinels.distinct should have size 5
    sentinels          shouldBe sentinels.sorted // …in _id order across the boundaries
  }

  private def show(hour: Int) =
    Showtime(java.time.LocalDateTime.of(2026, 6, 1, hour, 0), Some(s"https://book/rf-$hour"))

  private def screeningsDb(client: MongoClient) =
    client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  // `replaceFilm` used to cost a blocking `replaceOne` per slot, then a `findForFilm`
  // read, then a blocking `deleteOne` per stale slot — 12 sequential round-trips for a
  // film showing in 10 cinemas, paid on EVERY `MovieRepository.upsert` (i.e. constantly,
  // right through a scrape pass). It is now ONE ordered `bulkWrite`: all the slot upserts
  // plus a single `deleteMany` of the slots the write no longer names, which the driver
  // puts on the wire as one `update` command and one `delete` command, with no `find` at
  // all. Counted through a real driver command listener, so it fails before the change
  // (find=1, update=3, delete=2) and passes after (find=0, update=1, delete=1).
  it should "replace a film's slots in ONE bulk round-trip, with no read" in {
    import com.mongodb.event.{CommandListener, CommandStartedEvent}
    import com.mongodb.{ConnectionString, MongoClientSettings}
    import services.movies.MongoScreeningsRepository
    import java.util.concurrent.ConcurrentHashMap
    import java.util.concurrent.atomic.AtomicInteger

    val commands = new ConcurrentHashMap[String, AtomicInteger]()
    val listener = new CommandListener {
      override def commandStarted(event: CommandStartedEvent): Unit = {
        commands.computeIfAbsent(event.getCommandName, _ => new AtomicInteger(0)).incrementAndGet(); ()
      }
    }
    def count(name: String): Int = Option(commands.get(name)).map(_.get()).getOrElse(0)

    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(Env.get("MONGODB_URI").get))
      .addCommandListener(listener).build())
    val screenings = new MongoScreeningsRepository(Some(screeningsDb(client)))
    val film       = "__it-screenings-bulk-roundtrips__"
    try {
      // Seed four slots (this also forces the lazy collection + its createIndex, so those
      // commands land before the counter is reset).
      screenings.replaceFilm(film, Map(
        "A" -> Seq(show(10)), "B" -> Seq(show(11)), "C" -> Seq(show(12)), "STALE" -> Seq(show(13))))
      screenings.findForFilm(film).keySet shouldBe Set("A", "B", "C", "STALE")

      commands.clear()
      // One changed slot, one unchanged, one brand new — and two slots going stale.
      screenings.replaceFilm(film, Map("A" -> Seq(show(20)), "B" -> Seq(show(11)), "NEW" -> Seq(show(14))))

      count("find")   shouldBe 0 // the findForFilm read is gone entirely
      count("update") shouldBe 1 // all three upserts ride one bulk update
      count("delete") shouldBe 1 // both stale slots go in one deleteMany

      // …and the collapse did not change the result.
      screenings.findForFilm(film) shouldBe Map(
        "A" -> Seq(show(20)), "B" -> Seq(show(11)), "NEW" -> Seq(show(14)))
    } finally { screenings.deleteFilm(film); client.close() }
  }

  // Data-integrity guard for that same collapse. This write path has already destroyed
  // screenings once (a `replaceFilm` handed empty showtimes deleted a film's entire
  // listing), and the new single `deleteMany` — `filmId == X AND slotKey $nin [kept]` —
  // is now the ONLY thing deciding what a whole-record write destroys. Pins every case:
  // new slots, unchanged slots, changed slots, disappeared slots, a slot carrying EMPTY
  // showtimes, an empty `slots` map, a repeat of that empty map, a filmId containing the
  // composite-`_id` separator, and a neighbouring film that must never be touched.
  it should "preserve replaceFilm's exact set semantics without losing screenings" in {
    import services.movies.MongoScreeningsRepository
    val client     = MongoClient(Env.get("MONGODB_URI").get)
    val screenings = new MongoScreeningsRepository(Some(screeningsDb(client)))
    val film       = "__it-screenings-replace-semantics__"
    val neighbour  = "__it-screenings-replace-neighbour__"
    // A filmId that itself contains the `_id` separator, where `filmId + IdSep + slotKey`
    // can no longer be split back apart — the delete must key off the FIELDS, not the _id.
    val separatorFilm = s"__it-screenings${0x1f.toChar}sep__"
    try {
      screenings.replaceFilm(neighbour, Map("X" -> Seq(show(9))))

      // brand-new slots land
      screenings.replaceFilm(film, Map("A" -> Seq(show(10)), "B" -> Seq(show(11))))
      screenings.findForFilm(film) shouldBe Map("A" -> Seq(show(10)), "B" -> Seq(show(11)))

      // A unchanged, B changed, "E" mapped to EMPTY showtimes is STORED (never treated as
      // a delete — `showtimesOf` filters empties out upstream, `replaceFilm` does not).
      screenings.replaceFilm(film, Map("A" -> Seq(show(10)), "B" -> Seq(show(12)), "E" -> Seq.empty))
      screenings.findForFilm(film) shouldBe Map("A" -> Seq(show(10)), "B" -> Seq(show(12)), "E" -> Seq.empty)

      // repeating the identical write loses nothing (idempotent)
      screenings.replaceFilm(film, Map("A" -> Seq(show(10)), "B" -> Seq(show(12)), "E" -> Seq.empty))
      screenings.findForFilm(film) shouldBe Map("A" -> Seq(show(10)), "B" -> Seq(show(12)), "E" -> Seq.empty)

      // a separator-carrying filmId is replaced on its own terms
      screenings.replaceFilm(separatorFilm, Map("A" -> Seq(show(8)), "B" -> Seq(show(9))))
      screenings.replaceFilm(separatorFilm, Map("B" -> Seq(show(9))))
      screenings.findForFilm(separatorFilm) shouldBe Map("B" -> Seq(show(9)))

      // THE delete vector: an empty `slots` map clears THIS film entirely (`$nin: []`
      // matches every slot)…
      screenings.replaceFilm(film, Map.empty)
      screenings.findForFilm(film) shouldBe empty
      // …and nothing else — the filter never leaves the film.
      screenings.findForFilm(neighbour)     shouldBe Map("X" -> Seq(show(9)))
      screenings.findForFilm(separatorFilm) shouldBe Map("B" -> Seq(show(9)))

      // replacing an already-empty film is a harmless no-op, not an error
      screenings.replaceFilm(film, Map.empty)
      screenings.findForFilm(film) shouldBe empty
    } finally {
      Seq(film, neighbour, separatorFilm).foreach(screenings.deleteFilm)
      client.close()
    }
  }

  // A film mid-migration holds cinemas in BOTH places, and the two genuinely disagree —
  // `MongoStagingFolder` writes the embedded map and no slot rows, `updateIfPresent`
  // writes slot deltas and leaves the embedded map alone. Letting the stored rows shadow
  // the embedded map dropped the cinemas only the embedded map had: on prod PL
  // 2026-07-27, 14 films were being served with fewer cinemas than the corpus held.
  it should "serve a cinema the embedded map has and movie_slots does not" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val title  = "__integration-test-slot-union__"
    val year   = Some(1908)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      // Write the film with BOTH cinemas and NO slots repository wired — the un-migrated
      // shape, and exactly what the staging fold's in-transaction write leaves behind.
      val embeddedOnly = new MongoMovieRepository(Some(db), screenings = Some(scr), normalizer = titleNormalizer)
      embeddedOnly.upsert(title, year, MovieRecord(imdbId = Some("tt0000079"),
        data = Map[Source, SourceData](
          Multikino   -> SourceData(title = Some("from movies")),
          KinoMuranow -> SourceData(title = Some("embedded only")))))
      // …then let a per-slot delta land for ONE of them, as `updateIfPresent` does.
      slots.replaceFilm(id, Map(Multikino.displayName -> SourceData(title = Some("from movie_slots"))))

      val repo = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
      val read = repo.findById(id).map(_.record.cinemaData).getOrElse(Map.empty)
      // the stored row wins the key both carry…
      read.get(Multikino).flatMap(_.title)   shouldBe Some("from movie_slots")
      // …and the cinema only `movies` knows about SURVIVES. Fails before the union rule:
      // the stored rows shadowed the embedded map and this cinema simply vanished.
      read.get(KinoMuranow).flatMap(_.title) shouldBe Some("embedded only")
    } finally { slots.deleteFilm(id); scr.deleteFilm(id); client.close() }
  }

  // Once a film's embedded copy is retired, `movies` carries no cinemas at all — so a
  // FAILED slot read decodes to a film with none. That record is what the change-stream
  // fan-out hands the projector, whose `diffScreenings` then deletes every `web_screening`
  // the film has: a transient Mongo blip would empty a live film off the site.
  it should "refuse to decode a migrated film at all when its slot read fails" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord,
      SlotsRepository, UnreadableSlotsRepository}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val title  = "__integration-test-slot-readfail__"
    val year   = Some(1909)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    try {
      val repo = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
      repo.upsert(title, year, MovieRecord(imdbId = Some("tt0000080"),
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("live cinema")))))
      // the migrated shape: slots landed, so `movies` dropped its embedded copy
      slots.findForFilm(id)                                    should not be empty
      repo.findById(id).map(_.record.data.size)                shouldBe Some(1)

      // …now the same row, read through a slots repository whose reads fail.
      val blind: SlotsRepository = new UnreadableSlotsRepository
      val blindRepo = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(blind), normalizer = titleNormalizer)
      // None, NOT a record with an empty `data` map. Fails before the fix: `findById`
      // returned a film with zero cinemas, which the projector treats as "delete them all".
      blindRepo.findById(id) shouldBe None
    } finally { slots.deleteFilm(id); scr.deleteFilm(id); client.close() }
  }

  // END TO END, against real Mongo: the whole 2026-07-27 failure in one test.
  //
  // The unit specs pin each link — a failed read reports itself, a scrape defers on one —
  // but the damage came from the COMBINATION, and only real storage shows it: an
  // unreadable corpus leaves the cache cold, the scrape lands anyway, and
  // `screenings.replaceFilm` prunes with `$nin` against a record built from that one
  // cinema. This asserts the thing that actually matters to a user: the OTHER cinema's
  // showtimes are still in Mongo afterwards.
  it should "leave a film's other cinemas' showtimes alone when a scrape lands on an unreadable corpus" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord,
      UnreadableSlotsRepository, CaffeineMovieCache}
    import models.CinemaMovie
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val title  = "__integration-test-unreadable-scrape__"
    val year   = Some(1911)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val when   = java.time.LocalDateTime.now().plusDays(1).withHour(20).withMinute(0).withSecond(0).withNano(0)
    try {
      // A live film showing at TWO cinemas, written through the real repository.
      val healthy = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
      healthy.upsert(title, year, MovieRecord(imdbId = Some("tt0000081"), tmdbId = Some(4243),
        data = Map[Source, SourceData](
          Multikino   -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))),
          KinoMuranow -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))))))
      scr.findForFilm(id).keySet should have size 2

      // Now the corpus goes unreadable — the state the decode bug produced — so the cache
      // boot-hydrates EMPTY and the per-film read fails too.
      val blindRepo = new MongoMovieRepository(Some(db), screenings = Some(scr),
        slots = Some(new UnreadableSlotsRepository), normalizer = titleNormalizer)
      val cache = new CaffeineMovieCache(blindRepo, normalizer = titleNormalizer)

      // …and Multikino's scrape lands, as it would on any ordinary tick.
      cache.recordCinemaScrape(Multikino, Seq(CinemaMovie(
        movie = models.Movie(title, releaseYear = year), cinema = Multikino, posterUrl = None,
        filmUrl = None, synopsis = None, cast = Seq.empty, director = Seq.empty,
        showtimes = Seq(Showtime(when, None)))))

      // Kino Muranów never went anywhere. Before the fix this scrape rebuilt the film
      // from itself alone and `$nin` deleted Muranów's showtimes from `screenings`.
      withClue(s"screenings now: ${scr.findForFilm(id).keySet}: ")(
        scr.findForFilm(id).keySet should contain (KinoMuranow.displayName))
      cache.stop()
    } finally { slots.deleteFilm(id); scr.deleteFilm(id); client.close() }
  }

  // The same family one collection over, and the one member that never got a checked read.
  // `upsert` re-stitches a cache-STRIPPED record's showtimes back out of `screenings` before
  // writing, because `replaceFilm` deletes every slot the record doesn't name. When that read
  // FAILS it returns empty — indistinguishable from "this film has no screenings" — so every
  // slot looks showtime-less and the delete vector erases the lot, while `movie_slots` keeps
  // the film↔cinema rows and `movies` keeps the film. That is exactly the state prod DE was in
  // on 2026-07-27: 13,201 cinema slots, only 2,609 with a screenings doc, zero the other way,
  // film count flat and every city still present while ~80% of upcoming showtimes vanished.
  it should "keep a film's showtimes when the screenings read fails under a whole-record write" in {
    import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, ShowtimesDigest,
      StoredMovieRecord, UnreadableScreeningsRepository}
    val client = MongoClient(Env.get("MONGODB_URI").get)
    val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    val scr    = new MongoScreeningsRepository(Some(db))
    val slots  = new MongoSlotsRepository(Some(db))
    val title  = "__integration-test-unreadable-screenings__"
    val year   = Some(1912)
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val when   = java.time.LocalDateTime.now().plusDays(1).withHour(20).withMinute(0).withSecond(0).withNano(0)
    try {
      // A live film showing at two cinemas, written through the real repository.
      val healthy = new MongoMovieRepository(Some(db), screenings = Some(scr), slots = Some(slots), normalizer = titleNormalizer)
      healthy.upsert(title, year, MovieRecord(imdbId = Some("tt0000082"), tmdbId = Some(4244),
        data = Map[Source, SourceData](
          Multikino   -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))),
          KinoMuranow -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))))))
      scr.findForFilm(id).keySet should have size 2

      // The record as the CACHE holds it: showtimes stripped, digest kept — the shape every
      // ordinary enrichment/merge write carries. Now the screenings read fails.
      val stripped = ShowtimesDigest.stripForCache(
        MovieRecord(imdbId = Some("tt0000082"), tmdbId = Some(4244), data = Map[Source, SourceData](
          Multikino   -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))),
          KinoMuranow -> SourceData(title = Some("Unreadable"), showtimes = Seq(Showtime(when, None))))))
      val blind = new MongoMovieRepository(Some(db), slots = Some(slots),
        screenings = Some(new UnreadableScreeningsRepository(scr)), normalizer = titleNormalizer)
      blind.upsert(title, year, stripped)

      // Neither cinema stopped screening — the read just failed. Before the fix both were
      // deleted here and only the slot rows survived.
      withClue(s"screenings now: ${scr.findForFilm(id).keySet}: ")(
        scr.findForFilm(id).keySet should have size 2)
    } finally { slots.deleteFilm(id); scr.deleteFilm(id); client.close() }
  }

}
