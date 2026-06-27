package integration

import models.{Helios, HeliosOstrowWlkp, MovieRecord, Multikino, Showtime, Source, SourceData, Tmdb}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import services.movies.{MongoMovieRepository, StoredMovieRecord}
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

  private val repository = new MongoMovieRepository()

  // Every fake imdbId this spec writes. These are the STABLE handle: the worker
  // re-keys a row's `_id` (e.g. settles `__integration-test-dotted-cinema__` to
  // `dotted|1902` off its sourceData title), so an `_id`-only purge can miss a
  // re-keyed sentinel — but `imdbId` never changes.
  private val sentinelImdbIds = Seq(
    "tt0000001", "tt0000002", "tt0000003", "tt0000004",
    "tt0000005", "tt0000010", "tt0000077", "tt0000099"
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
    repository.upsert(title, year, MovieRecord(imdbId = Some("tt0000010"), data = Map[Source, SourceData](Multikino -> slot)))

    val listed = repository.findAllForListing().find(_.record.imdbId.contains("tt0000010"))
    listed should not be empty
    val lslot = listed.get.record.cinemaData(Multikino)
    lslot.showtimes shouldBe empty                 // stripped server-side…
    lslot.title shouldBe Some("Listing Sentinel")  // …but the rest of the slot survives

    // The full-fidelity reads still carry the showtimes (the /debug details path).
    val full = repository.findById(StoredMovieRecord.idOf(listed.get))
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
    val id    = StoredMovieRecord.idFor(title, year)
    val gotUpsert = new CountDownLatch(1)
    val gotDelete = new CountDownLatch(1)

    val handle = repository.watchChanges(
      onUpsert = r   => if (StoredMovieRecord.idOf(r) == id) gotUpsert.countDown(),
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
        Filters.eq("_id", StoredMovieRecord.idFor(title, year)),
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
    val ids = repository.findAll().map(StoredMovieRecord.idOf).filter(_.startsWith("integrationtestorder"))
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

    val paged    = new MongoMovieRepository(findAllBatchSize = 2)
    val streamed = scala.collection.mutable.ListBuffer.empty[String]
    try paged.foreachRecord(r => streamed += StoredMovieRecord.idOf(r)) finally paged.close()

    streamed.size shouldBe streamed.distinct.size        // no row re-visited at a boundary
    val sentinels = streamed.filter(_.startsWith("integrationteststream"))
    sentinels          should have size 5                // every seeded row visited — no skip/dup
    sentinels.distinct should have size 5
    sentinels          shouldBe sentinels.sorted          // …in _id order across the boundaries
  }
}
