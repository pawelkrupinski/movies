package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import services.staging.{MongoStagingRepository, StagingRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The sibling lookup behind staging's duplicate-entry warning ranges over `_id` and uses
 * NOTHING but the `_id`s — yet it fetched and decoded every whole document in that
 * range, showtimes array and all.
 *
 * It runs on every FRESH insert, so its cost grows with the staged backlog, and a
 * convergence leg stages a whole country before folding any of it. That took `bootCorpus`
 * from 30 seconds against in-memory repositories to 3,360 against Mongo — a 113x
 * regression that timed the leg out at CI's ceiling — with the movie codec's `showtimes`
 * decoder the top frame in every JVM sample while Mongo itself was under 1% of wall
 * clock. The work was never the query; it was deserialising payloads to read one string
 * off each.
 *
 * Guarded on BYTES RETURNED rather than documents examined. `docsExamined` looked like
 * the obvious discriminator and quietly is not: for this query shape the server can
 * report 0 either way, which produced several confident-looking green runs that proved
 * nothing. `responseLength` cannot be faked — projected or not, the wire either carries
 * the payloads or it doesn't.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class StagingSiblingProjectionIntegrationSpec extends AnyFlatSpec with Matchers with org.scalatest.BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  // Its OWN database, not the one every other `it` spec shares.
  //
  // Two reasons, both learned the hard way. This spec seeds deliberately HEAVY and
  // deliberately odd documents — that is the point of it — and a raw one missing
  // `updatedAt` made `StagingFoldIntegrationSpec` fail with `Missing field: updatedAt`,
  // the same "one bad row kills a whole batch decode" shape this repository has shipped
  // to production twice. And it measures Mongo's PROFILER, which is database-wide: with
  // specs running concurrently the byte counts belonged to whichever suite happened to be
  // querying, so the assertions were reading someone else's traffic.
  private val db     = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "staging-projection-spec")
  private val staged = db.getCollection[Document]("pending_movies")

  override protected def afterAll(): Unit = {
    tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  private val title  = "Ghost In The Shell"
  /** Derived from `idFor`, so the seeded siblings share the prefix `upsert` will compute
   *  — a hand-written prefix silently ranges over nothing. */
  private val prefix = StagingRecord.idFor(Multikino, title, None, titleNormalizer).stripSuffix("")

  private def purge(): Unit =
    Await.result(staged.deleteMany(Filters.regex("_id", "^" + java.util.regex.Pattern.quote(prefix))).toFuture(), 30.seconds)

  /** Heavy staged rows, written directly: the point is the payload on the wire, and a
   *  row without one cannot tell a projected read from an unprojected one. */
  private def seedHeavySiblings(): Unit = {
    val showtimes = (1 to 120).map(n =>
      Document("when" -> s"2026-08-0${n % 9 + 1}T20:00", "room" -> s"Hall $n", "url" -> s"https://cinema.test/$n"))
    Seq(1995, 2004, 2017).foreach(year =>
      Await.result(
        staged.insertOne(Document("_id" -> s"$prefix$year", "record" -> Document("showtimes" -> showtimes)))
          .toFuture(), 30.seconds))
  }

  /**
   * The lookup does not go to the database AT ALL any more, so the assertion is no
   * longer "how few bytes" but "none".
   *
   * The projection this spec was written for was the second round of the same problem.
   * First the warning decoded every sibling whole (19,748B a range, showtimes and all),
   * which took `bootCorpus` from 30 seconds to 3,360 and timed the leg out; projecting to
   * `_id` cut that to a few hundred bytes but left a QUERY per fresh insert, and a cold
   * pass inserts every row fresh — 121,544 round trips for the United States, to decide
   * whether to log a line. The `_id`s were in memory the whole time.
   *
   * Measured with the index already WARM, which is the state every call after the first
   * sees: `ensureAnchorIndex` reads the collection once per repository, and the scrape
   * path builds it on the venue's `findByCinema` before any row is written.
   */
  "the staging sibling lookup" should "not go to the database at all" in {
    val repository = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)
    purge()
    try {
      seedHeavySiblings()

      // The assertion is only meaningful if the rows are actually heavy. Without this a
      // future change that lightens the fixture would leave a test that passes whatever
      // the query does.
      val stored = Await.result(db.runCommand(Document("collStats" -> "pending_movies")).toFuture(), 30.seconds)
      val avgSize = stored.get("avgObjSize").map(_.asNumber().intValue()).getOrElse(0)
      withClue(s"fixture rows are only ${avgSize}B — too light for this assertion to mean anything: ") {
        avgSize should be > 3000
      }

      // Warm the row index, so what follows measures the LOOKUP and not the one-time
      // build every implementation of it has always paid.
      repository.findByAnchor(titleNormalizer.sanitize(title))

      Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)
      Await.result(db.getCollection[Document]("system.profile").drop().toFuture(), 30.seconds)
      Await.result(db.runCommand(Document("profile" -> 2)).toFuture(), 30.seconds)
      // A FRESH id, so `upsert` takes the insert branch that runs the sibling lookup.
      try repository.upsert(Multikino, title, Some(2029), MovieRecord())
      finally Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)

      val reads = Await.result(
        db.getCollection[Document]("system.profile")
          .find(Filters.and(Filters.eq("op", "query"), Filters.regex("ns", "pending_movies$")))
          .toFuture(), 30.seconds)

      // `upsert` still reads the row it is replacing (`recordAt`, one `_id` equality) —
      // that one carries the enrichment forward and has to happen. What must NOT be here
      // is a lookup that returns SEVERAL rows: that is the sibling range, and it is the
      // one this spec exists to keep out.
      val ranged = reads.filter(_.get("nreturned").exists(_.asNumber().intValue() > 1))
      withClue(s"the sibling lookup must answer from the in-memory row index, not the " +
               s"database — profiled ${ranged.size} multi-row read(s) on the insert path: ") {
        ranged shouldBe empty
      }
    } finally purge()
  }

  /**
   * The (cinema, anchor) lookup must answer what filtering the group answers — and must
   * not FETCH the group to do it.
   *
   * The detail step asks once per venue per film, so pulling the anchor's whole staging
   * group and filtering is quadratic in how many venues show it: 573 venues for a German
   * release is 328,329 document decodes for one film. The ids are the intersection of two
   * indexes the repository already keeps, so the fetch should be the pair's own rows.
   */
  "the (cinema, anchor) lookup" should "fetch one venue's rows, not the whole film's group" in {
    purge()
    try {
      val anchor = titleNormalizer.sanitize(title)
      // The same film staged at MANY venues — the shape that makes this quadratic.
      val venues: Seq[Source] = Seq(Multikino, models.CinemaCity, models.Helios)
      venues.foreach(v => repositoryUnderTest.upsert(v, title, Some(2026), MovieRecord()))

      val counting = new FetchCountingStagingRepository
      // Warm the index, then count only the lookup itself.
      counting.findByAnchor(anchor)
      counting.fetchedIds = 0

      val one = counting.findByCinemaAndAnchor(Multikino, anchor)
      withClue(s"the pair lookup fetched ${counting.fetchedIds} row(s) for a film staged at ${venues.size} venues: ") {
        counting.fetchedIds should be <= 1
      }
      withClue("and must still answer exactly what filtering the group answers: ") {
        one.map(_.id) shouldBe counting.findByAnchor(anchor).filter(_.cinema == Multikino).map(_.id)
        one should not be empty
      }
    } finally purge()
  }

  /** The scrape landing asks this once per diverted listing, so answering it by fetching
   *  the group re-opens the per-venue quadratic the question exists to avoid. */
  "holdsAnchor" should "answer what findByAnchor's emptiness answers, without fetching a row" in {
    purge()
    try {
      val anchor = titleNormalizer.sanitize(title)
      Seq[Source](Multikino, models.CinemaCity, models.Helios)
        .foreach(v => repositoryUnderTest.upsert(v, title, Some(2026), MovieRecord()))

      val counting = new FetchCountingStagingRepository
      val held   = counting.holdsAnchor(anchor)
      val absent = counting.holdsAnchor("a film nobody staged")
      withClue(s"holdsAnchor fetched ${counting.fetchedIds} row(s): ") { counting.fetchedIds shouldBe 0 }
      held shouldBe counting.findByAnchor(anchor).nonEmpty
      held shouldBe true
      absent shouldBe false
    } finally purge()
  }

  private def repositoryUnderTest = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)

  /** A Mongo staging repository that counts the rows it FETCHES by id — every group,
   *  venue and pair lookup goes through `fetchByIds`, so this is the decode count the
   *  quadratics below were paid in. */
  private class FetchCountingStagingRepository extends MongoStagingRepository(Some(db), normalizer = titleNormalizer) {
    var fetchedIds = 0
    override protected def fetchByIds(c: org.mongodb.scala.MongoCollection[services.movies.StoredMovieDto],
                                      ids: Seq[String]): scala.util.Try[Seq[services.movies.StoredMovieDto]] = {
      fetchedIds += ids.size
      super.fetchByIds(c, ids)
    }
  }

  /** The landing → kick → group-read path against Mongo, end to end: a new film landing
   *  at N venues must fetch its staging rows a number of times LINEAR in N. A kick per
   *  joining venue made the reaper fetch the whole growing group each time — 1 + 2 + … + N
   *  rows, which for a presale at 2,441 US venues timed the US sample leg out on
   *  2026-09-23. (Unit twin: `ScrapeCostIndependentOfCorpusSpec`.) */
  "landing one new film at many venues" should "fetch its staging rows linearly in the venue count" in {
    def rowsFetched(venueCount: Int): Int = {
      Await.result(staged.deleteMany(Filters.empty()).toFuture(), 30.seconds)
      val counting = new FetchCountingStagingRepository
      val reaper   = new services.staging.StagingReaper(
        new services.staging.StagingSteps(counting, Nil, (_, _, _) => None, (_, _, _) => None,
          new services.freshness.InMemoryFreshnessStore),
        new services.tasks.InMemoryTaskQueue, counting)
      val bus = new services.events.InProcessEventBus
      bus.subscribe(reaper.onNewcomerDiverted)
      val cache = new services.movies.CaffeineMovieCache(new services.movies.InMemoryMovieRepository, bus,
        staging = Some(counting), normalizer = titleNormalizer)
      models.Cinema.all.distinct.take(venueCount).foreach { venue =>
        cache.recordCinemaScrape(venue, Seq(models.CinemaMovie(models.Movie(title = "Presale Blockbuster", releaseYear = Some(2026)),
          venue, posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil)))
      }
      withClue("every venue must really have staged the film: ") { counting.findAll().size shouldBe venueCount }
      counting.fetchedIds
    }
    try {
      val small = rowsFetched(20)
      val large = rowsFetched(80)
      withClue(s"20 venues fetched $small staging row(s); 80 venues fetched $large — " +
        "a group read per joining venue is O(venues²): ") {
        large should be <= 2 * 80   // ~one fetch per venue landing, plus the single kick
        large.toDouble / small should be <= 4.5
      }
    } finally Await.result(staged.deleteMany(Filters.empty()).toFuture(), 30.seconds)
  }

  /** The detail half of the same chain against Mongo: each venue's finished detail step
   *  used to make the reaper fetch the film's whole group to learn whether any venue
   *  still owed detail — O(venues²) fetches per film. Only the finish that leaves none
   *  owing reads it now; the rest ask `cinemasUnder`, off the index.
   *  (Unit twin: `StagingQueueEndToEndSpec`.) */
  "a film's detail chain" should "fetch its staging rows linearly in its detail venues" in {
    def rowsFetched(venueCount: Int): Int = {
      Await.result(staged.deleteMany(Filters.empty()).toFuture(), 30.seconds)
      val counting = new FetchCountingStagingRepository
      val venues   = models.Cinema.all.distinct.take(venueCount)
      val chain    = new services.staging.StagingChain(counting, venues.map(new services.staging.StagingChain.CountingEnricher(_)))
      venues.foreach(v => counting.upsert(v, "Presale Blockbuster", Some(2026), services.staging.StagingChain.listing(v, "Presale Blockbuster")))
      counting.fetchedIds = 0
      chain.reaper.tick()
      chain.pump(limit = 10 * venueCount)
      withClue("the film must have graduated: ") { chain.movies.findAll() should have size 1 }
      counting.fetchedIds
    }
    try {
      val small = rowsFetched(20)
      val large = rowsFetched(80)
      withClue(s"20 detail venues fetched $small staging row(s); 80 fetched $large — " +
        "a group read per finished detail step is O(venues²): ") {
        large should be <= 8 * 80
        large.toDouble / small should be <= 4.5
      }
    } finally Await.result(staged.deleteMany(Filters.empty()).toFuture(), 30.seconds)
  }

  // The invariant an override must hold: answer EXACTLY what filtering `findAll` answers.
  // A previous attempt inferred the anchor from the `_id` — which holds the sanitized
  // title from the row's first write — and silently returned nothing once titles were
  // normalised, so the staging state machine stopped advancing. Comparing the two
  // implementations directly is what pins that.
  "findByAnchor" should "return exactly what filtering findAll returns, for every anchor" in {
    val repository = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)
    purge()
    try {
      val showtimes = (1 to 200).map(n =>
        Showtime(java.time.LocalDateTime.of(2026, 8, 1, 12, 0).plusMinutes(n.toLong * 7), None))
      Seq(1995, 2004, 2017).foreach(year =>
        repository.upsert(Multikino, title, Some(year),
          MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(
            title = Some(s"$title ($year)"), showtimes = showtimes)))))
      // A row whose DISPLAY title differs from the raw one it was first keyed under.
      repository.upsert(Multikino, "GHOST IN THE SHELL 2", Some(2032),
        MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Ghost in the Shell 2")))))

      val all     = repository.findAll()
      val anchors = all.map(row => titleNormalizer.sanitize(row.title)).distinct
      withClue("the fixture must produce at least two anchors: ") { anchors.size should be >= 2 }

      anchors.foreach { anchor =>
        val expected = all.filter(row => titleNormalizer.sanitize(row.title) == anchor)
        withClue(s"anchor '$anchor': ") {
          repository.findByAnchor(anchor).map(_.id).sorted shouldBe expected.map(_.id).sorted
        }
      }
    } finally purge()
  }

  // A read failure must cost TIME, not a film. Returning `Seq.empty` when the fetch fails
  // tells the reaper this film has no rows, so it skips its next step — indistinguishable
  // from the film being finished, and permanent, since nothing revisits it. That is the
  // same silent-degradation shape that hid a whole broken fold for a day.
  it should "fall back to a full scan when the id fetch fails, rather than losing the film" in {
    purge()
    try {
      val seeder = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)
      seeder.upsert(Multikino, title, Some(1995), MovieRecord())
      seeder.upsert(Multikino, title, Some(2017), MovieRecord())
      val anchor = titleNormalizer.sanitize(seeder.findAll().head.title)
      // Rows for THAT anchor only — `findAll` spans every film the fixture staged, and
      // `findByAnchor` answers for one.
      val ours   = seeder.findAll().filter(row => titleNormalizer.sanitize(row.title) == anchor)

      val broken = new MongoStagingRepository(Some(db), normalizer = titleNormalizer) {
        override protected def fetchByIds(
          c:   org.mongodb.scala.MongoCollection[services.movies.StoredMovieDto],
          ids: Seq[String]
        ): scala.util.Try[Seq[services.movies.StoredMovieDto]] =
          scala.util.Failure(new RuntimeException("simulated fetch failure"))
      }

      withClue("the fixture must have staged rows for this anchor: ") { ours should not be empty }
      withClue("the film's rows must still come back, via the slower path: ") {
        broken.findByAnchor(anchor).map(_.id).sorted shouldBe seeder.findByAnchor(anchor).map(_.id).sorted
      }
      withClue("a failed fetch must cost time, not the film: ") {
        broken.findByAnchor(anchor).map(_.id) should contain allElementsOf ours.map(_.id)
      }
    } finally purge()
  }
}
