package services

import tools.SpecTimeouts

import models.{CinemaShowing, KinoMuranow, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{FilmId, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository}
import services.readmodel.MongoReadModelRepository
import services.sharecards.{MongoFacebookRescrapeStore, RescrapeEntry, RescrapeKind, RescrapeTarget}
import services.tasks.{MongoChunkScrapeStore, MongoTaskQueue, TaskType}
import tools.QueryPlans

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Every query and write filter a store sends on its hot path, planned by a real Mongo: each must be
 * served by an index — no collection scan, and no in-memory sort — unless it is listed below with why
 * a scan is right.
 *
 * A collection scan answers correctly, so no result-checking spec sees one; it shows only at production
 * size, as load. The read model's "cheap" drift counts scanned every screening on every backstop tick, and
 * `uptimeServiceTags` scanned its collection on every tag upsert. This holds each store's actual
 * commands — recorded off the wire, not restated — to an index.
 */
class QueryPlanIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def plansOf(purpose: String)(work: MongoDatabase => Unit): QueryPlans.Recorded = QueryPlans.of(mongoTarget, purpose)(work)

  private def assertIndexed(recorded: QueryPlans.Recorded, allowed: Map[String, String] = Map.empty,
                            unread: Map[String, String] = Map.empty): Unit =
    withClue(s"every planned statement:\n${recorded.plans.map(_.toString).distinct.mkString("\n")}\n") {
      QueryPlans.violations(recorded, allowed, unread) shouldBe empty
    }

  /** Every stamp from the TTL-safe pinned clock: a row stamped in the past could be expired mid-spec. */
  private val Far = _root_.tools.MongoTtlSpecClock.Pinned.instant()

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, SpecTimeouts.Io)

  /** Indexes built off the caller's thread are awaited, or the plan would race their creation. */
  private def awaitIndexes(db: MongoDatabase, collection: String, count: Int): Unit =
    tools.Eventually.eventually(await(db.getCollection(collection).listIndexes().toFuture()).size should be >= count)

  "the read model's drift counts" should "count index keys, never read a document" in {
    var counted = Option.empty[(Long, Long)]
    val recorded = plansOf("readmodel") { db =>
      val rm = new MongoReadModelRepository(Some(db))
      tools.ReadModelSnapshot.loadInto(rm, tools.ReadModelSnapshot.read())
      counted = for (m <- rm.countMovies().answered; s <- rm.countScreenings().answered) yield (m, s)
      rm.close()
    }
    counted.exists { case (m, s) => m > 100 && s > 100 } shouldBe true
    val counts = recorded.plans.filter(_.statement.getFirstKey == "aggregate")
    counts should have size 2
    counts.foreach(c => withClue(s"$c: ")(c.docsExamined shouldBe 0L))
    assertIndexed(recorded)
  }

  "the task queue" should "enqueue, claim, settle, reap and count by index" in {
    assertIndexed(plansOf("tasks") { db =>
      val queue = new MongoTaskQueue(Some(db.withCodecRegistry(services.movies.MovieCodecs.registry)))
      awaitIndexes(db, "tasks", 4)
      val t0    = Far
      (1 to 20).foreach(i => queue.enqueue(TaskType.ScrapeCinema, s"scrape|venue-$i", Map.empty, t0.plusSeconds(i.toLong), None, Duration.Zero))
      val claimed = queue.claim("worker-a", 1.minute, t0.plusSeconds(60)).get
      queue.complete(claimed.id, "worker-a")
      val second = queue.claim("worker-a", 1.minute, t0.plusSeconds(60)).get
      queue.release(second.id, "worker-a", Some("boom"), Some(t0.plusSeconds(600)), refundAttempt = false)
      queue.claim("worker-b", 1.minute, t0.plusSeconds(60))
      queue.reapExpiredLeases(t0.plusSeconds(3600))
      queue.countByState()
      queue.waitingCount(TaskType.ScrapeCinema)
      queue.amendWaiting("scrape|venue-20", Map("a" -> "b"))
    })
  }

  "the fleet's Facebook re-scrape queue" should "claim its oldest due entry off the index, sorting nothing in memory" in {
    assertIndexed(plansOf("rescrapes") { db =>
      val store = new MongoFacebookRescrapeStore(db.getCollection[Document](MongoFacebookRescrapeStore.Collection))
      awaitIndexes(db, MongoFacebookRescrapeStore.Collection, 2)
      val t0 = Far
      store.add((1 to 30).map(i => RescrapeEntry("pl", RescrapeTarget.Page(s"https://kinowo.net/f/$i"), t0.plusSeconds(i.toLong * (if (i % 2 == 0) 1 else 1000)))))
      store.add(Seq(RescrapeEntry("us", RescrapeTarget.FilmPages("us", "film-1"), t0)))
      store.hasDue("pl", RescrapeKind.Page, t0.plusSeconds(100)) shouldBe true
      val claimed = store.claim("pl", RescrapeKind.Page, t0.plusSeconds(100), 5.minutes).get
      store.complete(claimed)
      store.claim("pl", RescrapeKind.Page, t0.plusSeconds(100), 5.minutes).foreach(store.retry(_, t0.plusSeconds(900), countAttempt = true))
      store.waitingPages("pl")
    })
  }

  "the film store and its side collections" should "find, write, patch and delete a film by index" in {
    assertIndexed(plansOf("films") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), _root_.tools.MongoTtlSpecClock.Pinned, screenings = Some(screenings), slots = Some(slots),
                                                normalizer = titleNormalizer)
      val at   = LocalDateTime.of(2099, 3, 1, 18, 0)
      val shop = CinemaShowing(KinoMuranow, "belle")
      def record(title: String, tmdbId: Int, hours: Int*) = MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        shop -> SourceData(title = Some(title), filmUrl = Some(s"https://muranow.pl/$title"), showtimes = hours.map(h => Showtime(at.withHour(h), None))),
        Tmdb -> SourceData(title = Some(title))))
      (1 to 10).foreach(i => repository.upsert(s"Film $i", Some(2000 + i), record(s"Film $i", 100 + i, 18)))
      repository.updateIfPresent("Film 1", Some(2001), record("Film 1", 101, 18), record("Film 1", 101, 18, 21)) shouldBe true
      val stored = repository.findByKeyChecked(services.movies.CacheKey("Film 2", Some(2002), titleNormalizer)).answered.get
      repository.findByIdChecked(stored.id).answered shouldBe defined
      repository.findByIdChecked(FilmId("no-such-film")).answered shouldBe empty
      repository.delete("Film 3", Some(2003))
      // The change-stream catch-up in steady state: nothing written since its cursor.
      repository.foreachRecordUpdatedSince(Far.plusSeconds(86400))(_ => ()) shouldBe tools.ScanOutcome.Complete
      repository.close()
    }, allowed = Map("movies find filter{updatedAt:{$gt}} sort{_id}" -> (
      "the change-stream catch-up: the updatedAt range is the rows written since the cursor (none, in steady state), " +
      "and the keyset page's limit makes its sort a top-k of one page")),
    unread = Seq("screenings", "movie_slots").map(side => s"$side.listingKey_1" -> (
      "a read by listing is the identity migration's next phase (docs/design/identity-resolver.md: slots move by ListingKey); " +
      "nothing reads it yet")).toMap)
  }

  "the scrape archive" should "record, read and scan venues by id, and carry no index nothing reads" in {
    import models.{Cinema, CinemaMovie, Movie}
    import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}
    assertIndexed(plansOf("archive") { db =>
      val repository = new MongoScrapeArchiveRepository(Some(db))
      val at = Far
      def film(title: String, cinema: Cinema) = CinemaMovie(movie = Movie(title, None, None, Nil, Nil, None, None), cinema = cinema,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = Seq(Showtime(LocalDateTime.parse("2026-08-01T18:00"), bookingUrl = None)))
      val cinemas = Cinema.all.take(5)
      cinemas.foreach(c => repository.record(ScrapeAttempt(cinema = c, city = Cinema.cityOf(c), at = at, listingComplete = true,
        films = Seq(film(s"Film at ${c.displayName}", c)))))
      repository.record(ScrapeAttempt(cinema = cinemas.head, city = Cinema.cityOf(cinemas.head), at = at.plusSeconds(3600),
        listingComplete = true, films = Nil))
      repository.find(cinemas(1))
      repository.contentStamps()
      repository.scanLean(_ => true)(_ => ()) shouldBe tools.ScanOutcome.Complete
      repository.findAll() should have size 5
      repository.close()
    })
  }

  "the chunked-scrape store" should "read and clear a run's chunks by index" in {
    assertIndexed(plansOf("chunks") { db =>
      val store = new MongoChunkScrapeStore(Some(db))
      val now   = Far
      awaitIndexes(db, "scrape_chunks", 3)
      val run   = store.startRun("helios-lodz", Seq("a", "b"), now, 1.hour).get
      store.storeChunk("helios-lodz", run, "a", "{}", now)
      store.storedKeys("helios-lodz", run)
      store.loadChunks("helios-lodz", run)
      store.activeRun("helios-lodz")
      store.completeRun("helios-lodz", run)
    })
  }

  "the TMDB store" should "read, write and sweep by id" in {
    import services.identity.{MongoTmdbDocuments, TmdbKind, TmdbStore}
    assertIndexed(plansOf("tmdb") { db =>
      val documents = new MongoTmdbDocuments(db)
      def film(n: Int) = new org.bson.BsonDocument(TmdbStore.FetchedAt, new org.bson.BsonInt64(n.toLong))
      documents.put(TmdbKind.Film, (1 to 20).map(n => s"$n" -> film(n)))
      documents.get(TmdbKind.Film, Seq("1", "2", "99"))
      documents.answers(TmdbKind.Film, Seq("3"))
      val stale = documents.fetchedBefore(TmdbKind.Film, 10L)
      documents.deleteIfStill(TmdbKind.Film, stale) shouldBe 9
    })
  }

  "the identity trace store and its admin reads" should "write by family and id, and read a rule's, a film's, a blocker's listings by index" in {
    import services.identity.{FamilyTraces, ListingTrace, MongoIdentityTraceReads, MongoIdentityTraceStore}
    import services.movies.ListingKey
    assertIndexed(plansOf("traces") { db =>
      def key(n: Int) = ListingKey.Published(s"Venue $n", s"Film $n", None, Nil)
      def trace(n: Int, family: String) = ListingTrace(key(n), family, Some(100 + n % 3), "OwnMatch", Seq(s"accept:rule-${n % 2}"), None, Nil,
        Some(100 + n % 3))
      val store = new MongoIdentityTraceStore(db)
      store.replace(Set.empty, FamilyTraces.of((1 to 20).map(n => trace(n, s"f${n % 4}")) :+
        trace(21, "f9").copy(film = None, blocker = Some("search:found-nothing"))))
      store.flush()
      store.replace(Set("f1"), FamilyTraces.of(Seq(trace(1, "f1"))))
      store.flush()
      val reads = new MongoIdentityTraceReads(db)
      reads.byRule("accept:rule-1", 10)
      reads.byFilm(101, 10)
      reads.byBlocker("search:found-nothing", 10)
      reads.unresolved(10, _ => true)
      reads.byTitle("film 2", 10)
      reads.ruleCounts()
      reads.blockers()
      store.close()
    }, allowed = Map(
      "identity_traces find filter{listing.rawTitle}" ->
        "the admin page's free-text title search, by hand: a case-insensitive substring match no index can serve",
      "identity_traces aggregate pipeline[{$unwind},{$group:{n:{$sum},_id}},{$sort:{n}}]" ->
        "the admin page's every-rule count: a tally of the whole collection, by hand",
      "identity_traces find filter{$and:[{blocker:{$exists}},{blocker:{$not}}]} sort{_id}" -> (
        "ProposalFill's page of unresolved listings in _id order: the sparse blocker index holds only the unresolved, and " +
        "the page's limit makes the sort a top-k (an _id-ordered partial index is not allowed on _id)")))
  }

  "the venue page store" should "read, write and scan pages by id" in {
    import services.venuepages.{MongoVenuePageStore, VenuePage, VenuePageKey}
    assertIndexed(plansOf("venue-pages") { db =>
      val store = new MongoVenuePageStore(db)
      (1 to 10).foreach(n => store.put(VenuePage(VenuePageKey("helios", s"/film/$n"), VenuePage.Gone(404), Far)))
      store.get(VenuePageKey("helios", "/film/3")) shouldBe defined
      store.foreach(_ => ()) shouldBe tools.ScanOutcome.Complete
    })
  }

  "the stores keyed by id alone" should "read, write and sweep by id" in {
    import services.attempts.{AttemptOutcome, EnrichmentAttempt, MongoEnrichmentAttemptReader, MongoEnrichmentAttemptStore}
    import services.cadence.{MongoRatingCadenceReader, MongoRatingCadenceStore}
    import services.freshness.{FreshnessKind, MongoFreshnessStore}
    import services.identity.MongoVenueSlotFingerprints
    import services.movies.ChangeStreamResumeToken
    import services.closure.MongoClosureLedger
    import services.resolution.MongoResolutionStore
    import services.tasks.{MongoScrapeCostStore, ScrapeCost}
    assertIndexed(plansOf("by-id") { db =>
      val keys = (1 to 10).map(n => s"imdb|tmdb:$n")
      val freshness = new MongoFreshnessStore(Some(db))
      Await.result(freshness.whenReady(FreshnessKind.ImdbRating), SpecTimeouts.Io)
      keys.foreach(freshness.markFresh(_, FreshnessKind.ImdbRating, Far))
      freshness.invalidate(keys.head)
      val attempts = new MongoEnrichmentAttemptStore(Some(db))
      keys.foreach(attempts.record(_, EnrichmentAttempt(Far, 12L, AttemptOutcome.Unchanged)))
      new MongoEnrichmentAttemptReader(Some(db)).forKeys(keys.take(3))
      val cadence = new MongoRatingCadenceStore(Some(db))
      keys.foreach(cadence.record(_, Some("7.1"), Far))
      val cadenceReads = new MongoRatingCadenceReader(Some(db))
      tools.Eventually.eventually(cadenceReads.all() should have size 10)
      cadenceReads.forKeys(keys.take(3))
      Seq(freshness.retention, attempts.retention, cadence.retention).foreach { rows =>
        tools.Eventually.eventually(rows.stampedBefore(Far.plusSeconds(1)) should not be empty)
        rows.deleteIfStill(rows.stampedBefore(Far.plusSeconds(1)).take(2))
      }
      freshness.close(); attempts.close(); cadence.close()
      val resolutions = new MongoResolutionStore(Some(db), "resolve_imdb", normalizer = titleNormalizer,
        ttlMismatches = new TtlIndexMismatches, clock = _root_.tools.MongoTtlSpecClock.Pinned)
      resolutions.put("anora|2024", "tt28607951")
      tools.Eventually.eventually(resolutions.get("anora|2024") shouldBe defined)
      resolutions.removeForFilm("anora")
      val costs = new MongoScrapeCostStore(db)
      (1 to 3).foreach(n => costs.record(s"scrape|venue-$n", ScrapeCost(n)))
      costs.recent()
      val ledger = new MongoClosureLedger(db)
      ledger.confirm("Kino Gone", Far); ledger.confirmed(); ledger.withdraw("Kino Gone")
      val fingerprints = new MongoVenueSlotFingerprints(db)
      fingerprints.update((1L to 10L).toSet, Set.empty); fingerprints.update(Set(11L), Set(1L)); fingerprints.all()
      val token = new ChangeStreamResumeToken("movies", Some(db), enabled = true)
      token.advance(new org.bson.BsonDocument("_data", new org.bson.BsonString("826A")), token.generation)
      token.save(force = true)
      token.load().answered shouldBe defined
      token.clear()
    })
  }

  "the uptime monitor" should "flush buckets and tag services by index" in {
    val recorded = plansOf("uptime") { db =>
      val monitor = new UptimeMonitor(Some(db), clock = _root_.tools.MongoTtlSpecClock.Pinned)
      awaitIndexes(db, "uptimeBuckets", 3)
      awaitIndexes(db, ServiceTags.Collection, 2)
      (1 to 5).foreach { i => monitor.recordSuccess(s"venue-$i", 120L); monitor.recordFailure(s"venue-$i", "boom") }
      (1 to 5).foreach(i => monitor.tagService(s"venue-$i", Set("chain:helios")))
      monitor.flushNow()
      // Both writes are fire-and-forget: wait for them to land, or the plans would race their sending.
      Seq("uptimeBuckets", ServiceTags.Collection).foreach(written => tools.Eventually.eventually(
        await(db.getCollection(written).estimatedDocumentCount().toFuture()) shouldBe 5L))
      monitor.close()
    }
    Seq("uptimeBuckets", ServiceTags.Collection).foreach(written => withClue(s"$written written, among ${recorded.plans}: ")(
      recorded.plans.exists(p => p.collection == written && p.statement.getFirstKey == "update") shouldBe true))
    assertIndexed(recorded)
  }
}
