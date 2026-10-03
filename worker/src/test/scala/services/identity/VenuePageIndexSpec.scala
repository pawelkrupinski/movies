package services.identity

import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.FakeDetailEnricher
import services.cinemas.common.FilmDetail
import services.events.{RecordingEventBus, VenueDetailRead}
import services.freshness.InMemoryFreshnessStore
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.tasks.{DueWindow, EnrichDetailsHandler, EnrichDetailsTasks, Task, TaskType}
import services.venuepages.{InMemoryVenuePageStore, VenuePage, VenuePageKey}
import tools.HttpStatusException

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._

/** The identity model's venue detail, read from venue_pages: a page answers once it is read there, and
 *  its read is announced; answers move only at the model's settle, which reports every one that moved. */
class VenuePageIndexSpec extends AnyFlatSpec with Matchers {

  private val clock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val Page  = "http://ref"
  private val Group = "kino-apollo"
  private val Full  = FilmDetail(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155), releaseYear = Some(2021),
    originalTitle = Some("Dune"), countries = Seq("US"), synopsis = Some("Sand."))
  /** The model asks venue detail only: any request here is a lookup that should never have been made. */
  private object NoNetwork extends tools.HttpFetch {
    def get(url: String): String = fail(s"unexpected request $url")
    def post(url: String, body: String, contentType: String): String = fail(s"unexpected request $url")
  }

  private final class World {
    val pages    = new InMemoryVenuePageStore
    val reported = scala.collection.mutable.ArrayBuffer.empty[String]
    val index    = new VenuePageIndex(pages, reported += _)
    def store(group: String, page: String, outcome: VenuePage.Outcome): Unit = {
      pages.put(VenuePage(VenuePageKey(group, page), outcome, clock.instant())); index.pageRead(group, page)
    }
  }

  // The first settle reads the whole store; a page read and announced while that scan runs, past the
  // point the scan reached, must still be taken in by the next settle — not wiped with the scan's
  // own announcements, which left it a gap for good.
  "A page read while the first settle scans the store" should "be taken in by the next settle" in {
    val pages = new InMemoryVenuePageStore
    pages.put(VenuePage(VenuePageKey(Group, "http://a"), VenuePage.Read(Full), clock.instant()))
    val reported = scala.collection.mutable.ArrayBuffer.empty[String]
    var index: VenuePageIndex = null
    val scanning = new services.venuepages.VenuePageStore {
      def get(key: VenuePageKey) = pages.get(key)
      def put(page: VenuePage)   = pages.put(page)
      def foreach(onPage: VenuePage => Unit): tools.ScanOutcome = {
        pages.foreach(onPage).isComplete shouldBe true
        // The scan has passed every page it will see; a reader writes and announces a new one now.
        pages.put(VenuePage(VenuePageKey(Group, Page), VenuePage.Read(Full), clock.instant()))
        index.pageRead(Group, Page)
        tools.ScanOutcome.complete
      }
    }
    index = new VenuePageIndex(scanning, reported += _)
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    index.settle()
    index.answer(enricher, Page) shouldBe None
    index.settle()
    index.answer(enricher, Page) shouldBe Some(Some(Full))
    reported shouldBe Seq(VenuePageIndex.keyOf(Group, Page))
  }

  // A settle whose page read throws (a Mongo timeout) fails the drain; the pages it had not read yet
  // must still be noted for the next settle — dropped, their answers stayed stale until announced again.
  "A settle whose page read fails" should "leave every page it did not take in for the next settle" in {
    val pages    = new InMemoryVenuePageStore
    val reported = scala.collection.mutable.ArrayBuffer.empty[String]
    var failing  = true
    val flaky = new services.venuepages.VenuePageStore {
      def get(key: VenuePageKey) = if (failing) throw new IllegalStateException("timed out") else pages.get(key)
      def put(page: VenuePage)   = pages.put(page)
      def foreach(onPage: VenuePage => Unit): tools.ScanOutcome = pages.foreach(onPage)
    }
    val index    = new VenuePageIndex(flaky, reported += _)
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    index.settle()                                                   // the baseline: an empty store
    Seq("http://a", "http://b").foreach { page =>
      pages.put(VenuePage(VenuePageKey(Group, page), VenuePage.Read(Full), clock.instant())); index.pageRead(Group, page)
    }
    an[IllegalStateException] should be thrownBy index.settle()
    failing = false
    index.settle()
    index.answer(enricher, "http://a") shouldBe Some(Some(Full))
    index.answer(enricher, "http://b") shouldBe Some(Some(Full))
    reported.toSet shouldBe Set(VenuePageIndex.keyOf(Group, "http://a"), VenuePageIndex.keyOf(Group, "http://b"))
  }

  // A failed read is not data: a first scan a Mongo error stopped short read as the whole store left
  // every page past the failure a gap until that page happened to be read again.
  "A first scan stopped short by a failed read" should "be scanned again at the next settle, reporting the pages it missed" in {
    val pages = new InMemoryVenuePageStore
    pages.put(VenuePage(VenuePageKey(Group, "http://a"), VenuePage.Read(Full), clock.instant()))
    pages.put(VenuePage(VenuePageKey(Group, "http://b"), VenuePage.Read(Full), clock.instant()))
    var failNextScan = true
    val failing = new services.venuepages.VenuePageStore {
      def get(key: VenuePageKey) = pages.get(key)
      def put(page: VenuePage)   = pages.put(page)
      def foreach(onPage: VenuePage => Unit): tools.ScanOutcome =
        if (failNextScan) { failNextScan = false; pages.get(VenuePageKey(Group, "http://a")).foreach(onPage); tools.ScanOutcome.of(whole = false, "scan fails on purpose") }
        else pages.foreach(onPage)
    }
    val reported = scala.collection.mutable.ArrayBuffer.empty[String]
    val index    = new VenuePageIndex(failing, reported += _)
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    index.settle()
    index.answer(enricher, "http://b") shouldBe None
    index.settle()
    index.answer(enricher, "http://b") shouldBe Some(Some(Full))
    reported.toSeq shouldBe Seq(VenuePageIndex.keyOf(Group, "http://b"))
  }

  "A venue page" should "be a gap until it is read into venue_pages" in {
    new World().index.answer(new FakeDetailEnricher(KinoApollo, Group), Page) shouldBe None
  }

  it should "answer what the page stated once read, and no detail once found gone" in {
    val world    = new World
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    world.index.settle()
    world.store(Group, Page, VenuePage.Read(Full)); world.index.settle()
    world.index.answer(enricher, Page) shouldBe Some(Some(Full))
    world.store(Group, Page, VenuePage.Gone(404)); world.index.settle()
    world.index.answer(enricher, Page) shouldBe Some(None)
  }

  it should "keep the answer the model was last told of until the next settle, and report then each page that moved" in {
    val world    = new World
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    world.store(Group, "http://arco", VenuePage.Read(Full))
    world.index.settle()
    world.reported shouldBe empty                                              // the first read is the baseline
    world.store(Group, Page, VenuePage.Read(Full))
    world.index.answer(enricher, Page) shouldBe None                           // announced, not yet settled
    world.index.settle()
    world.reported.toSeq shouldBe Seq(VenuePageIndex.keyOf(Group, Page))
    world.index.answer(enricher, Page) shouldBe Some(Some(Full))
    world.store(Group, Page, VenuePage.Read(Full)); world.index.settle()         // read again, unchanged: nothing to tell
    world.reported.size shouldBe 1
  }

  // Cinema City lists "Lalka" and "Ladies Night - Lalka" on one film row; their facts once shared one slot,
  // so neither could be told. By page, each answers its own.
  "Two pages of one chain" should "each answer their own page's facts" in {
    val world  = new World
    val plaza  = new FakeDetailEnricher(CinemaCityPoznanPlaza, "cinema-city")
    val lalka  = FilmDetail(director = Seq("Maciej Kawalski"), releaseYear = Some(2026))
    val dolly  = FilmDetail(director = Seq("Rod Blackhurst"), releaseYear = Some(2025))
    world.store("cinema-city", "http://cc/lalka", VenuePage.Read(lalka))
    world.store("cinema-city", "http://cc/ladies-night-lalka", VenuePage.Read(dolly))
    world.index.settle()
    world.index.answer(plaza, "http://cc/lalka") shouldBe Some(Some(lalka))
    world.index.answer(new FakeDetailEnricher(CinemaCityKinepolis, "cinema-city"), "http://cc/ladies-night-lalka") shouldBe Some(Some(dolly))
  }

  "The detail enrichment" should "make the page answer through venue_pages, and announce it" in {
    val world    = new World
    val enricher = new FakeDetailEnricher(KinoApollo, Group, Some(Full))
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer)
    val key      = cache.keyOf("Dune", None)
    cache.put(key, MovieRecord(data = Map(models.CinemaShowing.keyFor(KinoApollo, "Dune", titleNormalizer) -> SourceData(
      title = Some("Dune"), filmUrl = Some(Page),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book")))))))
    val bus = new RecordingEventBus
    new EnrichDetailsHandler(Map(Group -> enricher), cache, new InMemoryFreshnessStore, new UptimeMonitor(), bus, new DueWindow(6.hours),
      clock = clock, pages = world.pages).handle(Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(Group, key),
      EnrichDetailsTasks.payload(enricher, key, Page), attempts = 1))
    bus.published should contain (VenueDetailRead(Group, Page))
    world.index.pageRead(Group, Page); world.index.settle()
    world.index.answer(enricher, Page) shouldBe Some(Some(Full))

    val gone = new FakeDetailEnricher(KinoApollo, Group, failure = Some(new HttpStatusException(404, "GET", "http://gone", None)))
    new EnrichDetailsHandler(Map(Group -> gone), cache, new InMemoryFreshnessStore, new UptimeMonitor(), bus, new DueWindow(6.hours),
      clock = clock, pages = world.pages).handle(Task("id2", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(Group, key),
      EnrichDetailsTasks.payload(gone, key, "http://gone"), attempts = 1))
    world.index.pageRead(Group, "http://gone"); world.index.settle()
    world.index.answer(gone, "http://gone") shouldBe Some(None)
  }

  "The model's detail lookup" should "read the page's key and answer Unknown for an unread page, Known once read" in {
    val world    = new World
    val enricher = new FakeDetailEnricher(KinoApollo, Group, Some(Full))
    val reads    = new ObservationReads
    val lookups  = new TmdbIdentityLookups(new clients.TmdbClient(NoNetwork, apiKey = None),
      new services.enrichment.ImdbClient(NoNetwork), Seq(new VenuePageDetailEnricher(enricher, world.index, new LookupGaps, reads)), new LookupGaps)
    val listing  = Listing.of(KinoApollo, CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = Some(Page),
      synopsis = None, cast = Seq.empty, director = Seq.empty, showtimes = Nil), titleNormalizer)

    reads.asking(ObservationReads.Question.Detail(listing.key))(lookups.detail(listing)) shouldBe Answer.Unknown
    world.store(Group, Page, VenuePage.Read(Full)); world.index.settle()
    reads.asking(ObservationReads.Question.Detail(listing.key))(lookups.detail(listing)) shouldBe
      Answer.Known(Some(DetailFacts(Some(2021), Seq("Denis Villeneuve"), Some(155), Some("Dune"), Seq("US"))))
    reads.changedBy(Seq(VenuePageIndex.keyOf(Group, Page))).details shouldBe Set(listing.key)
  }
}
