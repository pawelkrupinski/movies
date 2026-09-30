package services.identity

import models.{CinemaCityChain, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.FakeDetailEnricher
import services.cinemas.common.FilmDetail
import services.events.{InProcessEventBus, RecordingEventBus, VenueDetailRead}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.InMemoryStagingRepository
import services.tasks.{DueWindow, EnrichDetailsHandler, EnrichDetailsTasks, StagingTaskKeys, Task, TaskType}
import tools.HttpStatusException

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._

/** The identity model's venue detail, read from the pipeline's own enrichment: a page answers only
 *  once the enrichment asked it, and the enrichment announces every page it asks. */
class VenueDetailSlotsSpec extends AnyFlatSpec with Matchers {

  private val clock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val Page  = "http://ref"
  /** The model asks venue detail only: any request here is a lookup that should never have been made. */
  private object NoNetwork extends tools.HttpFetch {
    def get(url: String): String = fail(s"unexpected request $url")
    def post(url: String, body: String, contentType: String): String = fail(s"unexpected request $url")
  }
  private val Group = "kino-apollo"
  private val Full  = FilmDetail(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155), releaseYear = Some(2021),
    originalTitle = Some("Dune"), countries = Seq("US"), synopsis = Some("Sand."))

  private final class World(enricher: FakeDetailEnricher, others: FakeDetailEnricher*) {
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), new InProcessEventBus(),
      normalizer = titleNormalizer, clock = clock)
    val staging   = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val freshness = new InMemoryFreshnessStore
    val bus       = new RecordingEventBus
    val slots     = new VenueDetailSlots(cache, staging, freshness, enricher +: others)
    val handler   = new EnrichDetailsHandler(Map(Group -> enricher), cache, freshness, new UptimeMonitor(), bus,
      new DueWindow(6.hours), clock = clock)

    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = Some(Page),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))))

    def enrich(): Unit = {
      val key = cache.keyOf("Dune", None)
      handler.handle(Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(Group, key),
        EnrichDetailsTasks.payload(enricher, key, Page), attempts = 1))
      slots.changed()
    }
  }

  "A venue's detail" should "be a gap until the pipeline's enrichment has asked its page" in {
    val world = new World(new FakeDetailEnricher(KinoApollo, Group, Some(Full)))
    world.slots.answer(new FakeDetailEnricher(KinoApollo, Group), Page) shouldBe None
  }

  it should "be what the enrichment merged into the venue's slot once it asked, announced after the stamp" in {
    val enricher = new FakeDetailEnricher(KinoApollo, Group, Some(Full))
    val world    = new World(enricher)
    world.enrich()
    world.bus.published should contain (VenueDetailRead(Group, Page))
    val answered = world.slots.answer(enricher, Page).flatten
    answered.map(d => (d.director, d.runtimeMinutes, d.releaseYear, d.originalTitle, d.countries)) shouldBe
      Some((Seq("Denis Villeneuve"), Some(155), Some(2021), Some("Dune"), Seq("US")))
  }

  it should "be no detail when the enrichment found the page gone, and still announce it" in {
    val enricher = new FakeDetailEnricher(KinoApollo, Group, failure = Some(new HttpStatusException(404, "GET", Page, None)))
    val world    = new World(enricher)
    world.enrich()
    world.bus.published should contain (VenueDetailRead(Group, Page))
    world.slots.answer(enricher, Page) shouldBe Some(None)
  }

  it should "come from a staged newcomer's row once staging stamped its detail" in {
    val enricher = new FakeDetailEnricher(KinoApollo, Group)
    val world    = new World(enricher)
    val slot     = SourceData(title = Some("Arco"), filmUrl = Some("http://arco"), director = Seq("Ugo Bienvenu"), releaseYear = Some(2025))
    world.staging.upsert(KinoApollo, "Arco", None, MovieRecord(data = Map(KinoApollo -> slot)))
    world.slots.changed()
    world.slots.answer(enricher, "http://arco") shouldBe None
    world.freshness.markFresh(StagingTaskKeys.detailKey(titleNormalizer.sanitize("Arco"), KinoApollo.displayName),
      FreshnessKind.DetailEnrich, clock.instant())
    world.slots.answer(enricher, "http://arco").flatten.map(_.director) shouldBe Some(Seq("Ugo Bienvenu"))
  }

  /** A chain lands every page of a row on one shared slot: it is a page's answer only when the row
   *  names that chain no other page. */
  "A chain's shared detail slot" should "answer the one page the row names for the chain" in {
    val chain = new FakeDetailEnricher(CinemaCityPoznanPlaza, "cinema-city", target = Some(CinemaCityChain))
    val world = new World(chain, new FakeDetailEnricher(CinemaCityKinepolis, "cinema-city", target = Some(CinemaCityChain)))
    val key   = world.cache.keyOf("Lalka", None)
    world.cache.put(key, MovieRecord(data = Map(
      CinemaCityPoznanPlaza -> SourceData(filmUrl = Some("http://cc/lalka")),
      CinemaCityKinepolis   -> SourceData(filmUrl = Some("http://cc/lalka")),
      CinemaCityChain       -> SourceData(director = Seq("Maciej Kawalski")))))
    world.freshness.markFresh(EnrichDetailsTasks.readMarker(EnrichDetailsTasks.dedupKey("cinema-city", key)),
      FreshnessKind.DetailEnrich, clock.instant())
    world.slots.changed()
    world.slots.answer(chain, "http://cc/lalka").flatten.map(_.director) shouldBe Some(Seq("Maciej Kawalski"))
  }

  it should "answer nothing when the row names the chain two pages, whose facts it cannot tell apart" in {
    val chain = new FakeDetailEnricher(CinemaCityPoznanPlaza, "cinema-city", target = Some(CinemaCityChain))
    val world = new World(chain, new FakeDetailEnricher(CinemaCityKinepolis, "cinema-city", target = Some(CinemaCityChain)))
    val key   = world.cache.keyOf("Lalka", None)
    world.cache.put(key, MovieRecord(data = Map(
      CinemaCityPoznanPlaza -> SourceData(filmUrl = Some("http://cc/lalka")),
      CinemaCityKinepolis   -> SourceData(filmUrl = Some("http://cc/ladies-night-lalka")),
      CinemaCityChain       -> SourceData(director = Seq("Rod Blackhurst")))))
    world.freshness.markFresh(EnrichDetailsTasks.readMarker(EnrichDetailsTasks.dedupKey("cinema-city", key)),
      FreshnessKind.DetailEnrich, clock.instant())
    world.slots.changed()
    world.slots.answer(chain, "http://cc/lalka") shouldBe None
    world.slots.answer(chain, "http://cc/ladies-night-lalka") shouldBe None
  }

  "The model's detail lookup" should "read the page's key and answer Unknown for an unasked page, Known once asked" in {
    val enricher = new FakeDetailEnricher(KinoApollo, Group, Some(Full))
    val world    = new World(enricher)
    val reads    = new ObservationReads
    val lookups  = new TmdbIdentityLookups(new clients.TmdbClient(NoNetwork, apiKey = None),
      new services.enrichment.ImdbClient(NoNetwork),
      Seq(new SourceDataDetailEnricher(enricher, world.slots, new LookupGaps, reads)), new LookupGaps)
    val listing  = Listing.of(KinoApollo, CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = Some(Page),
      synopsis = None, cast = Seq.empty, director = Seq.empty, showtimes = Nil), titleNormalizer)

    reads.asking(ObservationReads.Question.Detail(listing.key))(lookups.detail(listing)) shouldBe Answer.Unknown
    world.enrich()
    reads.asking(ObservationReads.Question.Detail(listing.key))(lookups.detail(listing)) shouldBe
      Answer.Known(Some(DetailFacts(Some(2021), Seq("Denis Villeneuve"), Some(155), Some("Dune"), Seq("US"))))
    reads.changedBy(Seq(VenueDetailSlots.keyOf(Group, Page))).details shouldBe Set(listing.key)
  }
}
