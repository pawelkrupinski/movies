package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.duration._

/** The model kept by the pipeline's events: venues' archived scrapes diffed into listings seen and
 *  gone, new observations mapped back to the questions that read them, a restart taking up what
 *  the store kept — after every drain, what a whole resolve of the listings held decides. */
class IdentityModelServiceSpec extends AnyFlatSpec with Matchers with LoneElement {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.resolver

  /** A film database whose every answer is read under a store key (`q:`, `f:`), as the observation
   *  store's readers name them — and whose answers a test can change, as the fill does. */
  private final class Keyed(reads: ObservationReads) extends IdentityLookups {
    var titles: Map[String, Seq[Hit]] = Map(
      "lalka"   -> Seq(Hit(1, "Lalka", None, Some(1968), 10), Hit(2, "Lalka", None, Some(2025), 50)),
      "matilda" -> Seq(Hit(3, "Matilda", None, Some(1996), 20)))
    def hasDetail(listing: Listing): Boolean = false
    def detail(listing: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = {
      reads.read("q:" + query.sortKey)
      query match {
        case CandidateQuery.Title(text) => Answer.Known(titles.collect { case (word, hits) if normalizer.searchQuery(text).toLowerCase.contains(word) => hits }.flatten.toSeq)
        case _                          => Answer.Known(Nil)
      }
    }
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
      reads.read(s"f:$id")
      Answer.Known(titles.values.flatten.find(_.tmdbId == id).map(hit => IdentityMeasures.Film(hit.title, None, Nil, hit.year)))
    }
  }

  private def movie(cinema: Cinema, title: String, year: Option[Int] = None): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, Some(s"${cinema.displayName}/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None)))

  private def listingsOf(scrapes: Map[Cinema, Seq[CinemaMovie]]) =
    Listing.distinct(Listing.all(scrapes.toSeq, normalizer))

  private def decided(decisions: Seq[ResolverDecision]) = decisions.map(d => (d.listings, d.film)).toSet

  private final class World {
    val reads   = new ObservationReads
    val lookups = new Keyed(reads)
    val store   = new InMemoryIdentityModelStore
    var scrapes = Map.empty[Cinema, Seq[CinemaMovie]]
    def service(): IdentityModelService = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(lookups, reads), normalizer, calibration, store = store),
      reads, () => listingsOf(scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor())
    def scrape(service: IdentityModelService, cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
      scrapes += cinema -> films; service.venueScraped(cinema, films)
    }
    def expected: Set[(Set[ListingKey], Option[Int])] =
      decided(IdentityResolver.resolve(listingsOf(scrapes), lookups, normalizer, calibration).decisions)
  }

  "the model service" should "decide as a whole resolve after each drain of venues' scrapes, and sweep a film a venue stopped listing" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain().map(_.venues) shouldBe Some(2)
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected

    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))           // Matilda no longer listed
    service.drain()
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    service.peek(10.seconds).get.resolution.decisions.flatMap(_.film) should not contain 3
  }

  it should "re-read exactly the questions a new observation answers" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Matilda")))
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain()
    world.lookups.titles += "matilda" -> Seq(Hit(4, "Matilda", None, Some(2022), 90))       // the fill filed a new answer
    val matilda = world.reads.changedBy(Seq("q:" + CandidateQuery.Title("Matilda").sortKey))
    service.observed("q:" + CandidateQuery.Title("Matilda").sortKey)
    val batch = service.drain()
    batch.map(_.observations) shouldBe Some(1)
    batch.map(_.familiesResolved) shouldBe Some(1)                                        // Matilda's family, not Lalka's
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    matilda.queries should not be empty
  }

  it should "take up after a restart what its store kept, and decide as before" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(first, Helios, Seq(movie(Helios, "Lalka")))
    first.drain()
    val again = world.service()
    again.takeUp()
    decided(again.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    again.drain() shouldBe None                                                           // nothing queued, nothing to do
  }

  it should "forget, in the reads index, the questions of a film no venue lists any more" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    service.drain()
    val before = world.reads.keys
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka")))                      // Matilda gone
    service.drain()
    world.reads.keys should be < before
    world.reads.changedBy(Seq("q:" + CandidateQuery.Title("Matilda").sortKey)).queries shouldBe empty
  }

  it should "report its families as soon as it has taken up its model, before any event" in {
    val world    = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    val reported = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service  = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = reported += batch; def rebuilt(): Unit = () })
    service.takeUp()
    reported.map(_.families) shouldBe Seq(2)
  }

  it should "report how large its families are: the largest, its busiest node, and none past a region" in {
    val world    = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")), Helios -> Seq(movie(Helios, "Lalka")))
    val reported = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service  = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = reported += batch; def rebuilt(): Unit = () })
    service.takeUp()
    val sizes = reported.loneElement.sizes
    sizes.largest.map(_.listings) shouldBe Seq(2, 1)
    sizes.largest.head.busiest.map(_._2) shouldBe Seq(2)                                   // both Lalkas one node
    (sizes.largestListings, sizes.largestNodes, sizes.large) shouldBe ((2, 1, 0))
  }

  it should "drain nothing before it has taken up its model" in {
    val world   = new World
    val service = world.service()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka")))
    service.drain() shouldBe None
    service.takeUp()
    service.drain().map(_.venues) shouldBe Some(1)
  }

  // A worker's readiness waits on the take-up (the next workers roll out only once it settles), so
  // a take-up that FAILED must count as settled too, or one bad boot would hold every rollout.
  it should "say its take-up has settled once it finished, even when it failed" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val service   = new IdentityModelService(
      () => throw new IllegalStateException("store unreachable"),
      world.reads, () => Nil, normalizer, 1.hour, scheduler)
    try {
      service.takeUpSettled shouldBe false
      service.start()
      scheduler.submit((() => ()): Runnable).get(10, java.util.concurrent.TimeUnit.SECONDS)   // behind the take-up
      service.takeUpSettled shouldBe true
    } finally scheduler.shutdownNow()
  }
}
