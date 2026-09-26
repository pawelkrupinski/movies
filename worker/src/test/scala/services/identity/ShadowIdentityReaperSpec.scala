package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer, StoredMovieRecord}
import tools.{MutableClock, TestWiring}

import scala.concurrent.duration._

/** The shadow run: resolves the listing set, diffs it against the pipeline's films, persists the
 *  run and exports the gauges — and never fails the settle it rides. */
class ShadowIdentityReaperSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.default

  private def listing(venue: Cinema): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, "Lalka", Some(2026), Seq("Maciej Kawalski")), "Lalka", "Lalka", "Lalka",
      Some(2026), Seq("Maciej Kawalski"), Some(150), None, None)
  private val listings = Seq(Multikino, Helios).map(listing)

  /** A film database of one film, the one the listings credit. */
  private object OneFilm extends IdentityLookups {
    private val hit = Hit(1321666, "Lalka", None, Some(2026), 5.0)
    def hasDetail(l: Listing): Boolean                          = false
    def detail(l: Listing): Answer[Option[DetailFacts]]         = Answer.Known(None)
    def candidates(q: CandidateQuery): Answer[Seq[Hit]]         = Answer.Known(q match {
      case CandidateQuery.Title(t) if t.equalsIgnoreCase("Lalka") => Seq(hit)
      case CandidateQuery.Director("Maciej Kawalski")              => Seq(hit)
      case _                                                       => Nil
    })
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(Option.when(id == 1321666)(
      IdentityMeasures.Film("Lalka", None, Nil, Some(2026), Some(150), Some(Seq("Maciej Kawalski")), None, Some(5.0))))
  }

  /** Today's pipeline: one film holding both listings' slots. */
  private val pipeline = StoredMovieRecord.synthesised("Lalka", Some(2026), MovieRecord(tmdbId = Some(1321666), data = Map[Source, SourceData](
    CinemaShowing.keyFor(Multikino, "Lalka", normalizer) -> SourceData(title = Some("Lalka"), releaseYear = Some(2026)),
    CinemaShowing.keyFor(Helios, "Lalka", normalizer)    -> SourceData(title = Some("Lalka"), releaseYear = Some(2026)))), normalizer)

  private final class Recorded extends ShadowIdentityMetrics {
    var films: Map[ShadowRelation, Int] = Map.empty
    var crossings: Option[Int]          = None
    def resolved(counts: Map[ShadowRelation, Int], resolveSeconds: Double): Unit = films = counts
    def crossings(count: Int): Unit                                              = crossings = Some(count)
  }

  private def reaper(runs: ShadowRunStore, metrics: ShadowIdentityMetrics, pins: PinStore = new InMemoryPinStore,
                     lookups: () => IdentityLookups = () => OneFilm, clock: MutableClock = new MutableClock(TestWiring.FixedInstant)) =
    new ShadowIdentityReaper(() => listings, () => Seq(pipeline), () => (lookups(), new ObservationGaps), pins, normalizer, calibration,
      runs, ShadowRetention(8.days), metrics, clock)

  "a shadow tick" should "persist the resolve's decisions and its diff against the pipeline, and export the gauges" in {
    val runs    = ShadowRunStore.inMemory(new MutableClock(TestWiring.FixedInstant))
    val metrics = new Recorded
    val tick    = reaper(runs, metrics).tick()

    val run = runs.latestRun().get
    run shouldBe tick.run.get
    run.at shouldBe TestWiring.FixedInstant
    run.clusters.map(c => (c.decision.listings, c.decision.film, c.relation)) shouldBe
      Seq((listings.map(_.key).toSet, Some(1321666), Some(ShadowRelation.Identical)))
    run.families shouldBe empty
    runs.verdicts().map(_.correct) shouldBe Seq(true)
    metrics.films shouldBe Map(ShadowRelation.Identical -> 1, ShadowRelation.Split -> 0, ShadowRelation.Merged -> 0, ShadowRelation.Moved -> 0)
    metrics.crossings shouldBe Some(0)
    tick.listings shouldBe 2
  }

  it should "resolve under the curation pins" in {
    val pins = new InMemoryPinStore
    pins.insert(Pin(listings.map(_.key), PinClaim.NeverFilm(1321666), "admin", "not this film", TestWiring.FixedInstant))
    val runs = ShadowRunStore.inMemory(new MutableClock(TestWiring.FixedInstant))
    reaper(runs, new Recorded, pins).tick()
    runs.latestRun().get.clusters.flatMap(_.decision.film) shouldBe empty
    runs.latestRun().get.clusters.flatMap(_.relation).toSet shouldBe Set(ShadowRelation.Moved)
  }

  "a failed tick" should "never fail its caller, and leave the previous run the latest" in {
    val clock = new MutableClock(TestWiring.FixedInstant)
    val runs  = ShadowRunStore.inMemory(clock)
    reaper(runs, new Recorded, clock = clock).tick()
    val first = runs.latestRun()
    clock.advance(java.time.Duration.ofMinutes(30))
    noException should be thrownBy reaper(runs, new Recorded, lookups = () => throw new IllegalStateException("store down"), clock = clock).tickQuietly()
    runs.latestRun() shouldBe first
  }
}
