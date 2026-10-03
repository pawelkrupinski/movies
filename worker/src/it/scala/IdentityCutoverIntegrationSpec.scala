package integration

import models.{Cinema, CinemaMovie, Country}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.identity.{Listing, ProjectionTick}
import services.movies.TitleNormalizer
import tools._

import scala.collection.mutable
import scala.util.{Random, Try}

/**
 * The identity projection over the HARD CLUSTERS (docs/design/identity-resolver.md §8, §10):
 * every country's hard-cluster corpus booted through the real pipeline — real Mongo, the listing intake, the resolver over the recorded
 * answers, the projection's writes — asserting on what was STORED:
 *
 *  - P1 ORDER INDEPENDENCE: two seeded arrival orders store the same films, ids included; an
 *    arrival that first publishes half of every venue's listing, projects, then the rest, stores the
 *    same partition and films (its ids follow its history, by design);
 *  - P2 FIXPOINT: a projection over the projection's own output writes nothing;
 *  - P3: no stored film holds two listings the resolution cannot-linked;
 *  - P4: every published showtime is on the film holding its listing.
 *
 * Each pass runs in its own database (`ConvergenceStorage.mongo`), dropped in `afterAll`.
 */
class IdentityCutoverIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val OrderSeed = 0x2026_09_26L
  private val storages  = mutable.ListBuffer.empty[ConvergenceStorage]

  override def afterAll(): Unit = { storages.synchronized(storages.toList).foreach(s => Try(s.close())); super.afterAll() }

  private val countries: Seq[Country] = Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)))
  private lazy val responses: Map[Country, RecordedResponses] =
    countries.map(c => c -> RecordedResponses.replaying(RecordedResponses.pathFor(c.code))).toMap

  private def storage(country: Country, label: String): ConvergenceStorage = {
    val s = ConvergenceStorage.mongo(mongoTarget, s"cut-${country.code}-$label", TitleNormalizer.forCountry(country),
      services.movies.MovieChangeStream.Debounce.forCountry(country))
    storages.synchronized(storages += s)
    s
  }

  /** A wiring over `store`. */
  private def wiring(country: Country, store: ConvergenceStorage): ArchiveReplayWiring = {
    val w = FetchReplayWiring(country, store, CorpusFixture.read(HardClusters.corpusKey(country)), responses(country))
    // What production's `start()` does before any tick: the cache holds the stored films.
    w.movieCache.rehydrate()
    w
  }

  private def arrivals(w: ArchiveReplayWiring, rnd: Random): Seq[CinemaScraper] =
    rnd.shuffle(w.archivedListings.toSeq.sortBy(_._1.displayName)).map { case (cinema, films) =>
      PreScrapedCinemaScraper.replaying(cinema, rnd.shuffle(films.toList))
    }

  private def publish(w: ArchiveReplayWiring, scrapers: Seq[CinemaScraper]): Unit =
    scrapers.foreach(s => Try(w.cinemaScrapeRunner.run(s)))

  private def published(w: ArchiveReplayWiring): Seq[(Cinema, Seq[CinemaMovie])] =
    w.identityListingIntake.listings(w.cinemaScrapers.map(_.cinema))
  private def listings(w: ArchiveReplayWiring): Seq[Listing] =
    published(w).flatMap { case (c, fs) => fs.map(Listing.of(c, _, w.titleNormalizer)) }

  private def same(a: Set[String], b: Set[String], what: String): Unit =
    withClue(s"$what — only in the first:\n  ${(a -- b).toSeq.sorted.mkString("\n  ")}\nonly in the second:\n  ${(b -- a).toSeq.sorted.mkString("\n  ")}\n")(a shouldBe b)

  private final case class Pass(w: ArchiveReplayWiring, tick: ProjectionTick)

  /** The venues the corpus lists films at. */
  private def venuesOf(w: ArchiveReplayWiring): Set[Cinema] = w.archivedListings.collect { case (c, fs) if fs.nonEmpty => c }.toSet

  /** PREMISE of every claim below: `publish` carries on past a venue whose scrape throws, as
   *  production's scheduler does — so a pass where venues never landed would hold every relative
   *  claim (same films, nothing lost) over less corpus, or none. Every listed venue published, and
   *  the projection made films. */
  private def landedWhole(p: Pass): Unit =
    withClue("premise — every venue the corpus lists published, and the projection made films: ") {
      published(p.w).map(_._1).toSet shouldBe venuesOf(p.w)
      p.tick.plan.get.films should not be empty
    }

  private def cutPass(country: Country, label: String, seed: Long, halfFirst: Boolean): Pass = {
    val w = wiring(country, storage(country, label))
    val scrapers = arrivals(w, new Random(seed))
    if (halfFirst) {
      publish(w, scrapers.map(s => PreScrapedCinemaScraper.replaying(s.cinema, w.archivedListings(s.cinema).take(
        math.max(1, w.archivedListings(s.cinema).size / 2)).toList)))
      w.projectIdentity()
    }
    publish(w, scrapers)
    Pass(w, settled(w))
  }

  /** Projections until one writes nothing — the rest production's projection interval reaches as the
   *  venue pages a projection's enrichment fetched are taken in by the next — and that projection. */
  private def settled(w: ArchiveReplayWiring): ProjectionTick = {
    var tick = w.projectIdentity()
    var n    = 1
    while (!tick.wroteNothing && n < 5) { tick = w.projectIdentity(); n += 1 }
    tick
  }

  /** EVERY boot this spec asserts on — each country's three passes, each in its own database and
   *  wiring, sharing nothing but the read-only corpus and the (concurrent) recorded answers — run up
   *  front on a small pool rather than one after another inside the tests; the tests below only assert. */
  // Held as a Try: a lazy val whose initialiser throws is re-run on the next access, so one failed
  // boot re-ran every boot for every test after it instead of failing each at once.
  private lazy val bootAttempt: Try[Map[Country, Seq[Pass]]] = Try {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(BootParallelism)
    try {
      def run[A](body: => A): java.util.concurrent.Future[A] = pool.submit(() => body)
      val passes = countries.map { c =>
        c -> Seq(run(cutPass(c, "p0", OrderSeed, halfFirst = false)), run(cutPass(c, "p1", OrderSeed + 1, halfFirst = false)),
          run(cutPass(c, "half", OrderSeed + 2, halfFirst = true)))
      }
      passes.map { case (c, fs) => c -> fs.map(_.get()) }.toMap
    } finally pool.shutdownNow(): Unit
  }
  private def passes: Map[Country, Seq[Pass]] = bootAttempt.get

  /** A CI runner's four vCPUs; `itAll` runs other suites beside this one, so no more. */
  private val BootParallelism = 4

  countries.foreach { country =>
    val cc = country.code

    s"$cc's projection" should "store the same films whatever the arrival order (P1)" in {
      val Seq(p0, p1, half) = passes(country)
      info(s"[$cc] ${p0.tick.listings} listings → ${p0.tick.plan.get.films.size} films " +
        s"(${p0.tick.plan.get.films.count(_.record.tmdbId.isDefined)} matched)")
      p0.tick.refused shouldBe None
      passes(country).foreach(landedWhole)
      same(CutoverProperties.films(p0.tick, withIds = true), CutoverProperties.films(p1.tick, withIds = true), "two orders")
      same(CutoverProperties.films(p0.tick, withIds = false), CutoverProperties.films(half.tick, withIds = false), "half first")
    }

    it should "write nothing on a projection over its own output (P2)" in {
      passes(country).foreach { p =>
        val settled = p.w.identityProjection.tick()
        settled.plan.get.regroupings.isEmpty shouldBe true
        val again = p.w.identityProjection.tick()
        withClue(s"${again.written} written, ${again.retired} retired\n")(again.wroteNothing shouldBe true)
      }
    }

    it should "hold no cannot-linked pair in one film (P3) and lose no published showtime (P4)" in {
      passes(country).foreach { p =>
        CutoverProperties.cannotLinked(p.tick, listings(p.w)) shouldBe empty
        CutoverProperties.lostShowtimes(published(p.w), p.tick, p.w.movieRepository.findAll()) shouldBe empty
      }
    }
  }
}
