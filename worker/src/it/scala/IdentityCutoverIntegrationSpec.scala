package integration

import models.{Cinema, CinemaMovie, Country}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.identity.{FilmIdCounters, Listing, ProjectionTick}
import services.movies.TitleNormalizer
import tools._

import scala.collection.mutable
import scala.util.{Random, Try}

/**
 * PHASE 5 over the HARD CLUSTERS (docs/design/identity-resolver.md §8, §10 "Phase 3 (cutover)"):
 * every country's hard-cluster corpus booted through the real pipeline with that country CUT OVER
 * to the identity projection — real Mongo, the listing intake, the resolver over the recorded
 * answers, the projection's writes — asserting on what was STORED:
 *
 *  - P1 ORDER INDEPENDENCE: two seeded arrival orders store the same films, ids included; an
 *    arrival that first publishes half of every venue's listing, projects, then the rest, stores the
 *    same partition and films (its ids follow its history, by design);
 *  - P2 FIXPOINT: a projection over the projection's own output writes nothing;
 *  - P3: no stored film holds two listings the resolution cannot-linked;
 *  - P4: every published showtime is on the film holding its listing;
 *  - IDS: switching a country OVER from the old path, every film keeps the id `IdAssigner` gives it
 *    over the old path's films, and no showtime is lost;
 *  - ROLLBACK: switching it BACK, the old path re-lands every listing onto the projection's rows
 *    and serves every published showtime.
 *
 * Each pass runs in its own database (`ConvergenceStorage.mongo`), dropped in `afterAll`.
 */
class IdentityCutoverIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val OrderSeed = 0x2026_09_26L
  private val storages  = mutable.ListBuffer.empty[ConvergenceStorage]

  override def afterAll(): Unit = { storages.foreach(s => Try(s.close())); super.afterAll() }

  private val countries: Seq[Country] = Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)))
  private lazy val responses: Map[Country, RecordedResponses] =
    countries.map(c => c -> RecordedResponses.replaying(RecordedResponses.pathFor(c.code))).toMap

  private def storage(country: Country, label: String): ConvergenceStorage = {
    val s = ConvergenceStorage.mongo(mongoTarget, s"cut-${country.code}-$label", TitleNormalizer.forCountry(country))
    storages += s
    s
  }

  /** A wiring over `store`, cut over or on the old path. */
  private def wiring(country: Country, store: ConvergenceStorage, cutOver: Boolean): ArchiveReplayWiring = {
    val w = FetchReplayWiring(country, store, CorpusFixture.read(HardClusters.corpusKey(country)), responses(country),
      environment = if (cutOver) Env.of("KINOWO_IDENTITY_CUTOVER" -> country.code) else Env.of())
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

  /** The old path's boot, as the hard-cluster convergence spec drives it. */
  private def bootOldPath(w: ArchiveReplayWiring, scrapers: Seq[CinemaScraper]): Unit = {
    scrapers.foreach { s => Try(w.cinemaScrapeRunner.run(s)); w.advanceStagingOnce() }
    w.enrichDetailsSync(); w.drainServices(); w.drainStaging()
    w.movieService.settle(); w.drainStaging(); w.concludeEnrichment(); w.movieService.settle()
  }

  private def published(w: ArchiveReplayWiring): Seq[(Cinema, Seq[CinemaMovie])] =
    w.identityListingIntake.get.listings(w.cinemaScrapers.map(_.cinema))
  private def listings(w: ArchiveReplayWiring): Seq[Listing] =
    published(w).flatMap { case (c, fs) => fs.map(Listing.of(c, _, w.titleNormalizer)) }

  private def same(a: Set[String], b: Set[String], what: String): Unit =
    withClue(s"$what — only in the first:\n  ${(a -- b).toSeq.sorted.mkString("\n  ")}\nonly in the second:\n  ${(b -- a).toSeq.sorted.mkString("\n  ")}\n")(a shouldBe b)

  private final case class Pass(w: ArchiveReplayWiring, tick: ProjectionTick)

  private def cutPass(country: Country, label: String, seed: Long, halfFirst: Boolean): Pass = {
    val w = wiring(country, storage(country, label), cutOver = true)
    val scrapers = arrivals(w, new Random(seed))
    if (halfFirst) {
      publish(w, scrapers.map(s => PreScrapedCinemaScraper.replaying(s.cinema, w.archivedListings(s.cinema).take(
        math.max(1, w.archivedListings(s.cinema).size / 2)).toList)))
      w.projectIdentity()
    }
    publish(w, scrapers)
    Pass(w, w.projectIdentity())
  }

  private lazy val passes: Map[Country, Seq[Pass]] = countries.map { c =>
    c -> Seq(cutPass(c, "p0", OrderSeed, halfFirst = false), cutPass(c, "p1", OrderSeed + 1, halfFirst = false),
      cutPass(c, "half", OrderSeed + 2, halfFirst = true))
  }.toMap

  countries.foreach { country =>
    val cc = country.code

    s"A cut-over $cc" should "store the same films whatever the arrival order (P1)" in {
      val Seq(p0, p1, half) = passes(country)
      info(s"[$cc] ${p0.tick.listings} listings → ${p0.tick.plan.get.films.size} films " +
        s"(${p0.tick.plan.get.films.count(_.record.tmdbId.isDefined)} matched)")
      p0.tick.refused shouldBe None
      same(CutoverProperties.films(p0.tick, withIds = true), CutoverProperties.films(p1.tick, withIds = true), "two orders")
      same(CutoverProperties.films(p0.tick, withIds = false), CutoverProperties.films(half.tick, withIds = false), "half first")
    }

    it should "write nothing on a projection over its own output (P2)" in {
      passes(country).foreach { p =>
        val settled = p.w.identityProjection.get.tick()
        settled.plan.get.regroupings.isEmpty shouldBe true
        val again = p.w.identityProjection.get.tick()
        withClue(s"${again.written} written, ${again.retired} retired\n")(again.wroteNothing shouldBe true)
      }
    }

    it should "hold no cannot-linked pair in one film (P3) and lose no published showtime (P4)" in {
      passes(country).foreach { p =>
        CutoverProperties.cannotLinked(p.tick, listings(p.w)) shouldBe empty
        CutoverProperties.lostShowtimes(published(p.w), p.tick, p.w.movieRepository.findAll()) shouldBe empty
      }
    }

    it should "keep, switching OVER from the old path, the ids IdAssigner gives the old films, and every showtime" in {
      val store = storage(country, "over")
      val old   = wiring(country, store, cutOver = false)
      bootOldPath(old, arrivals(old, new Random(OrderSeed)))
      val before = old.movieRepository.findAll()
      val cut    = wiring(country, store, cutOver = true)
      val tick   = cut.projectIdentity()
      tick.refused shouldBe None
      val after = cut.movieRepository.findAll()
      val plan  = tick.plan.get
      info(s"[$cc] old path ${before.size} films → projection ${after.size}: kept ${plan.films.count(f => before.exists(_.id == f.id))} ids, " +
        s"${plan.regroupings}; canary ${plan.canary.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}")
      CutoverProperties.misassigned(before, listings(cut), tick, after, FilmIdCounters.empty, cut.titleNormalizer) shouldBe empty
      CutoverProperties.lostShowtimes(published(cut), tick, after) shouldBe empty
      cut.filmIdCounterStore.allChecked()._1.map(_.filmId).toSet should contain allElementsOf after.map(_.id.value)
    }

    it should "leave, switching BACK, rows the old path re-lands onto, serving every published showtime" in {
      val p = passes(country).head
      val store = storage(country, "back")
      val cut   = wiring(country, store, cutOver = true)
      publish(cut, arrivals(cut, new Random(OrderSeed)))
      val projected = cut.projectIdentity().plan.get.films
      val shown     = published(cut)
      val old       = wiring(country, store, cutOver = false)
      bootOldPath(old, arrivals(old, new Random(OrderSeed + 7)))
      val after = old.movieRepository.findAll()
      info(s"[$cc] projection ${projected.size} films → old path ${after.size}; " +
        s"${projected.count(f => after.exists(_.id == f.id))} projected ids still stored")
      CutoverProperties.unservedShowtimes(shown, after) shouldBe empty
      after.map(_.key(old.titleNormalizer)).distinct.size shouldBe after.size
      p.tick.refused shouldBe None
    }
  }
}
