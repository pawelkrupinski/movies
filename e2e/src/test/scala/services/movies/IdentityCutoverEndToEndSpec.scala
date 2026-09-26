package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Listing, ProjectionTick}
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeArchiveRepository}
import settings.ProcessConfiguration
import tools.{CutoverProperties, Env, FixtureTestWiring}

/**
 * The recorded Poznań corpus through the identity projection (docs/design/identity-resolver.md §8,
 * phase 5) with Poland cut over: the fixture pipeline runs end to end on the new path — every
 * venue's scrape into the listing intake, the resolver over the recorded TMDB answers and venue
 * details, the films written, their ratings fetched and the read model projected — and holds the
 * projection's properties on the stored films: no cannot-linked pair in one film (P3), no published
 * showtime missing (P4), and a second projection that writes nothing (P2).
 *
 * The switch OFF is `FilmScheduleEndToEndSpec`'s job: its snapshots are byte-identical with the
 * projection code present and not wired.
 */
class IdentityCutoverEndToEndSpec extends AnyFlatSpec with Matchers {

  private final class CutOver(env: Env) extends FixtureTestWiring("08-06-2026") {
    override lazy val configuration: ProcessConfiguration = new ProcessConfiguration(env)
    // The fixture wiring's Mongo is disabled: in-memory archives keep what the scrapes publish.
    override lazy val scrapeArchive: ScrapeArchiveRepository    = new InMemoryScrapeArchiveRepository
    override lazy val acceptedListings: ScrapeArchiveRepository = new InMemoryScrapeArchiveRepository
  }

  private lazy val booted: (CutOver, ProjectionTick) = {
    val w = new CutOver(Env.of("KINOWO_IDENTITY_CUTOVER" -> Country.Poland.code))
    val tick = w.bootCutover()
    w.readModelProjector.reconcile()
    w.webReadModel.reload()
    (w, tick)
  }
  private def wiring = booted._1
  private def published = wiring.identityListingIntake.get.listings(wiring.cinemaScrapers.map(_.cinema))

  "The switch" should "be off unless the country is named" in {
    val off = new CutOver(Env.of())
    off.identityCutover shouldBe false
    off.identityProjection shouldBe None
    off.identityListingIntake shouldBe None
    new CutOver(Env.of("KINOWO_IDENTITY_CUTOVER" -> "es,de")).identityProjection shouldBe None
  }

  "A cut-over Poland" should "run the fixture pipeline end to end through the projection" in {
    val (w, tick) = booted
    tick.refused shouldBe None
    val plan = tick.plan.get
    val films = w.movieRepository.findAll()
    info(s"${tick.listings} listings → ${plan.films.size} films (${plan.films.count(_.record.tmdbId.isDefined)} matched); " +
      s"${w.webReadModel.allMovies().size} served; regroupings ${plan.regroupings}")
    films.map(_.id).toSet shouldBe plan.films.map(_.id).toSet
    films.map(_.key(w.titleNormalizer)).distinct.size shouldBe films.size
    films.flatMap(_.record.tmdbId).distinct.size shouldBe films.count(_.record.tmdbId.isDefined)
    films.forall(_.record.readyToProject) shouldBe true
    w.webReadModel.allMovies() should not be empty
    films.count(_.record.imdbRating.isDefined) should be > 0
  }

  it should "hold no cannot-linked pair in one film (P3) and lose no published showtime (P4)" in {
    val (w, tick) = booted
    val listings = published.flatMap { case (c, fs) => fs.map(Listing.of(c, _, w.titleNormalizer)) }
    CutoverProperties.cannotLinked(tick, listings) shouldBe empty
    CutoverProperties.lostShowtimes(published, tick, w.movieRepository.findAll()) shouldBe empty
    tick.resolution.get.violations shouldBe 0
  }

  it should "write nothing on a projection over its own output (P2)" in {
    val (w, tick) = booted
    // The first projection's films were enriched since (IMDb ids, ratings, an IMDb year that can move
    // a yearless film's key): the next projection takes that in without regrouping anything...
    val settled = w.identityProjection.get.tick()
    settled.plan.get.regroupings.isEmpty shouldBe true
    settled.plan.get.canary.getOrElse(services.identity.ShadowRelation.Identical, 0) shouldBe tick.plan.get.films.size
    settled.plan.get.films.map(_.id).toSet shouldBe tick.plan.get.films.map(_.id).toSet
    // ...and the one after it, over nothing new, writes nothing at all.
    val again = w.identityProjection.get.tick()
    withClue(s"${again.written} written, ${again.retired} retired, ${again.plan.map(_.regroupings)}\n") {
      again.wroteNothing shouldBe true
    }
    CutoverProperties.films(again, withIds = true) shouldBe CutoverProperties.films(settled, withIds = true)
  }
}
