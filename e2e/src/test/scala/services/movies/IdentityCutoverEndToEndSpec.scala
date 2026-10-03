package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Listing, ProjectionTick}
import tools.{CutoverProperties, FixtureTestWiring}

/**
 * The recorded Poznań corpus through the identity projection (docs/design/identity-resolver.md §8):
 * the fixture pipeline runs end to end — every
 * venue's scrape into the listing intake, the resolver over the recorded TMDB answers and venue
 * details, the films written, their ratings fetched and the read model projected — and holds the
 * projection's properties on the stored films: no cannot-linked pair in one film (P3), no published
 * showtime missing (P4), and a second projection that writes nothing (P2).
 */
class IdentityCutoverEndToEndSpec extends AnyFlatSpec with Matchers {

  private final class CutOver extends FixtureTestWiring("08-06-2026")

  private lazy val booted: (CutOver, ProjectionTick) = {
    val w = new CutOver
    // Settled as production's projection interval settles it: the venue pages the boot's enrichment
    // fetched are what the next projection takes in; rest is a projection that writes nothing, and
    // it is the tick every claim below reads.
    val first = w.bootCutover()
    val tick  = Iterator.continually(w.projectIdentity()).take(4).find(_.wroteNothing).getOrElse(first)
    tools.WholeReconcile(w.readModelProjector)
    w.webReadModel.reload()
    (w, tick)
  }
  private def wiring = booted._1
  private def published = wiring.identityListingIntake.listings(wiring.cinemaScrapers.map(_.cinema))

  "Poland's identity projection" should "run the fixture pipeline end to end through the projection" in {
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
    val settled = w.identityProjection.tick()
    settled.plan.get.regroupings.isEmpty shouldBe true
    settled.plan.get.canary.getOrElse(services.identity.ShadowRelation.Identical, 0) shouldBe tick.plan.get.films.size
    settled.plan.get.films.map(_.id).toSet shouldBe tick.plan.get.films.map(_.id).toSet
    // ...and the one after it, over nothing new, writes nothing at all.
    val again = w.identityProjection.tick()
    withClue(s"${again.written} written, ${again.retired} retired, ${again.plan.map(_.regroupings)}\n") {
      again.wroteNothing shouldBe true
    }
    CutoverProperties.films(again, withIds = true) shouldBe CutoverProperties.films(settled, withIds = true)
  }

  /** Found by `Identity model convergence` (run 36717160191): a day after a cut-over boot, every
   *  country re-asked every film's ratings round after round, each task `Skipped` as fresh. The
   *  enqueuer judged due on the wiring's clock; the rating handlers judged — and stamped — on the
   *  wall clock they defaulted to. Each stamp must be the wiring's own time. */
  "A cut-over Poland's rating refreshes" should "be stamped on the wiring's clock, the one the enqueuer reads" in {
    val (w, _) = booted
    val stamps = for {
      film   <- w.movieRepository.findAll()
      tmdbId <- film.record.tmdbId.toSeq
      kind   <- Seq(services.freshness.FreshnessKind.ImdbRating, services.freshness.FreshnessKind.RtRating,
                    services.freshness.FreshnessKind.McRating)
      at     <- w.freshnessStore.lastFetchedAt(services.attempts.RatingKeys.tmdbKey(kind, tmdbId)).toSeq
    } yield at
    stamps should not be empty
    stamps.distinct shouldBe Seq(w.clock.instant())
  }

  /** A cut-over model reads venue pages from the pipeline's own detail enrichment and waits for it
   *  (`VenuePageIndex`), so a cut-over tick must run that enrichment, as production's detail reaper
   *  does: Identity model convergence (run 36756016590) had Poland's sample at 50% matched against
   *  production's 71% with every page an unanswered gap. */
  "A cut-over Poland's model" should "have its listings' venue pages answered by the pipeline's detail enrichment" in {
    val (w, _) = booted
    val listings = published.flatMap { case (c, fs) => fs.map(Listing.of(c, _, w.titleNormalizer)) }
    val enricherOf = w.detailEnrichers.map(e => e.cinema -> e).toMap
    val paged = listings.flatMap(l => l.page.flatMap(p => enricherOf.get(l.cinema).map(_ -> p)))
    paged should not be empty
    // Every page the enrichment can read (found, or found gone) is answered; one whose fetch fails is
    // left unstamped for the reaper's next tick, as production leaves it, and stays a gap. So does a
    // page on a film naming one enricher group several pages: the detail reaper asks one page per
    // group and film (its dedup key), and a chain's one network slot cannot say which page its facts
    // are (`VenuePageIndex`). And a page no stored slot names at all — the venue's same-title fold
    // kept another listing's slot for that title — is no page the enrichment can reach.
    val slotted: Set[String] = w.movieCache.entries.flatMap { case (_, r) =>
      r.data.values.flatMap(services.cinemas.common.DetailEnricher.nativeRefOf) }.toSet
    val sharedPages: Set[String] = w.movieCache.entries.flatMap { case (_, r) =>
      w.detailEnrichers.groupBy(_.detailGroup).values.flatMap { group =>
        val pages = r.data.toSeq.collect { case (src, sd) if models.Source.cinemaOf(src).exists(c => group.exists(_.cinema == c)) =>
          services.cinemas.common.DetailEnricher.nativeRefOf(sd) }.flatten.distinct
        if (pages.sizeIs > 1) pages else Nil
      }
    }.toSet
    val readable   = paged.distinct.filter { case (e, page) =>
      slotted(page) && !sharedPages(page) && e.fetchDetail(page) != services.cinemas.common.DetailFetchOutcome.Failed }
    val unanswered = readable.filterNot { case (e, page) => w.venuePageIndex.answer(e, page).isDefined }
    info(s"${readable.size} readable venue pages; ${sharedPages.size} pages sharing a film's enricher group left unanswered")
    readable should not be empty
    withClue(s"${unanswered.size} of ${readable.size} readable venue pages unanswered, e.g. ${unanswered.take(5).map(_._2).mkString(", ")}: ") {
      unanswered shouldBe empty
    }
  }
}
