package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Listing
import services.identity.adapters.ProjectionTick
import tools.{CutoverProperties, FixtureTestWiring}

/**
 * The recorded Poznań corpus through the identity projection (docs/design/identity-resolver.md §8):
 * the fixture pipeline runs end to end — every
 * venue's scrape into the listing intake, the resolver over the recorded TMDB answers and venue
 * details, the films written, their ratings fetched and the read model projected — and holds the
 * projection's properties on the stored films: no cannot-linked pair in one film (P3), no published
 * showtime missing (P4), and a second projection that writes nothing (P2).
 */
@CorpusReplay
class IdentityCutoverEndToEndSpec extends AnyFlatSpec with Matchers with StopsItsWirings {

  private final class CutOver extends FixtureTestWiring("08-06-2026") {
    /** Each written projection's change list, as the take-change counter is handed it. */
    val told = new java.util.concurrent.ConcurrentLinkedQueue[services.identity.TakeChange]()
    override def takeChangesTold: Seq[services.identity.TakeChange] => Unit = changes => { changes.foreach(told.add); () }
    def drainTold(): Seq[services.identity.TakeChange] = Iterator.continually(told.poll()).takeWhile(_ != null).toSeq
  }

  private lazy val booted: (CutOver, ProjectionTick) = {
    val w = stoppedAfterAll(new CutOver)
    // Settled as production's projection interval settles it: the venue pages the boot's enrichment
    // fetched are what the next projection takes in; rest is a projection that writes nothing, and
    // it is the tick every claim below reads.
    val first = w.bootCutover()
    Iterator.continually(w.projectIdentity()).take(4).find(_.atRest).getOrElse(first)
    // One of the whole corpus over the settled store: its plan is every film (a scoped projection's, only what moved).
    val tick  = w.identityProjection.tick(whole = true)
    tools.WholeReconcile(w.readModelSweeper)
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
    val settled = w.identityProjection.tick(whole = true)
    settled.plan.get.regroupings.isEmpty shouldBe true
    settled.plan.get.films.map(_.id).toSet shouldBe tick.plan.get.films.map(_.id).toSet
    // ...and the one after it, over nothing new, writes nothing at all.
    val again = w.identityProjection.tick(whole = true)
    withClue(s"${again.written} written, ${again.retired} retired, ${again.plan.map(_.regroupings)}\n") {
      again.wroteNothing shouldBe true
    }
    CutoverProperties.films(again, withIds = true) shouldBe CutoverProperties.films(settled, withIds = true)
  }

  // The take-change counter reads the projections' change lists: over the recorded corpus, from
  // the boot through the projections that settle it, each tick's change list is exactly the listings whose stored film's
  // TMDB film it moved, or that it placed on a film first — nothing missed, nothing told twice.
  it should "tell, each projection, exactly the takes it moved on the site" in {
    val w = stoppedAfterAll(new CutOver)
    // a listing's state on the site: on no stored film (None), or on one of this TMDB film (or none)
    var before = Map.empty[ListingKey, Option[Int]]   // the boot's projection starts from no stored film
    var told   = 0
    (Iterator.single(() => w.bootCutover()) ++ Iterator.continually(() => w.projectIdentity())).take(5).zipWithIndex.foreach { case (project, n) =>
      w.drainTold()
      project()
      val (after, clusterOf) = services.identity.ProjectionPlacements.of(w.identityProjection, w.filmIdCounterStore)
      // the boot runs more than one projection (the model's batches ask for them): what each told is chained per listing
      val changes = w.drainTold()
      val chained = changes.groupBy(_.listing).map { case (listing, cs) =>
        listing -> cs.foldLeft(before.get(listing)) { (state, c) =>
          val from = if (c.moved) Some(c.before) else None
          withClue(s"projection ${n + 1}, ${ListingKey.serialised(listing)}: $c told from $state: ")(from shouldBe state)
          if (!c.moved) withClue(s"$c: ")(c.joined shouldBe clusterOf.getOrElse(listing, Set.empty).exists(m => m != listing && before.contains(m)))
          Some(c.after)
        }
      }
      after.foreach { case (listing, now) =>
        withClue(s"projection ${n + 1}, ${ListingKey.serialised(listing)}: ")(chained.getOrElse(listing, before.get(listing)) shouldBe Some(now))
      }
      before = after; told += changes.size
    }
    told should be > 0   // the boot decided every listing
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

  /** A cut-over worker projects every five minutes, and on most ticks nothing moved: since the cut-over
   *  the workers allocated 4-7x what they had (US ~25 GB a tick), every venue slot rebuilt each tick
   *  (2.4 GB of a US draft) and the listings read whole each time — each found only by reading a
   *  production log. A quiet tick over the settled fixture corpus is held to its allocation on the
   *  projecting thread (the median of several, so the periodic whole reads and reconciles fall to
   *  either side), and to building no venue slot and writing nothing. */
  "A quiet projection of the settled corpus" should "stay within its allocation budget and rebuild no venue slot" in {
    val (w, _) = booted
    val ticks  = Vector.newBuilder[ProjectionTick]
    val quiet  = tools.costs.AllocationMeter.median(warmups = 3, runs = 7)(ticks += w.identityProjection.tick())
    val whole  = tools.costs.AllocationMeter.median(warmups = 1, runs = 3)(ticks += w.identityProjection.tick(whole = true))
    val all    = ticks.result()
    info(tools.costs.PerformanceBudgets.QuietProjectionTick.render(quiet))
    info(tools.costs.PerformanceBudgets.WholeProjectionTick.render(whole))
    all.foreach(t => withClue(s"${t.phases.map(_.render).mkString(", ")}: ")(t.refused shouldBe None))
    all.map(_.written).sum shouldBe 0
    tools.costs.PerformanceBudgets.QuietProjectionSlotsBuilt.check(all.map(_.slotsBuilt.toLong).sum, s"over ${all.size} ticks")
    tools.costs.PerformanceBudgets.QuietProjectionTick.check(quiet)
    tools.costs.PerformanceBudgets.WholeProjectionTick.check(whole)
  }
}
