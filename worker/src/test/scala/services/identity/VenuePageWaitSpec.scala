package services.identity

import models.{CinemaMovie, KinoApollo, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FakeDetailEnricher
import services.cinemas.common.FilmDetail
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.tasks.{EnrichDetailsTasks, InMemoryTaskQueue}
import services.venuepages.{InMemoryVenuePageStore, VenuePage, VenuePageKey}

import java.time.Instant
import scala.concurrent.duration._

/** How long a cut-over country's new listing waits for its venue page: a page that carries its identity
 *  (a deferring venue) until it is read or gone; a display-only page only until it has been tried once —
 *  as staging, which tolerates a display-only page's failure. */
class VenuePageWaitSpec extends AnyFlatSpec with Matchers {

  private val Page = "http://ref"
  private val now  = Instant.parse("2026-10-01T10:00:00Z")
  private val listing = Listing.of(KinoApollo, CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = Some(Page),
    synopsis = None, cast = Seq.empty, director = Seq.empty, showtimes = Nil), titleNormalizer)

  private final class World(defers: Boolean) {
    val enricher  = new FakeDetailEnricher(KinoApollo, "kino-apollo", defersTmdb = defers)
    val pages     = new InMemoryVenuePageStore
    val index     = new VenuePageIndex(pages)
    val freshness = new InMemoryFreshnessStore
    val pageWait  = new VenuePageWait(Seq(enricher), index, new InMemoryTaskQueue, freshness, 1.hour, _root_.tools.SpecClock.Pinned)
    def attempted(): Unit = freshness.markFresh(EnrichDetailsTasks.pageAttempted("kino-apollo", Page), FreshnessKind.DetailEnrich, now)
    def read(): Unit = { pages.put(VenuePage(VenuePageKey("kino-apollo", Page), VenuePage.Read(FilmDetail()), now)); index.pageRead("kino-apollo", Page); index.settle() }
  }

  "A listing at a venue whose page carries its identity" should "wait until the page is read, however often a read failed" in {
    val world = new World(defers = true)
    world.pageWait.awaiting(listing) shouldBe true
    world.attempted()
    world.pageWait.awaiting(listing) shouldBe true
    world.read()
    world.pageWait.awaiting(listing) shouldBe false
  }

  "A listing at a display-only venue" should "wait only until its page has been tried once" in {
    val world = new World(defers = false)
    world.pageWait.awaiting(listing) shouldBe true
    world.attempted()
    world.pageWait.awaiting(listing) shouldBe false
  }
}
