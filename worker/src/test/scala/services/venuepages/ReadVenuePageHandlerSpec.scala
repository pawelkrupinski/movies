package services.venuepages

import models.KinoApollo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.FakeDetailEnricher
import services.cinemas.common.FilmDetail
import services.events.{RecordingEventBus, VenueDetailRead}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.{EnrichDetailsTasks, HandlerOutcome, Task, TaskType}

import java.time.{Clock, Instant, ZoneOffset}

/** A cut-over listing's venue page, read by page into venue_pages before any film row holds it. */
class ReadVenuePageHandlerSpec extends AnyFlatSpec with Matchers {

  private val clock  = Clock.fixed(Instant.parse("2026-10-01T10:00:00Z"), ZoneOffset.UTC)
  private val Page   = "https://kinoapollo.pl/film/dune"
  private val Detail = FilmDetail(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155), releaseYear = Some(2021))

  private def taskFor(enricher: FakeDetailEnricher) =
    Task("id", TaskType.ReadVenuePage, EnrichDetailsTasks.pageDedupKey(enricher.detailGroup, Page), ReadVenuePageTasks.payload(enricher, Page), 1)

  "ReadVenuePageHandler" should "read the page into venue_pages, stamp it, announce it and record it on /uptime" in {
    val enricher  = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(Detail))
    val store     = new InMemoryVenuePageStore
    val freshness = new InMemoryFreshnessStore
    val bus       = new RecordingEventBus
    val uptime    = new UptimeMonitor()
    val handler   = new ReadVenuePageHandler(Map("kino-apollo" -> enricher), new VenuePageReader(store, freshness, e => bus.publish(e), clock), uptime, freshness, clock)

    handler.handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    store.get(VenuePageKey("kino-apollo", Page)).map(_.outcome) shouldBe Some(VenuePage.Read(Detail))
    freshness.isFresh(EnrichDetailsTasks.pageRead("kino-apollo", Page), FreshnessKind.DetailEnrich, clock.instant()) shouldBe true
    bus.published should contain (VenueDetailRead("kino-apollo", Page))
    uptime.history(UptimeMonitor.enrichmentService(KinoApollo.displayName)).map(_.successes).sum shouldBe 1
  }

  it should "leave a page that failed for now unread, and record the failure" in {
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None)
    val store    = new InMemoryVenuePageStore
    val uptime   = new UptimeMonitor()
    new ReadVenuePageHandler(Map("kino-apollo" -> enricher), new VenuePageReader(store, new InMemoryFreshnessStore, _ => (), clock), uptime, new InMemoryFreshnessStore, clock)
      .handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    store.get(VenuePageKey("kino-apollo", Page)) shouldBe None
    uptime.history(UptimeMonitor.enrichmentService(KinoApollo.displayName)).map(_.failures).sum shouldBe 1
  }

  it should "stamp the page as tried, whatever the read said — a display-only venue's listing waits for no more" in {
    val enricher  = new FakeDetailEnricher(KinoApollo, "kino-apollo", None)
    val freshness = new InMemoryFreshnessStore
    new ReadVenuePageHandler(Map("kino-apollo" -> enricher), new VenuePageReader(new InMemoryVenuePageStore, freshness, _ => (), clock),
      new UptimeMonitor(), freshness, clock).handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    freshness.lastFetchedAt(EnrichDetailsTasks.pageAttempted("kino-apollo", Page)) shouldBe Some(clock.instant())
  }
}
