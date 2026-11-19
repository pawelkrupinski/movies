package services.venuepages

import models.{EnrichmentUptimeService, KinoApollo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.FakeDetailEnricher
import services.cinemas.common.FilmDetail
import services.events.{RecordingEventBus, VenueDetailRead}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.{HandlerOutcome, Task}
import services.tasktypes.TaskType

import scala.concurrent.duration.DurationInt

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
    val uptime    = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    val handler   = handlerOver(Map("kino-apollo" -> enricher), store, freshness, e => bus.publish(e), uptime)

    handler.handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    store.get(VenuePageKey("kino-apollo", Page)).map(_.outcome) shouldBe Some(VenuePage.Read(Detail))
    freshness.isFresh(EnrichDetailsTasks.pageRead("kino-apollo", Page), FreshnessKind.DetailEnrich, clock.instant()) shouldBe true
    bus.published should contain (VenueDetailRead("kino-apollo", Page))
    uptime.history(EnrichmentUptimeService.forCinema(KinoApollo.displayName)).map(_.successes).sum shouldBe 1
  }

  it should "leave a page that failed for now unread, and record the failure" in {
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None)
    val store    = new InMemoryVenuePageStore
    val uptime   = new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)
    handlerOver(Map("kino-apollo" -> enricher), store, new InMemoryFreshnessStore, _ => (), uptime)
      .handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    store.get(VenuePageKey("kino-apollo", Page)) shouldBe None
    uptime.history(EnrichmentUptimeService.forCinema(KinoApollo.displayName)).map(_.failures).sum shouldBe 1
  }

  it should "ask again for a page venue_pages did not take, neither stamping it tried nor announcing it" in {
    val enricher  = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(Detail))
    val freshness = new InMemoryFreshnessStore
    val bus       = new RecordingEventBus
    val handler   = handlerOver(Map("kino-apollo" -> enricher), new _root_.tools.UnacknowledgedVenuePageWrites(new InMemoryVenuePageStore), freshness, e => bus.publish(e), new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned))

    handler.handle(taskFor(enricher)) shouldBe a [HandlerOutcome.Reschedule]
    freshness.lastFetchedAt(EnrichDetailsTasks.pageAttempted("kino-apollo", Page)) shouldBe None
    bus.published shouldBe empty

    handler.handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    bus.published shouldBe Seq(VenueDetailRead("kino-apollo", Page))
  }

  it should "stamp the page as tried, whatever the read said — a display-only venue's listing waits for no more" in {
    val enricher  = new FakeDetailEnricher(KinoApollo, "kino-apollo", None)
    val freshness = new InMemoryFreshnessStore
    handlerOver(Map("kino-apollo" -> enricher), new InMemoryVenuePageStore, freshness, _ => (), new UptimeMonitor(clock = _root_.tools.SpecClock.Pinned)).handle(taskFor(enricher)) shouldBe HandlerOutcome.Done
    freshness.lastFetchedAt(EnrichDetailsTasks.pageAttempted("kino-apollo", Page)) shouldBe Some(clock.instant())
  }

  /** The handler as the wiring builds it: its page reads into `store`, stamped in `freshness`, announced to `announce`
   *  and recorded on `uptime`. */
  private def handlerOver(enrichers: Map[String, services.cinemas.common.DetailEnricher], store: VenuePageStore,
                          freshness: services.freshness.FreshnessStore, announce: VenueDetailRead => Unit,
                          uptime: services.UptimeRecorder): ReadVenuePageHandler = {
    val schedule = new DetailSchedule(new services.tasks.InMemoryTaskQueue, freshness, new services.tasks.DueWindow(6.hours))
    new ReadVenuePageHandler(enrichers, new RecordedPageReader(new VenuePageReader(store, schedule, announce, clock), uptime), schedule, clock)
  }
}
