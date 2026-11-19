package services.venuepages

import services.UptimeRecorder
import services.cinemas.common.DetailEnricher
import services.events.EventBus
import services.freshness.FreshnessStore
import services.movies.{MovieCache, ScreeningTokens}
import services.tasks.{DueWindow, InMemoryTaskQueue}

import java.time.Clock
import java.util.Locale

/** The `EnrichDetails` handler as the composition root (`DetailWiring`) builds it, for the specs: its stores, bus,
 *  uptime monitor and due window in, the handler out. The schedule's queue is one of its own: the handler never
 *  enqueues. */
object EnrichDetailsHandlerOf {
  def apply(enrichersByGroup: Map[String, DetailEnricher], cache: MovieCache, freshness: FreshnessStore,
            uptime: UptimeRecorder, bus: EventBus, dueWindow: DueWindow, clock: Clock, enrichmentLanguage: Locale,
            screeningTokens: ScreeningTokens = ScreeningTokens.forDefaultCountry(),
            pages: VenuePageStore = new InMemoryVenuePageStore): EnrichDetailsHandler = {
    val schedule = new DetailSchedule(new InMemoryTaskQueue, freshness, dueWindow)
    new EnrichDetailsHandler(enrichersByGroup, schedule,
      new RecordedPageReader(new VenuePageReader(pages, schedule, event => bus.publish(event), clock), uptime),
      cache, new LandedPages(pages, enrichmentLanguage), screeningTokens, clock)
  }
}
