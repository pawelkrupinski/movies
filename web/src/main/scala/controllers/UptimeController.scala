package controllers

import play.api.mvc._
import services.UptimeHistory
import services.fallback.FallbackStore

/** `GET /uptime`: the operator's page of every service's health — each scraper's and source's bars,
 *  triaged into its sections ([[UptimeTriage]]), the cinemas currently served by the Filmweb
 *  fallback and the venues every source dropped. Its live feed is [[UptimeStreamController]]'s;
 *  the poster-load beacon feeding its `img:` rows, [[ImageEventController]]'s. */
class UptimeController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeHistory,
                       // The bars, labelled in the deployment's country's time.
                       bars: UptimeBars, filmwebFallback: FallbackStore,
                       closures: services.closure.ClosureLedgerReader, country: models.Country) extends AbstractController(cc) {

  // Which section each service lands in, for this deployment's roster.
  private val triage = new UptimeTriage(country)

  def index: Action[AnyContent] = adminAction {
    val sections = triage.groupRows(monitor.services, monitor.recentStatuses(_, UptimeTriage.RecentScrapes),
      monitor.recentErrors(_, UptimeTriage.RecentScrapes), bars.rowBuilder())
    // The Filmweb fallbacks render as a section ON the page, below the failing / no-screenings triage.
    Ok(views.html.uptime(sections.failing, sections.gone, sections.zero, FallbackRow.activeIn(filmwebFallback, bars.stamp), sections.cinemasByCity,
      sections.services, sections.other, sections.hiddenHealthyCinemas, thin = sections.thin,
      hiddenThin = sections.hiddenThin, dropped = DroppedRow.allIn(closures, bars.stamp), current = country))
  }
}
