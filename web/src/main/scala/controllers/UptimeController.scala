package controllers

import play.api.mvc.*
import services.UptimeHistory

/** `GET /uptime`: the operator's page of every service's health — each scraper's and source's bars,
 *  triaged into its sections ([[UptimeTriage]]) and rendered with the page's side lists
 *  ([[UptimePage]]). Its live feed is [[UptimeStreamController]]'s; the poster-load beacon feeding
 *  its `img:` rows, [[ImageEventController]]'s. */
class UptimeController(cc: ControllerComponents, adminAction: AdminAction, monitor: UptimeHistory,
                       // The bars, labelled in the deployment's country's time.
                       bars: UptimeBars,
                       // Which section each service lands in, for this deployment's roster.
                       triage: UptimeTriage, page: UptimePage) extends AbstractController(cc) {

  def index: Action[AnyContent] = adminAction {
    Ok(page.render(triage.groupRows(monitor.services, monitor.recentStatuses(_, UptimeTriage.RecentScrapes),
      monitor.recentErrors(_, UptimeTriage.RecentScrapes), bars.rowBuilder()), bars.stamp))
  }
}
