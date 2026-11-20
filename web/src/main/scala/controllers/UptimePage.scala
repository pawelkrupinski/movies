package controllers

import java.time.Instant

import play.twirl.api.Html
import services.fallback.FallbackStore

/** The /uptime page ([[UptimeController]]) around its triaged sections: the cinemas currently served by
 *  the Filmweb fallback and the venues every source dropped, rendered below the triage, for ONE
 *  deployment's country. */
final class UptimePage(filmwebFallback: FallbackStore, closures: services.closure.ClosureLedgerReader,
                       country: models.Country) {

  /** The page over `sections`, its times labelled by `stamp`. */
  def render(sections: UptimeSections, stamp: Instant => String): Html =
    // The Filmweb fallbacks render as a section ON the page, below the failing / no-screenings triage.
    views.html.uptime(sections.failing, sections.gone, sections.zero, FallbackRow.activeIn(filmwebFallback, stamp),
      sections.cinemasByCity, sections.services, sections.other, sections.hiddenHealthyCinemas, thin = sections.thin,
      hiddenThin = sections.hiddenThin, dropped = DroppedRow.allIn(closures, stamp), current = country)
}
