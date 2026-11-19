package controllers

import models._
import play.api.mvc._
import play.api.Mode

/**
 * The dev-only corpus table of `/debug` and its rows' detail. Its siblings answer elsewhere: the rating
 * cadence ([[CadenceController]]), the read-model dump
 * ([[DebugReadModelController]]), the tuning pages ([[TuneController]]) and
 * `rehydrate` ([[RehydrateController]]), the one /debug endpoint that runs in prod.
 *
 * ⚠️ WITH THE CADENCE PAGE, THE ONLY CONTROLLERS IN THE WEB TIER THAT READ THE
 * SOURCE CORPUS. Every page here pulls `movies` / the task queue from Mongo on
 * demand and blocks on it (`Await`) — full-collection scans over the local→prod
 * tunnel, ~6 s apiece. That is fine for an operator's tab and would not be for a
 * public route, which is why none of this lives beside the listing handlers:
 * `MovieController` renders from the projected read model alone and never
 * learns a `MovieRepository` exists.
 */
class DebugController(cc: ControllerComponents,
                      // Every collaborator the /debug pages read (corpus,
                      // queue, cadence), keyed by country. In
                      // prod a single stack — this deployment's country; locally
                      // in Dev one per switchable country, so /debug can switch
                      // which country's db it shows via `?country=xx` same-origin
                      // instead of hopping to the other country's prod host
                      // (which 404s /debug).
                      debugCountries: DebugCountries,
                      environment: Mode,
                      // `cinema displayName -> public source-page URL`, the same
                      // links /uptime shows, sourced from the UptimeMonitor tag
                      // snapshot. Evaluated per request so it tracks live retags;
                      // used by a row's detail to link cinema names.
                      cinemaSourceUrls: () => Map[String, String] = () => Map.empty,
                      // What "now" is for the pages' own age readings (the mirror badge).
                      clock: java.time.Clock,
                     ) extends AbstractController(cc) {
  // The latest year a title may name, read off this controller's clock: what the row details cap a title's year by.
  private val latestYear = services.movies.LatestTitleYear(clock)

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  def debug(): Action[AnyContent] = Action { request =>
    devOnly {
      // The debug table is the global corpus; the only thing the view needs a
      // city for is the /movie fallback link on a row with no live showtimes
      // anywhere — give it the default city for that edge case.
      implicit val c: City = City.all.head
      // Which country's corpus to show (the boot country unless a Dev-only
      // ?country= switch selected another). `stack` binds every debug read below
      // to that country's Mongo db.
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      // The corpus listing is a SNAPSHOT (see `RefreshingSnapshot`): re-reading
      // `movies` + every `movie_slots` row per load cost 4–10 s a country switch
      // even off the local mirror. `findAllForListing` drops each row's per-cinema
      // `showtimes` server-side — the table renders only metadata + counts; the
      // showtimes are fetched per-row on expand via `/debug/details`.
      val listing = stack.listing()
      Ok(views.html.debug(
        listing.value.table,
        // The SELECTED country's rules, not the deployment's: /debug can switch
        // countries, and a row's display title must read as its own corpus keyed it.
        stack.movieRepository.normalizer,
        current = country, sameOrigin = debugCountries.switchable, mirror = listing.mirrorAge(clock.instant())))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: the heavy per-source breakdown for ONE corpus row, fetched lazily
   *  by the /debug table when a row is expanded. Rendering every row's breakdown
   *  inline (each iterates `Cinema.all` × day × showtime) built one giant `Html`
   *  string that OOM'd the view on the full corpus; serving them per-row on
   *  demand keeps the initial /debug render to the light data rows only. The `id`
   *  is the row's Mongo `_id` (its `FilmId`), the same value the table rows are
   *  keyed on. */
  def debugDetails(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val stack = debugCountries.stackFor(debugCountries.resolve(request))
      stack.movieRepository.findById(models.FilmId(id)) match {
        case Some(row) =>
          // The per-source enrichment log, joined on the tmdbId-keyed rating key.
          // Two bounded `_id in [...]` lookups (4 keys each), not the readers'
          // full-collection reads — this runs per row-expand. Issued CONCURRENTLY:
          // the reads are independent, and against a remote Mongo the round-trip
          // is the whole cost (see `buildFrom`).
          scala.util.Try(controllers.FilmAttemptReport.buildFrom(
            row.record.tmdbId, stack.attemptReader, stack.ratingCadenceReader)) match {
            case scala.util.Success(statuses) =>
              Ok(views.html.debugDetails(row.title, row.year, row.record,
                stack.movieRepository.normalizer, cinemaSourceUrls(), statuses, latestYear, clock.instant()))
            // Not an empty report: that reads as "never attempted".
            case scala.util.Failure(e) =>
              ServiceUnavailable(s"could not read the enrichment log for this row: ${e.getMessage}")
          }
        case None      => NotFound("no such row")
      }
    }
  }
}
