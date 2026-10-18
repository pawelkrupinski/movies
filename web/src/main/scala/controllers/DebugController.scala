package controllers

import services.movies.LatestTitleYear

import models._
import play.api.mvc._
import play.api.Mode
import services.movies.TitleNormalizer
import services.readmodel.WebReadModel

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{DurationInt, DurationLong}

/**
 * The dev-only `/debug*` pages: the corpus table, its per-row detail, the
 * the read-model dump, the rating cadence, the per-row re-enrich
 * button and the card / film tuning pages — plus `rehydrate`, the one endpoint
 * here that runs in prod (admin-gated).
 *
 * ⚠️ THE ONLY CONTROLLER IN THE WEB TIER THAT READS THE SOURCE CORPUS. Every
 * page here pulls `movies` / the task queue from Mongo on
 * demand and blocks on it (`Await`) — full-collection scans over the local→prod
 * tunnel, ~6 s apiece. That is fine for an operator's tab and would not be for a
 * public route, which is why none of this lives beside the listing handlers:
 * `MovieController` renders from the projected read model alone and never
 * learns a `MovieRepository` exists.
 */
class DebugController(cc: ControllerComponents,
                      // Every collaborator the /debug pages read (corpus,
                      // queue, cadence, read-model dump), keyed by country. In
                      // prod a single stack — this deployment's country; locally
                      // in Dev one per switchable country, so /debug can switch
                      // which country's db it shows via `?country=xx` same-origin
                      // instead of hopping to the other country's prod host
                      // (which 404s /debug).
                      debugCountries: DebugCountries,
                      // What `rehydrate` reloads.
                      readModel: WebReadModel,
                      // Gate for the state-mutating /…/debug/rehydrate trigger
                      // (the other /debug pages are dev-only; rehydrate runs in
                      // every mode, so it needs the admin gate instead).
                      adminAction: AdminAction,
                      environment: Mode,
                      // `cinema displayName -> public source-page URL`, the same
                      // links /uptime shows, sourced from the UptimeMonitor tag
                      // snapshot. Evaluated per request so it tracks live retags;
                      // used by the /debug table to link cinema names.
                      cinemaSourceUrls: () => Map[String, String] = () => Map.empty,
                      // The ONE country this deployment serves — which city slugs
                      // the tuning pages resolve. Injected
                      // rather than read from the process configuration at each use so a
                      // spec can exercise another country's host without mutating
                      // the process-global env that parallel suites share.
                      servingCountry: models.Country,
                      // What "now" is for the pages' own age readings (the mirror badge, cadence).
                      clock: java.time.Clock,
                      // The serving country's title rules — the wiring's one instance.
                      normalizer: TitleNormalizer,
                     )(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) {
  // The latest year a title may name, read off this controller's clock: what the row details cap a title's year by.
  private val latestYear = LatestTitleYear(clock)


  // English on the `showtimes.cc` apex, the deployment's language elsewhere — see `ServedCity.resolve`.
  private val frontDoorMessages: play.api.i18n.Messages = FrontDoorMessages(cc.messagesApi)
  private def withCity(slug: String, request: RequestHeader)(f: City => Result): Result =
    ServedCity.resolve(slug, request, servingCountry, messages, frontDoorMessages)(f)

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
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(listing)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** The debug navbar's age badge: how far behind the mirror was when the data on
   *  screen was READ (the sync's health — a snapshot's own age doesn't make the
   *  mirror look broken), plus how long ago that read was once it is old enough to
   *  matter, e.g. a snapshot restored after a restart. `None` in prod, where the
   *  pages read the source and there is no copy to be behind. */
  private def mirrorAge(snapshot: DebugSnapshot[?]): Option[services.MirrorFreshness.Age] = {
    val now = clock.instant()
    services.MirrorFreshness.describe(snapshot.mirrorNewest, snapshot.takenAt.getOrElse(now)).map { age =>
      val sinceRead = snapshot.takenAt.map(at => java.time.Duration.between(at, now).toMillis.millis)
      age.copy(snapshotAge = sinceRead.filter(_ >= DebugController.SnapshotAgeShownAfter))
    }
  }

  /** Dev-only: the per-(rating source, film) adaptive refresh cadence. Films are
   *  grouped by their current refresh interval, slowest (most backed-off / stable)
   *  first, with the last two displayed-value changes shown on hover. Reads the
   *  worker-written `rating_cadence` collection + names the films from the same
   *  corpus-listing snapshot `/debug` renders. */
  def cadence(): Action[AnyContent] = Action { request =>
    devOnly {
      val country = debugCountries.resolve(request)
      val stack   = debugCountries.stackFor(country)
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val recordsFuture = Future(stack.ratingCadence())
      val listingFuture = Future(stack.listing())
      val (records, listing) = Await.result(recordsFuture.zip(listingFuture), 70.seconds)
      implicit val c: City = City.all.head   // only for the shared debug navbar's city link
      Ok(views.html.cadence(services.cadence.CadenceReport.build(records.value, listing.value.titleByTmdb.get), clock.instant(),
        // The OLDER of the two snapshots' stamps: the page is as behind as its stalest half.
        current = country, sameOrigin = debugCountries.switchable,
        mirror = mirrorAge(Seq(records, listing).minBy(_.mirrorNewest.getOrElse(java.time.Instant.MAX)))))
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
      stack.movieRepository.findById(services.movies.FilmId(id)) match {
        case Some(row) =>
          // The per-source enrichment log, joined on the tmdbId-keyed rating key.
          // Two bounded `_id in [...]` lookups (4 keys each), not the readers'
          // full-collection reads — this runs per row-expand. Issued CONCURRENTLY:
          // the reads are independent, and against a remote Mongo the round-trip
          // is the whole cost (see `buildFrom`).
          scala.util.Try(services.attempts.FilmAttemptReport.buildFrom(
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

  /** Dev-only: dump the read cache the web actually serves from — `web_movies` plus
   *  per-film `web_screenings` counts — so you can see exactly what a request would
   *  resolve against (vs `/debug`, which shows the source `movies` corpus). A row's
   *  screenings come from [[debugReadModelScreenings]] on expand: inlining every
   *  showtime of the US read model (~100k screening docs) OOM'd the dev server. */
  def debugReadModel(): Action[AnyContent] = Action { request =>
    devOnly {
      implicit val c: City = City.all.head
      val country = debugCountries.resolve(request)
      val dump    = debugCountries.stackFor(country).readModel()
      Ok(views.html.debugReadModel(dump.value,
        current = country, sameOrigin = debugCountries.switchable, mirror = mirrorAge(dump)))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }

  /** Dev-only: ONE read-model film's screening docs, every field of every showtime —
   *  fetched lazily by the `/debug/readmodel` table when a row is expanded. */
  def debugReadModelScreenings(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      debugCountries.stackFor(debugCountries.resolve(request)).readModelScreeningsFor(id) match {
        case Some(screenings) => Ok(views.html.debugReadModelScreenings(id, screenings))
        // Not an empty list: that reads as "this film has no screenings".
        case None             => ServiceUnavailable(s"could not read the screenings of $id")
      }
    }
  }

  /** Dev-only visual-tuning page. Renders the real `_movieCard` partial(s)
   *  inside a `.tune-scope` wrapper plus a slider panel that drives the CSS
   *  custom properties the production card styles read. Self-contained: the
   *  sample films are built in-process so the page works regardless of cache
   *  state. */
  def tune(city: String): Action[AnyContent] = Action { request =>
    // The dev gate first: in production the page does not exist for ANY city, so
    // the slug is not worth looking up (nor an "unknown city" answer worth giving).
    devOnly {
      withCity(city, request) { implicit c =>
        Ok(views.html.tune(TuneSamples.films))
      }
    }
  }

  /** Dev-only tuning page for the film-detail view — live sliders over the real
   *  `_filmDetailContent` for the title / meta / Seanse typography. */
  def tuneFilm(city: String): Action[AnyContent] = Action { request =>
    devOnly {
      withCity(city, request) { implicit c =>
        Ok(views.html.tuneFilm(TuneSamples.film))
      }
    }
  }

  /** Reload the in-memory read-model caches from Mongo. Available in every mode
   * (unlike the rest of the debug endpoints, which are dev-only) so a production
   * instance whose caches drifted from the derived collections can be reconciled
   * without a redeploy — but since it runs in prod and mutates state, it's gated
   * by [[AdminAction]] (login session + ADMIN_ALLOWLIST) rather than left open. */
  def rehydrate(city: String): Action[AnyContent] = adminAction { request =>
    withCity(city, request) { _ =>
      val count = readModel.reload()
      Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
    }
  }
}

object DebugController {

  /** A snapshot younger than this is what the warm-up keeps them at (~1 min) and is
   *  not called out; an older one — restored after a restart — says how old it is. */
  private[controllers] val SnapshotAgeShownAfter: scala.concurrent.duration.FiniteDuration = 2.minutes
}
