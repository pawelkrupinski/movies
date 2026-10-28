package controllers

import models._
import play.api.mvc._
import play.api.Mode
import services.readmodel.WebReadModel

/**
 * The dev-only card / film tuning pages and read-model dump, plus `rehydrate`, the
 * one endpoint here that runs in prod (admin-gated).
 */
class DebugController(cc: ControllerComponents,
                      // What `rehydrate` reloads.
                      readModel: WebReadModel,
                      // Gate for the state-mutating /…/debug/rehydrate trigger
                      // (the tuning pages are dev-only; rehydrate runs in
                      // every mode, so it needs the admin gate instead).
                      adminAction: AdminAction,
                      environment: Mode,
                      // The ONE country this deployment serves — which city slugs
                      // the tuning pages resolve. Injected
                      // rather than read from the process configuration at each use so a
                      // spec can exercise another country's host without mutating
                      // the process-global env that parallel suites share.
                      servingCountry: models.Country,
                      // What `/debug/readmodel` lists, per country.
                      readModels: DebugReadModels,
                     )(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) {

  // English on the `showtimes.cc` apex, the deployment's language elsewhere — see `ServedCity.resolve`.
  private val frontDoorMessages: play.api.i18n.Messages = FrontDoorMessages(cc.messagesApi)
  private def withCity(slug: String, request: RequestHeader)(f: City => Result): Result =
    ServedCity.resolve(slug, request, servingCountry, messages, frontDoorMessages)(f)

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

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

  /** Dev-only: the read model (`web_movies` plus per-film `web_screenings` counts) of
   *  `?country=` (the serving country by default) — the projection's output, so a film in the
   *  corpus but missing here is a projection gap, not a scrape gap. A row's screenings come
   *  from [[debugReadModelScreenings]] on expand: inlining every showtime of the US read
   *  model (~100k screening docs) OOM'd the dev server. */
  def debugReadModel(): Action[AnyContent] = Action { request =>
    devOnly {
      val country   = readModels.resolve(request)
      val readModel = readModels(country)
      ReadModelDump.read(readModel.reader) match {
        case Some(dump) => Ok(views.html.debugReadModel(dump, country, readModel.source, readModels.countries))
        // Not a smaller listing: that reads as films missing from the read model.
        case None       => ServiceUnavailable(s"could not read the ${country.code} read model whole (${readModel.source})")
      }
    }
  }

  /** Dev-only: ONE read-model film's screening docs, every field of every showtime —
   *  fetched lazily by the `/debug/readmodel` table when a row is expanded. */
  def debugReadModelScreenings(id: String): Action[AnyContent] = Action { request =>
    devOnly {
      readModels(readModels.resolve(request)).reader.findCard(id) match {
        case Some(card) => Ok(views.html.debugReadModelScreenings(id, card.screenings))
        // Not an empty list: that reads as "this film has no screenings".
        case None       => ServiceUnavailable(s"could not read the screenings of $id")
      }
    }
  }

  /** Reload the in-memory read-model caches from Mongo. Available in every mode
   * (unlike the tuning pages, which are dev-only) so a production
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

