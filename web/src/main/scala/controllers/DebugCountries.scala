package controllers

import models.{CityScreening, Country, ResolvedMovie}
import play.api.mvc.{Cookie, RequestHeader}
import services.attempts.EnrichmentAttemptReader
import services.cadence.RatingCadenceReader
import services.movies.MovieRepository
import services.staging.StagingRepository
import services.tasks.TaskQueue

import java.time.Instant

/**
 * Everything a `/debug` page reads, bound to ONE country's Mongo database — the
 * corpus (`movies`), the staging table (`pending_movies`), the task queue, the
 * rating-cadence collection, and the read-model dump.
 *
 * For the read-model dump the boot country reads the WARM in-memory
 * [[services.readmodel.WebReadModel]] the app actually serves from (so
 * `/debug/readmodel` shows exactly what a request would resolve against); a
 * switched-to country has no warm model, so those functions read that country's
 * `web_movies` / `web_screenings` straight from Mongo and report `now` for the
 * mtime.
 */
final class DebugStack(
  val country:               Country,
  val movieRepository:       MovieRepository,
  val stagingRepository:     StagingRepository,
  val taskQueue:             TaskQueue,
  val ratingCadenceReader:   RatingCadenceReader,
  val attemptReader:         EnrichmentAttemptReader,
  val readModelMovies:       () => Seq[ResolvedMovie],
  val readModelScreenings:   () => Seq[CityScreening],
  val readModelLastModified: () => Instant,
)

/**
 * Which country's data the `/debug` pages read, per request.
 *
 * In prod (and every controller test) there is a single stack — the
 * deployment's own country — and the switch is off: a `?country=` param is
 * ignored. Locally in Dev the wiring builds one stack per switchable country so
 * the debug pages can hop between countries' corpora SAME-ORIGIN via
 * `?country=xx`, instead of the navbar navigating to the other country's
 * production deployment (which serves prod-mode and 404s every `/debug` route).
 *
 * Resolution order in Dev: the `?country=` query param wins (and is stamped into
 * a sticky cookie so the plain `<a>` tab links keep the selection), then the
 * cookie, then the boot country. An unknown or not-wired code falls back to the
 * boot country.
 */
final class DebugCountries private (
  bootCountry: Country,
  stacks:      Map[Country, DebugStack],
  devMode:     Boolean,
) {
  /** Whether the Dev-only same-origin switch is live: Dev mode AND more than one
   *  country wired. The debug navbar renders the `?country=` switcher only then;
   *  otherwise it keeps the prod cross-deployment links. */
  val switchable: Boolean = devMode && stacks.sizeIs > 1

  /** The country this request's `/debug` view should read. Always the boot
   *  country when the switch is off. */
  def resolve(request: RequestHeader): Country =
    if (!switchable) bootCountry
    else queryCountry(request).orElse(cookieCountry(request)).getOrElse(bootCountry)

  def stackFor(country: Country): DebugStack = stacks.getOrElse(country, stacks(bootCountry))

  /** When the request carried an explicit `?country=`, the cookie to persist that
   *  selection across the plain tab links; `None` otherwise (nothing to stamp). */
  def selectionCookie(request: RequestHeader): Option[Cookie] =
    if (!switchable) None
    else queryCountry(request).map(c => Cookie(DebugCountries.CookieName, c.code, httpOnly = false))

  private def queryCountry(request: RequestHeader): Option[Country] =
    request.getQueryString("country").flatMap(Country.byCode).filter(stacks.contains)

  private def cookieCountry(request: RequestHeader): Option[Country] =
    request.cookies.get(DebugCountries.CookieName).flatMap(c => Country.byCode(c.value)).filter(stacks.contains)
}

object DebugCountries {
  /** Client-readable cookie (not httpOnly) that persists the switched country
   *  across the debug navbar's plain tab links + the SSE stream request. */
  val CookieName = "debugCountry"

  /** The single-country holder — prod and every controller test. One stack, no
   *  switching: a `?country=` param is ignored. */
  def single(bootStack: DebugStack): DebugCountries =
    new DebugCountries(bootStack.country, Map(bootStack.country -> bootStack), devMode = false)

  /** The boot stack plus any Dev-only extra per-country stacks. `devMode` gates
   *  the switch; when false only the boot stack is ever selected. */
  def of(bootStack: DebugStack, extras: Map[Country, DebugStack], devMode: Boolean): DebugCountries =
    new DebugCountries(bootStack.country, extras + (bootStack.country -> bootStack), devMode)
}
