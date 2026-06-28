package controllers

import play.api.Mode
import play.api.mvc.Result
import play.api.mvc.Results.NotFound

/**
 * Dev-only gating shared by the controllers. The navbar Debug tab and every
 * `/debug` endpoint are hidden in production so the cache contents aren't
 * exposed on a deployed instance. `Mode` defaults to Dev in `AppLoader` unless
 * `APP_MODE=prod` is set explicitly.
 */
object DevMode {
  /** The flag the navbar uses to gate the Debug tab. */
  def enabled(environment: Mode): Boolean = environment != Mode.Prod

  /** `result`, except in production where every dev endpoint 404s. */
  def gate(environment: Mode)(result: => Result): Result =
    if (environment == Mode.Prod) NotFound("dev-only endpoint") else result
}
