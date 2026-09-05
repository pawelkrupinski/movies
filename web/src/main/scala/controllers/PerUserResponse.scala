package controllers

import play.api.mvc.Result

/**
 * Cache headers for a response that was produced FOR SOMEBODY.
 *
 * Nothing renders a person into an HTML page any more: the listing, film and
 * browse views are byte-identical for every visitor, and `shared.js` asks
 * `/api/me` who is looking (see `_authMenu`). That moved the whole of this
 * site's per-user surface into a handful of JSON endpoints and auth redirects —
 * which makes THEM the thing that must never be re-used, and makes the header
 * below load-bearing rather than belt-and-braces. A cached `/api/me` is a
 * signed-in avatar rebuilt on a page whose owner signed out, or worse, rebuilt
 * for whoever asks next.
 *
 * `no-store` rather than `no-cache`, because `no-cache` still permits STORING:
 * it forces revalidation on a normal navigation, but the back/forward cache
 * ignores it entirely, and "log out, press Back, you are logged in again" is the
 * same bug wearing a hat. `no-store` is the only thing that opts a response out
 * of bfcache. `private` on top of it so no shared cache treats the response as
 * fair game even if it ignores the rest.
 */
object PerUserResponse {

  /** What a response produced for one person must never be: reusable. */
  val CacheControl = "private, no-store"

  /** `result`, marked un-reusable because it was produced for somebody. */
  def apply(result: Result): Result =
    result.withHeaders("Cache-Control" -> CacheControl)
}
