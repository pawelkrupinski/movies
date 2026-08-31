package controllers

import play.api.mvc.Result

/**
 * Cache headers for a page that was rendered FOR SOMEBODY.
 *
 * A signed-out page is byte-identical for every visitor and says so:
 * `MovieController.conditionalGzipped` gives it `Last-Modified` and
 * `private, no-cache`, so a browser keeps it and re-validates cheaply. A page
 * carrying somebody's avatar, name and hidden films took the other branch, which
 * until now set NO cache headers at all — and a response with no headers is
 * heuristically cacheable, so the browser was free to show a signed-in page back
 * to somebody who had just signed out. Which it did.
 *
 * `no-store` rather than `no-cache`, because `no-cache` still permits STORING:
 * it forces a revalidation on a normal navigation (which would have returned the
 * signed-out page correctly) but the back/forward cache ignores it entirely, and
 * "log out, press Back, you are logged in again" is the same bug wearing a hat.
 * `no-store` is the only thing that opts a page out of bfcache.
 *
 * The cost is that signed-in visitors re-fetch pages instead of re-using them.
 * That is the right way round: this site is overwhelmingly anonymous, so the
 * caching that matters is on the pages this does not touch, and for the handful
 * of people with accounts a correct page beats a cheap one.
 */
object PersonalisedPage {

  /** What a page rendered for a signed-in visitor must never be: reusable. */
  val CacheControl = "private, no-store"

  /** `result`, marked un-reusable when it was rendered for somebody.
   *
   *  Anonymous renders are returned untouched, so they keep whatever validators
   *  their caller set and stay as cacheable as they were. */
  def apply(user: Option[models.User])(result: Result): Result =
    if (user.isEmpty) result else result.withHeaders("Cache-Control" -> CacheControl)
}
