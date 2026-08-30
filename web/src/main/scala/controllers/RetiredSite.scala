package controllers

import play.api.http.Status

/**
 * The address arithmetic for a RETIRED deployment — a host that still answers,
 * but whose content lives somewhere else now (`kinowo.fly.dev` after the move
 * to `kinowo.net`).
 *
 * Kept apart from [[RetiredSiteController]] because it is the half with no Play
 * machinery in it: given where the live site is and what was asked for, where
 * does the request go and with which status. That is the part worth pinning in
 * a test, and the part a reader has to be able to check by eye.
 */
object RetiredSite {

  /** The same resource on the live site, query string intact.
   *
   *  `origin` is the live deployment's SCHEME + HOST with no mount prefix
   *  ([[models.Country.webOrigin]], not `webUrl`) — `path` is the raw incoming
   *  path, which already carries whatever prefix the request arrived on, so
   *  appending it to a base that also carried the prefix would double it. */
  def destination(origin: String, path: String, rawQueryString: String): String =
    origin + path + (if (rawQueryString.isEmpty) "" else "?" + rawQueryString)

  /** 301 for a read, 308 for everything else.
   *
   *  The distinction is not cosmetic. A 301 on a POST/PUT/DELETE lets the client
   *  downgrade it to a GET and drop the body — the behaviour browsers settled on
   *  for 301/302 decades ago, and the reason `/api/me/state` (a PUT) or
   *  `/auth/token` (a POST) would silently stop working against the new host.
   *  308 is the same permanence with the method and body preserved. */
  def redirectStatus(method: String): Int =
    if (method == "GET" || method == "HEAD") Status.MOVED_PERMANENTLY
    else                                     Status.PERMANENT_REDIRECT
}
