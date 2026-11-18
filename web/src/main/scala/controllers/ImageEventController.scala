package controllers

import play.api.libs.json.{JsObject, JsValue}
import play.api.mvc._

/** `POST /uptime/img-event`: the poster-load beacon every visitor's browser sends, recorded as
 *  the uptime monitor's `img:` rows ([[ImageLoadBeacon]]). */
class ImageEventController(cc: ControllerComponents,
                           // Where a beacon's records go — see [[imgEvent]] for why it is anonymous at all.
                           imageLoads: ImageLoadBeacon) extends AbstractController(cc) {

  /** Browser-reported image load outcomes. The page's tracker batches
   *  ~10s of img onload/onerror events and POSTs them here so the uptime
   *  page sees per-host image-fetch reliability.
   *
   *  `host` is the ORIGIN CDN (image.tmdb.org, m.media-amazon.com,
   *  de.web.img3.acsta.net …), not the `images.weserv.nl` proxy that fronts
   *  most of them — the tracker unwraps the proxy's `?url=` before posting.
   *  Reporting the proxy host instead merged every CDN into one row, which is
   *  how a total m.media-amazon.com outage stayed invisible behind TMDB's
   *  healthy traffic.
   *
   *  `fallback` marks an attempt at a non-primary poster URL, and gets its own
   *  service row. The two are worth separating: a failed primary is a poster
   *  the visitor watched break, whereas a failed fallback is only a dead spare
   *  behind a poster that may have rendered fine. Merging them would let a
   *  broken spare mask — or fake — a user-visible outage.
   *
   *  `proxied` says the browser asked weserv for the image rather than the
   *  origin, and gives the PROXY a row of its own without recreating the
   *  collapsed one. It feeds that row successes only: an `<img>` error carries
   *  no status, so a proxied failure is as likely to be the origin's fault as
   *  the proxy's, and charging it to the proxy would paint the row red on every
   *  CDN outage. The one failure attributable to weserv — it broke a poster the
   *  origin was willing to serve — can only be established by re-requesting the
   *  image direct, which the tracker does client-side and posts as an ordinary
   *  event against `images.weserv.nl`.
   *
   *  ── ⚠️ THE ONE /uptime ENDPOINT THAT IS NOT ADMIN-GATED, and it has to be ──
   *
   *  Every other /uptime route is an operator READING internal
   *  state. This one is every VISITOR'S BROWSER writing the telemetry the rows
   *  above are made of — a CDN outage is only observable from the machines that
   *  tried to load the posters, which are the public's, not ours.
   *
   *  It was swept behind `adminAction` on 2026-06-12 along with the pages
   *  (20c521c4, "/uptime (+ its fallback/stream/img-event)"), which quietly did
   *  two things: the `img:` rows stopped being fed by anyone but a signed-in
   *  admin's own browser, so the feature was dead for three months; and the
   *  beacon became a permanent 401 stream that on 2026-09-04 was 100% of a
   *  25% error share on the application-health dashboard while not one request
   *  was actually failing. See web-errors.rules for the alerting that came out
   *  of that.
   *
   *  What replaced the gate as the bound on an anonymous write:
   *  [[ImageOriginRoster]] caps the number of rows a caller can create,
   *  [[ImageLoadBeacon.MaxEventsPerBeacon]] caps one request's records, and the body parser caps
   *  its bytes. What is deliberately NOT defended against is a caller
   *  fabricating plausible events for a real origin — an unauthenticated
   *  writer can make a poster CDN look worse than it is, and that is the price
   *  of measuring from the only place the measurement exists. */
  def imgEvent: Action[JsValue] = Action(parse.json(maxLength = ImageEventController.MaxBeaconBytes)) { request =>
    imageLoads.record((request.body \ "events").asOpt[Seq[JsObject]].getOrElse(Seq.empty))
    NoContent
  }
}

object ImageEventController {
  /** The body parser's cap on one beacon. */
  val MaxBeaconBytes = 64 * 1024
}
