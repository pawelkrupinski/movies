package controllers

import play.api.libs.json.JsObject
import services.UptimeMonitor

/** Browser-reported image load outcomes (`POST /uptime/img-event`) recorded as the
 *  `img:` rows of the uptime monitor — see `UptimeController.imgEvent` for what each
 *  field means and why the endpoint is anonymous.
 *
 *  [[ImageOriginRoster]] bounds the rows a caller can create and
 *  [[UptimeController.MaxEventsPerBeacon]] the records one beacon can carry. */
final class ImageLoadBeacon(monitor: UptimeMonitor) {

  private val imageOrigins = new ImageOriginRoster()

  def record(events: Seq[JsObject]): Unit =
    events.take(UptimeController.MaxEventsPerBeacon).foreach(recordOne)

  private def recordOne(e: JsObject): Unit = {
    val host     = imageOrigins.label((e \ "host").asOpt[String].getOrElse(""))
    val success  = (e \ "success").asOpt[Boolean].getOrElse(false)
    val fallback = (e \ "fallback").asOpt[Boolean].getOrElse(false)
    val proxied  = (e \ "proxied").asOpt[Boolean].getOrElse(false)
    val service  = if (fallback) s"img: $host${UptimeController.FallbackSuffix}" else s"img: $host"
    if (success) {
      monitor.recordSuccess(service)
      // Every poster weserv served, primary or spare, so the proxy's row has
      // green bars and a denominator its failures can be read against.
      if (proxied) monitor.recordSuccess(UptimeController.ProxyService)
    } else {
      val error = (e \ "error").asOpt[String].getOrElse("image load failed")
      monitor.recordFailure(service, error.take(200))
    }
  }
}
