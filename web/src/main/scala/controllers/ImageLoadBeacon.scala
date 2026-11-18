package controllers

import play.api.libs.json.JsObject
import services.UptimeRecorder

/** Browser-reported image load outcomes (`POST /uptime/img-event`) recorded as the
 *  `img:` rows of the uptime monitor — see `ImageEventController.imgEvent` for what each
 *  field means and why the endpoint is anonymous.
 *
 *  [[ImageOriginRoster]] bounds the rows a caller can create and
 *  [[ImageLoadBeacon.MaxEventsPerBeacon]] the records one beacon can carry. */
final class ImageLoadBeacon(monitor: UptimeRecorder) {

  private val imageOrigins = new ImageOriginRoster()

  def record(events: Seq[JsObject]): Unit =
    events.take(ImageLoadBeacon.MaxEventsPerBeacon).foreach(recordOne)

  private def recordOne(e: JsObject): Unit = {
    val host     = imageOrigins.label((e \ "host").asOpt[String].getOrElse(""))
    val success  = (e \ "success").asOpt[Boolean].getOrElse(false)
    val fallback = (e \ "fallback").asOpt[Boolean].getOrElse(false)
    val proxied  = (e \ "proxied").asOpt[Boolean].getOrElse(false)
    val service  = if (fallback) s"img: $host${ImageLoadBeacon.FallbackSuffix}" else s"img: $host"
    if (success) {
      monitor.recordSuccess(service)
      // Every poster weserv served, primary or spare, so the proxy's row has
      // green bars and a denominator its failures can be read against.
      if (proxied) monitor.recordSuccess(ImageLoadBeacon.ProxyService)
    } else {
      val error = (e \ "error").asOpt[String].getOrElse("image load failed")
      monitor.recordFailure(service, error.take(200))
    }
  }
}

object ImageLoadBeacon {
  /** The suffix of a host's row for its non-primary (fallback) poster loads. */
  val FallbackSuffix = " · fallback"

  /** The poster proxy's own row, fed the loads it served. */
  val ProxyService = s"img: ${tools.PosterProxy.ProxyHost}"

  /** The records one beacon may carry. */
  val MaxEventsPerBeacon = 500
}
