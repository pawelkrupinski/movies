package views

import controllers.UptimeBarPayload
import play.api.libs.json.{JsArray, JsValue, Json}

/** Reads the compact `/uptime` grid payload ([[controllers.UptimeBarPayload]]) back
 *  into buckets, the way the page's script expands it — so a spec asserts on what a
 *  bar SHOWS rather than on the payload's index encoding. The page script itself is
 *  exercised in real Chrome by `UptimeLiveBarsSpec`. */
object UptimePayload {

  final case class Slot(timestamp: Long, timeFrom: String, timeTo: String, dateLabel: String)
  final case class Bucket(status: String, successes: Int, failures: Int, zeroes: Int,
                          fallback: Boolean, thin: Boolean, errors: Seq[String])

  final class Decoded(json: JsValue) {
    val slots: Seq[Slot] = (json \ "slots").as[Seq[JsArray]].map { s =>
      Slot(s(0).as[Long], s(1).as[String], s(2).as[String], s(3).as[String])
    }
    private val statuses = (json \ "statuses").as[Seq[String]]
    private val errors   = (json \ "errors").as[Seq[String]]

    def services: Set[String] = (json \ "data").as[Map[String, JsValue]].keySet

    def bucket(service: String, timestamp: Long): Option[Bucket] =
      (json \ "data" \ service).asOpt[Seq[Seq[Int]]].getOrElse(Seq.empty)
        .find(b => slots(b(0)).timestamp == timestamp)
        .map(b => Bucket(statuses(b(1)), b(2), b(3), b(4),
          fallback = (b(5) & UptimeBarPayload.FallbackFlag) != 0,
          thin     = (b(5) & UptimeBarPayload.ThinFlag) != 0,
          errors   = b.drop(6).map(errors)))
  }

  def of(payload: String): Decoded = new Decoded(Json.parse(payload))

  /** The payload embedded in a rendered page — `<` inside it is escaped, so the
   *  block ends at the first `<`. */
  def inPage(html: String): Decoded =
    of(html.split("""id="uptime-bars">""", 2).last.takeWhile(_ != '<'))
}
