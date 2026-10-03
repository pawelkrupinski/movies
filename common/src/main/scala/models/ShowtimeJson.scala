package models

import java.time.LocalDateTime

import play.api.libs.functional.syntax._
import play.api.libs.json._

/** A showtime as JSON — the shape `Json.format` derived while `Showtime` was a case class (it no
 *  longer is; see it): `dateTime`, then `bookingUrl` and `room` when set, then `format`, always.
 *  Read back, an absent optional is `None` and an absent `format` is empty. `ShowtimeJsonSpec`
 *  pins it. The date-time spelling is the caller's. */
object ShowtimeJson {
  def format(implicit dateTimes: Format[LocalDateTime]): OFormat[Showtime] = (
    (__ \ "dateTime").format[LocalDateTime] and
      (__ \ "bookingUrl").formatNullable[String] and
      (__ \ "room").formatNullable[String] and
      (__ \ "format").formatWithDefault[List[String]](Nil)
  )((dateTime, bookingUrl, room, format) => Showtime(dateTime, bookingUrl, room, format),
    showtime => (showtime.dateTime, showtime.bookingUrl, showtime.room, showtime.format))
}
