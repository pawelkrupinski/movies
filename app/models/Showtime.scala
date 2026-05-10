package models

import play.api.libs.json._
import java.time.Instant

case class Showtime(
  id: Option[String],
  movieId: String,          // references Movie._id
  cinema: String,           // e.g. "Poznań Stary Browar"
  date: String,             // raw date string from scraper, e.g. "12.05", "Dzisiaj"
  time: String,             // "HH:mm"
  screeningFormat: String,  // "2D", "3D", "IMAX"
  language: String,         // "napisy", "dubbing", "lektor"
  bookingUrl: Option[String],
  createdAt: Option[Instant]
)

object Showtime {

  implicit val instantFormat: Format[Instant] =
    Format(
      Reads(js => js.validate[Long].map(Instant.ofEpochMilli)),
      Writes(i => JsNumber(i.toEpochMilli))
    )

  implicit val format: OFormat[Showtime] = Json.format[Showtime]
}
