package models

import play.api.libs.json._
import java.time.Instant

case class Movie(
  id: Option[String],
  title: String,
  originalTitle: Option[String],
  genre: Option[String],
  durationMinutes: Option[Int],
  rating: Option[String],       // e.g. "7+", "12+", "18+"
  posterUrl: Option[String],
  multikinoId: Option[String],  // Multikino's own identifier; used for safe upserts
  createdAt: Option[Instant],
  updatedAt: Option[Instant]
)

object Movie {

  implicit val instantFormat: Format[Instant] =
    Format(
      Reads(js => js.validate[Long].map(Instant.ofEpochMilli)),
      Writes(i => JsNumber(i.toEpochMilli))
    )

  implicit val format: OFormat[Movie] = Json.format[Movie]
}
