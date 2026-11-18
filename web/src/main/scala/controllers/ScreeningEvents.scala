package controllers

import play.api.libs.json.{JsObject, Json}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}

/** A film's showtimes as schema.org ScreeningEvents ([[FilmStructuredData]]): one per showtime, at its
 *  cinema, booked at its booking link (else the film's page). */
object ScreeningEvents {

  /** `title`'s showings, each starting in `zone`, its cinema at `address` (a PostalAddress). */
  def of(showings: Seq[(LocalDate, Seq[CinemaShowtimes])], title: String, canonicalUrl: String, zone: ZoneId,
         address: JsObject): Seq[JsObject] =
    showings.flatMap { case (_, perCinema) =>
      perCinema.flatMap { cs =>
        cs.showtimes.map { st =>
          val start = st.dateTime.atZone(zone).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          Json.obj(
            "@context" -> StructuredData.Ctx, "@type" -> "ScreeningEvent",
            "name" -> title,
            "startDate" -> start,
            "url" -> st.bookingUrl.flatMap(WebHref.of).fold(canonicalUrl)(_.url),
            "location" -> Json.obj(
              "@type" -> "MovieTheater", "name" -> cs.cinema.displayName,
              "address" -> address,
            ),
            "workPresented" -> Json.obj("@type" -> "Movie", "name" -> title),
          )
        }
      }
    }
}
