package services.cinemas

import models._
import play.api.libs.json._
import services.movies.TrailerEmbed
import tools.HttpFetch

import java.time.{Instant, ZoneId}
import scala.util.Try

/**
 * KinoGram (Warszawa, Fabryka Norblina). The ticketing backend is an Apollo
 * GraphQL API whose `getScreeningList` returns every screening with its film
 * fully described inline — so a single POST yields the whole repertoire, no
 * per-film fetches or HTML parsing.
 */
class KinoGramClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoGram

  private val ApiUrl  = "https://bilety.kinogram.pl/api/graphql"
  private val BookingBase = "https://bilety.kinogram.pl/screening/"
  private val WarsawZone  = ZoneId.of("Europe/Warsaw")

  private val Query =
    """{ getScreeningList(query: {}) { id screeningTimeFrom screen { name } movie { id title originalTitle duration description director country yearOfProduction genres { name } posters trailers } } }"""

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl)

  def fetch(): Seq[CinemaMovie] = {
    val body = Json.obj("query" -> Query).toString
    val json = Try(Json.parse(http.post(ApiUrl, body, "application/json"))).getOrElse(Json.obj())
    val screenings = (json \ "data" \ "getScreeningList").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

    screenings.groupBy(s => (s \ "movie" \ "id").asOpt[String].getOrElse("")).toSeq.flatMap { case (movieId, group) =>
      if (movieId.isEmpty) None
      else {
        val movie = group.head \ "movie"
        val showtimes = group.flatMap { s =>
          (s \ "screeningTimeFrom").asOpt[String].flatMap(ts => Try(Instant.parse(ts)).toOption).map { inst =>
            val dt = inst.atZone(WarsawZone).toLocalDateTime
            val id = (s \ "id").asOpt[String]
            Showtime(dt, id.map(BookingBase + _), (s \ "screen" \ "name").asOpt[String].filter(_.nonEmpty), Nil)
          }
        }.distinctBy(_.dateTime).sortBy(_.dateTime)

        (movie \ "title").asOpt[String].filter(_.nonEmpty).map { title =>
          CinemaMovie(
            movie     = Movie(
              title          = title,
              runtimeMinutes = (movie \ "duration").asOpt[Int].filter(_ > 0),
              releaseYear    = (movie \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
              originalTitle  = (movie \ "originalTitle").asOpt[String].filter(_.nonEmpty),
              countries      = (movie \ "country").asOpt[String].map(_.trim).filter(_.nonEmpty)
                                 .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
                                 .map(KinoGramClient.toPolishCountry),
              genres         = (movie \ "genres").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
                                 .flatMap(g => (g \ "name").asOpt[String]).filter(_.nonEmpty)
                                 .map(tools.TextNormalization.titleCaseIfAllLower)
            ),
            cinema    = cinema,
            posterUrl = (movie \ "posters").asOpt[JsArray].flatMap(_.value.headOption).flatMap(_.asOpt[String]).filter(_.nonEmpty),
            filmUrl   = None,
            // description is bilingual: "[PL] …polski… [/PL] [EN] …english… [/EN]".
            // Keep only the Polish part.
            synopsis  = (movie \ "description").asOpt[String]
                          .map(_.replaceFirst("^\\[PL\\]\\s*", "").split("\\[/PL\\]").head.trim)
                          .filter(_.nonEmpty).map(tools.TextNormalization.stripHtml),
            cast      = Seq.empty,
            director  = (movie \ "director").asOpt[String].filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
            showtimes = showtimes,
            externalIds = Map.empty,
            trailerUrl = (movie \ "trailers").asOpt[JsArray].flatMap(_.value.headOption).flatMap(_.asOpt[String])
                          .filter(_.nonEmpty).flatMap(u => TrailerEmbed.youTubeId(u).map(id => s"https://www.youtube.com/watch?v=$id"))
          )
        }.filter(_.showtimes.nonEmpty)
      }
    }
  }
}

object KinoGramClient {

  /** The GraphQL `country` field uses English names while every other source
   *  (and the UI) uses Polish; map the common ones so countries merge. Unknown
   *  names pass through unchanged. */
  private val CountryPl: Map[String, String] = Map(
    "The United States of America" -> "USA", "United States of America" -> "USA", "United States" -> "USA", "USA" -> "USA",
    "UK" -> "Wielka Brytania", "United Kingdom" -> "Wielka Brytania", "Great Britain" -> "Wielka Brytania",
    "France" -> "Francja", "Germany" -> "Niemcy", "Poland" -> "Polska", "Italy" -> "Włochy", "Spain" -> "Hiszpania",
    "Canada" -> "Kanada", "Australia" -> "Australia", "Japan" -> "Japonia", "Ireland" -> "Irlandia",
    "Belgium" -> "Belgia", "Netherlands" -> "Holandia", "Sweden" -> "Szwecja", "Denmark" -> "Dania",
    "Norway" -> "Norwegia", "Switzerland" -> "Szwajcaria", "Austria" -> "Austria", "Czech Republic" -> "Czechy",
    "Czechia" -> "Czechy", "Ukraine" -> "Ukraina", "Russia" -> "Rosja", "China" -> "Chiny", "South Korea" -> "Korea Południowa"
  )

  def toPolishCountry(s: String): String = CountryPl.getOrElse(s, s)
}
