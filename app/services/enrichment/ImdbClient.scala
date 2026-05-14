package services.enrichment

import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Direct IMDb ratings via the public CDN GraphQL endpoint that imdb.com itself
 * uses. Returns the same rating you'd see on the IMDb title page.
 *
 * Endpoint:
 *   POST https://caching.graphql.imdb.com/
 *   body: GraphQL { title(id) { ratingsSummary { aggregateRating voteCount } } }
 *
 * Note: IMDb's API disclaimer says "Public, commercial, and/or non-private use
 * of the IMDb data provided by this API is not allowed". This is the same
 * licensing they apply to all their unofficial APIs.
 */
class ImdbClient(http: HttpFetch = new RealHttpFetch()) {
  import ImdbClient._

  /** Live IMDb rating, or None when unrated / unknown / network blip. */
  def lookup(imdbId: String): Option[Double] =
    Try(parseRating(http.post(Endpoint, queryBody(imdbId), "application/json"))).toOption.flatten

  def parseRating(body: String): Option[Double] = {
    val js = Json.parse(body)
    val summary = js \ "data" \ "title" \ "ratingsSummary"
    for {
      r <- (summary \ "aggregateRating").asOpt[JsValue].flatMap {
             case JsNumber(n) => Some(n.toDouble)
             case _           => None
           } if r > 0
      // Suppress single-enthusiast ratings the same way TMDB used to —
      // too few votes is noisy and unrepresentative.
      v = (summary \ "voteCount").asOpt[Int].getOrElse(0)
      if v >= MinVotes
    } yield r
  }

  private def queryBody(imdbId: String): String = {
    // GraphQL with `id` as a String variable; sent as a JSON object body.
    val query = "query Rating($id:ID!){title(id:$id){ratingsSummary{aggregateRating voteCount}}}"
    Json.stringify(Json.obj(
      "query"     -> query,
      "variables" -> Json.obj("id" -> imdbId)
    ))
  }

  /** Find an IMDb tt-id by title (+ optional year) using IMDb's public
   *  suggestion endpoint — the same JSON the imdb.com header autocomplete
   *  hits. Used as a fallback when TMDB resolves a film but has no IMDb
   *  cross-reference yet (e.g. "Mortal Kombat II" 2026).
   *
   *  Endpoint: GET `https://v3.sg.media-imdb.com/suggestion/{prefix}/{q}.json`.
   *  No auth. The `prefix` segment is conventionally the lowercase first
   *  letter of the query; for non-ASCII titles we fall back to `x` which the
   *  endpoint also accepts.
   *
   *  Conservative match: only `qid == "movie"` entries with an exact
   *  case-insensitive title match qualify. Year-closest breaks ties; rank
   *  (lower = more popular) is the final tie-breaker. Returns None rather
   *  than guessing — a wrong id pollutes ratings + RT lookups downstream.
   */
  def findId(title: String, year: Option[Int]): Option[String] = {
    if (title.trim.isEmpty) None
    else {
      val encoded = URLEncoder.encode(title, StandardCharsets.UTF_8)
      val prefix  = title.trim.headOption.filter(c => c.isLetter && c.toInt < 128).map(_.toLower).getOrElse('x')
      val url     = s"$SuggestionBase/$prefix/$encoded.json"
      Try(http.get(url)).toOption.flatMap(body => parseSuggestions(body, title, year))
    }
  }

  /** Parse the IMDb suggestion JSON. See `findId` for the matching rules.
   *  Public for testability — the parsing has enough corner cases (non-tt
   *  ids, video games sharing the title, missing optional fields) that we
   *  want fixture-driven assertions independent of HTTP. */
  def parseSuggestions(body: String, title: String, year: Option[Int]): Option[String] = {
    val q = title.toLowerCase.trim
    Try(Json.parse(body)).toOption.flatMap { js =>
      (js \ "d").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        .flatMap { entry =>
          for {
            id <- (entry \ "id").asOpt[String] if id.startsWith("tt")
            qid <- (entry \ "qid").asOpt[String] if qid == "movie"
            l <- (entry \ "l").asOpt[String] if l.toLowerCase.trim == q
          } yield (id, (entry \ "y").asOpt[Int], (entry \ "rank").asOpt[Int].getOrElse(Int.MaxValue))
        }
        .sortBy { case (_, y, rank) =>
          val yearDistance = year.flatMap(req => y.map(yi => math.abs(yi - req))).getOrElse(Int.MaxValue)
          (yearDistance, rank)
        }
        .headOption.map(_._1)
    }
  }
}

object ImdbClient {
  private val Endpoint        = "https://caching.graphql.imdb.com/"
  private val SuggestionBase  = "https://v3.sg.media-imdb.com/suggestion"
  // Mirror the threshold TMDB suppression used: rating with <5 votes is noise.
  val MinVotes: Int = 5
}
