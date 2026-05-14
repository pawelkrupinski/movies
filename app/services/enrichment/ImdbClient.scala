package services.enrichment

import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

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
}

object ImdbClient {
  private val Endpoint  = "https://caching.graphql.imdb.com/"
  // Mirror the threshold TMDB suppression used: rating with <5 votes is noise.
  val MinVotes: Int = 5
}
