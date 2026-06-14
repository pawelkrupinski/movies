package services.enrichment

import play.api.libs.json._
import tools.HttpFetch

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
class ImdbClient(http: HttpFetch) {
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

  /** One GraphQL POST that returns rating + director + top cast + the
   *  English-language title and poster. Used by the IMDb enrichment stage
   *  to fill the `SourceData(Imdb)` slot in a single round-trip even when
   *  TMDB already supplied the IMDb id.
   *
   *  `principalCredits` carries up to ~10 directors/writers/stars in the
   *  same shape the title page uses. We pick the Directors block and the
   *  Stars block; cast names join into one comma-separated string capped
   *  at `MaxCastNames` to match TMDB's shape.
   *
   *  IMDb's `plot.plotText.plainText` is the English long-form synopsis
   *  — deliberately NOT fetched. Polish-audience film cards should show
   *  Polish copy (cinema-scraped or TMDB's pl-PL `overview`); IMDb's
   *  English plot used to win the merged-synopsis "longest wins" rule on
   *  rows where the cinema-side synopsis was short or missing, producing
   *  inappropriate English blurbs for Polish films. */
  def details(imdbId: String): Option[ImdbClient.Details] =
    Try(http.post(Endpoint, detailsQueryBody(imdbId), "application/json"))
      .toOption.flatMap(body => Try(parseDetails(body)).toOption)

  def parseDetails(body: String): ImdbClient.Details = {
    val title = Json.parse(body) \ "data" \ "title"
    val rating = parseRating(body)
    val titleText  = (title \ "titleText" \ "text").asOpt[String].filter(_.nonEmpty)
    val originalT  = (title \ "originalTitleText" \ "text").asOpt[String].filter(_.nonEmpty)
    val releaseYr  = (title \ "releaseYear" \ "year").asOpt[Int]
    val runtimeS   = (title \ "runtime" \ "seconds").asOpt[Int].map(s => (s / 60).max(1))
    val credits    = (title \ "principalCredits").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
    def namesFor(category: String): Seq[String] = credits
      .filter(c => (c \ "category" \ "id").asOpt[String].contains(category))
      .flatMap(c => (c \ "credits").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty))
      .flatMap(c => (c \ "name" \ "nameText" \ "text").asOpt[String])
      .filter(_.nonEmpty)
    val directors = namesFor("director")
    val stars     = namesFor("cast").take(MaxCastNames)
    val countries = (title \ "countriesOfOrigin" \ "countries").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(c => (c \ "text").asOpt[String]).filter(_.nonEmpty)
    val poster = (title \ "primaryImage" \ "url").asOpt[String].filter(_.nonEmpty)
    ImdbClient.Details(
      rating         = rating,
      title          = titleText,
      originalTitle  = originalT,
      director       = directors,
      cast           = stars,
      runtimeMinutes = runtimeS,
      releaseYear    = releaseYr,
      countries      = countries,
      posterUrl      = poster
    )
  }

  private def queryBody(imdbId: String): String = {
    // GraphQL with `id` as a String variable; sent as a JSON object body.
    val query = "query Rating($id:ID!){title(id:$id){ratingsSummary{aggregateRating voteCount}}}"
    Json.stringify(Json.obj(
      "query"     -> query,
      "variables" -> Json.obj("id" -> imdbId)
    ))
  }

  // Larger GraphQL query covering rating + credits + title info. IMDb's
  // caching CDN accepts the same `title(id)` query shape — this is
  // exactly what their site uses to render the title page header. Plot
  // (`plot{plotText{plainText}}`) deliberately omitted; see `details`.
  private def detailsQueryBody(imdbId: String): String = {
    val query =
      """query TitleDetails($id:ID!){
        |  title(id:$id){
        |    titleText{text}
        |    originalTitleText{text}
        |    releaseYear{year}
        |    runtime{seconds}
        |    ratingsSummary{aggregateRating voteCount}
        |    countriesOfOrigin{countries{text}}
        |    primaryImage{url}
        |    principalCredits{
        |      category{id}
        |      credits{name{nameText{text}}}
        |    }
        |  }
        |}""".stripMargin
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
   *  Conservative match: only `qid == "movie"` entries qualify. An exact
   *  case-insensitive title match wins (year-closest breaks ties, rank —
   *  lower = more popular — is the final tie-breaker). Failing that, a
   *  foreign film whose IMDb display title is its international/English name
   *  (e.g. Polish "Kumotry" → IMDb "Double Trouble") is accepted only when
   *  it is IMDb's top movie suggestion AND its year matches the one we
   *  already know from TMDB. Otherwise None — a wrong id pollutes ratings +
   *  RT lookups downstream.
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
      // Real film candidates: tt-id, qid "movie". Keep document order — IMDb
      // returns the best query match first, popularity padding after.
      val movies = (js \ "d").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        .flatMap { entry =>
          for {
            id  <- (entry \ "id").asOpt[String] if id.startsWith("tt")
            qid <- (entry \ "qid").asOpt[String] if qid == "movie"
          } yield Suggestion(id, (entry \ "l").asOpt[String].map(_.toLowerCase.trim), (entry \ "y").asOpt[Int], (entry \ "rank").asOpt[Int].getOrElse(Int.MaxValue))
        }
      val exact = movies
        .filter(_.title.contains(q))
        .sortBy { s =>
          val yearDistance = year.flatMap(request => s.year.map(yi => math.abs(yi - request))).getOrElse(Int.MaxValue)
          (yearDistance, s.rank)
        }
        .headOption.map(_.id)
      // Foreign-title fallback: when nothing matches the local title, accept
      // IMDb's #1 movie suggestion only if its year corroborates the one TMDB
      // gave us. That pair of signals (query relevance + exact year) is enough
      // to bind e.g. "Kumotry"→"Double Trouble" without wild-guessing.
      exact.orElse(year.flatMap(request => movies.headOption.collect {
        case s if s.year.contains(request) => s.id
      }))
    }
  }
}

object ImdbClient {
  private val Endpoint        = "https://caching.graphql.imdb.com/"
  private val SuggestionBase  = "https://v3.sg.media-imdb.com/suggestion"
  // Mirror the threshold TMDB suppression used: rating with <5 votes is noise.
  val MinVotes: Int    = 5
  // Top-N cap for IMDb's principal cast — matches TMDB's shape.
  val MaxCastNames: Int = 5

  /** One parsed suggestion-endpoint movie row: tt-id plus the fields the
   *  matcher ranks on (lowercased display title, release year, popularity
   *  rank). */
  private case class Suggestion(id: String, title: Option[String], year: Option[Int], rank: Int)

  /** Full IMDb record consumed by the IMDb enrichment stage: rating plus the
   *  content fields that fill `SourceData(Imdb)` (synopsis, director, cast,
   *  …). Returned by `details(imdbId)` in one GraphQL round-trip. */
  case class Details(
    rating:         Option[Double],
    title:          Option[String],
    originalTitle:  Option[String],
    director:       Seq[String],
    cast:           Seq[String],
    runtimeMinutes: Option[Int],
    releaseYear:    Option[Int],
    countries:      Seq[String],
    posterUrl:      Option[String]
  )
}
