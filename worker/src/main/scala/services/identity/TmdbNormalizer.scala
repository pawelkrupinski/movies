package services.identity

import clients.TmdbClient
import play.api.Logging
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import services.enrichment.ImdbClient
import tools.{HttpFetch, HttpStatusException}

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
 * Each identity response, as it arrives, into [[TmdbStore]]'s normalized documents — read through
 * the very parsers the clients use (`TmdbClient.rankedResults`, `personCandidates`, `creditsIn`,
 * `parseFindMovieResults`, `ImdbClient.movieSuggestions`, `TmdbFilmRecord`), so a stored answer is
 * what the client made of the body. A response that answers no identity question is ignored.
 *
 * Failures follow the clients' own reading: a 404 on a film's record or a person's credits is an
 * answer (the clients read it as `{"crew":[]}`), on IMDb's suggestions an empty one; a 404 on a
 * search or a find, and any transient failure, is no answer — nothing is written, and what the
 * store held stands.
 */
final class TmdbNormalizer(store: TmdbStore) extends Logging {
  import TmdbStore.Partial

  def filed(method: String, url: String, outcome: Try[String]): Unit =
    try normalize(method, url, outcome)
    catch { case NonFatal(e) => logger.warn(s"identity store: $method $url not normalized: $e") }

  private def normalize(method: String, url: String, outcome: Try[String]): Unit = if (method == "GET") {
    val uri    = new URI(url)
    val params = Option(uri.getRawQuery).toSeq.flatMap(_.split('&')).flatMap { pair =>
      pair.split("=", 2) match {
        case Array(k, v) => Some(k -> URLDecoder.decode(v, StandardCharsets.UTF_8))
        case Array(k)    => Some(k -> "")
        case _           => None
      }
    }.toMap
    val path   = Option(uri.getPath).getOrElse("").split('/').filter(_.nonEmpty).toSeq
    val body   = outcome match {
      case Success(b)                                          => Some(Right(b))
      case Failure(e: HttpStatusException) if e.code == 404    => Some(Left(404))
      case Failure(_)                                          => None
    }
    (Option(uri.getHost).getOrElse(""), path, body) match {
      case ("api.themoviedb.org", Seq("3", "search", "movie"), Some(Right(b))) if !params.contains("year") =>
        params.get("query").zip(params.get("language")).foreach { case (query, language) =>
          store.question(TmdbStore.titleSearchId(language, query), TmdbClient.rankedResults(b).map(hitOf))
        }
      case ("api.themoviedb.org", Seq("3", "search", "person"), Some(Right(b))) =>
        params.get("query").foreach(query => store.people(TmdbStore.personSearchId(query), TmdbClient.personCandidates(b)))
      case ("api.themoviedb.org", Seq("3", "person", id, "movie_credits"), Some(answer)) if id.forall(_.isDigit) =>
        val b = answer.getOrElse("""{"crew":[]}""")
        store.person(id.toInt, TmdbClient.creditsIn(b, "Directing").map(hitOf), TmdbClient.creditsIn(b, "Writing").map(hitOf))
      case ("api.themoviedb.org", Seq("3", "movie", id), Some(answer)) if id.forall(_.isDigit) =>
        val append = params.getOrElse("append_to_response", "")
        val partial =
          if (append.split(',').contains("credits")) Some(Partial.Local)
          else if (append == "alternative_titles" && params.get("language").contains("en-US")) Some(Partial.English)
          else None
        partial.foreach(p => store.filmPartial(id.toInt, p, answer.fold(_ => Json.obj("crew" -> JsArray()), b => TmdbNormalizer.minimal(Json.parse(b)))))
      case ("api.themoviedb.org", Seq("3", "find", imdbId), Some(Right(b))) if params.get("external_source").contains("imdb_id") =>
        store.question(TmdbStore.findId(imdbId), TmdbClient.parseFindMovieResults(b).map(hitOf))
      case (host, _, Some(answer)) if url.startsWith(ImdbClient.SuggestionBase) =>
        store.suggestions(TmdbStore.suggestionsId(url), answer.fold(_ => Nil, b => ImdbClient.movieSuggestions(Json.parse(b))))
      case _ => ()
    }
  }

  private def hitOf(r: TmdbClient.SearchResult): Hit = Hit(r.id, r.title, r.originalTitle, r.releaseYear, r.popularity)
}

object TmdbNormalizer {
  /** A `/movie/{id}` body cut to what `TmdbFilmRecord.parse` reads — title, original title, year,
   *  runtime, IMDb id, popularity (as its bucket), countries, alternative titles and the directors
   *  among the crew — keeping the shape it reads them in (a `credits` block stays a `credits` block:
   *  its presence is what marks the localized response, and "directors known"). */
  def minimal(body: JsValue): JsValue = {
    val keep = Seq("title", "original_title", "runtime", "imdb_id", "origin_country")
      .flatMap(k => (body \ k).toOption.map(k -> _))
    val date = (body \ "release_date").asOpt[String].map(d => "release_date" -> Json.toJson(d.take(4)))
    val popularity = (body \ "popularity").asOpt[Double].map(p => "popularity" -> Json.toJson(PopularityBucket.representative(PopularityBucket.of(p))))
    val countries = (body \ "production_countries").asOpt[Seq[JsValue]].map(cs =>
      "production_countries" -> JsArray(cs.flatMap(c => (c \ "iso_3166_1").asOpt[String]).map(iso => Json.obj("iso_3166_1" -> iso))))
    val alternatives = (body \ "alternative_titles" \ "titles").asOpt[Seq[JsValue]].map(ts =>
      "alternative_titles" -> Json.obj("titles" -> JsArray(ts.flatMap(t => (t \ "title").asOpt[String]).map(t => Json.obj("title" -> t)))))
    def directors(crew: Seq[JsValue]) = JsArray(crew.filter(c => (c \ "job").asOpt[String].contains("Director"))
      .flatMap(c => (c \ "name").asOpt[String]).map(n => Json.obj("job" -> "Director", "name" -> n)))
    val credits = (body \ "credits").toOption.map(c => "credits" -> Json.obj("crew" -> directors((c \ "crew").asOpt[Seq[JsValue]].getOrElse(Nil))))
    val crew    = (body \ "crew").asOpt[Seq[JsValue]].map(c => "crew" -> directors(c))
    JsObject(keep ++ date ++ popularity ++ countries ++ alternatives ++ credits ++ crew)
  }
}

/** `underlying`, every response it answers also normalized into the identity store — what the
 *  pipeline's and the fill's clients fetch is filed as the model reads it, and never kept raw. */
final class NormalizingHttpFetch(underlying: HttpFetch, normalizer: TmdbNormalizer) extends HttpFetch {
  override def get(url: String): String                              = filed(url)(underlying.get(url))
  override def get(url: String, headers: Map[String, String]): String = filed(url)(underlying.get(url, headers))
  override def getBytes(url: String): Array[Byte]                   = underlying.getBytes(url)
  override def post(url: String, body: String, contentType: String): String = underlying.post(url, body, contentType)

  private def filed(url: String)(call: => String): String = {
    val outcome = Try(call)
    normalizer.filed("GET", url, outcome)
    outcome.get
  }
}
