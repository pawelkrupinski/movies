package services.identity

import clients.{TmdbClient, TmdbJson}
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
 * answer (the clients read it as `{"crew":[]}`), on IMDb's suggestions an empty one, on IMDb's titles
 * or record of a title none; a 404 on a search or a find, and any transient failure, is no answer —
 * nothing is written, and what the store held stands.
 */
final class TmdbNormalizer(store: TmdbStore, bodies: tools.JsonBodies = new tools.JsonBodies) extends Logging {
  import TmdbStore.Partial

  def filed(method: String, url: String, outcome: Try[String]): Unit =
    try normalize(method, url, outcome)
    catch { case NonFatal(e) => logger.warn(s"identity store: $method ${tools.RedactedUrl(url)} not normalized: $e") }

  /** A POST's answer: IMDb's titles of one title (`ImdbClient.titlesOf`), or its identity record
   *  (`ImdbClient.identityRecord`). A 404 is IMDb having no such title — no titles, no record — as the client reads
   *  it; any other POST, and any other failure, files nothing. */
  def filedPost(url: String, body: String, outcome: Try[String]): Unit =
    try if (url == ImdbClient.Endpoint) outcome match {
      case Success(answer) =>
        ImdbClient.titlesQueryId(body).foreach(id => store.imdbTitles(TmdbStore.imdbTitlesId(id), ImdbClient.titlesIn(bodies.parse(answer))))
        // a reply without GraphQL data is a failed read, not IMDb saying it has no such title
        ImdbClient.identityRecordId(body).map(_ -> bodies.parse(answer)).filter { case (_, js) => (js \ "data").toOption.isDefined }
          .foreach { case (id, js) => store.imdbRecord(TmdbStore.imdbRecordId(id), ImdbClient.identityRecordIn(js)) }
      case Failure(e) if tools.ReadOutcome.isAbsent(e) =>
        ImdbClient.titlesQueryId(body).foreach(id => store.imdbTitles(TmdbStore.imdbTitlesId(id), Nil))
        ImdbClient.identityRecordId(body).foreach(id => store.imdbRecord(TmdbStore.imdbRecordId(id), None))
      case Failure(_) => ()
    }
    catch { case NonFatal(e) => logger.warn(s"identity store: POST ${tools.RedactedUrl(url)} not normalized: $e") }

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
          store.question(TmdbStore.titleSearchId(language, query), TmdbClient.rankedResults(b).map(TmdbIdentityLookups.hitOf))
        }
      case ("api.themoviedb.org", Seq("3", "search", "person"), Some(Right(b))) =>
        params.get("query").foreach(query => store.people(TmdbStore.personSearchId(query), TmdbClient.personCandidates(b)))
      case ("api.themoviedb.org", Seq("3", "person", id, "movie_credits"), Some(answer)) if id.forall(_.isDigit) =>
        val b = answer.getOrElse("""{"crew":[]}""")
        store.person(id.toInt, TmdbClient.creditsIn(b, "Directing").map(TmdbIdentityLookups.hitOf), TmdbClient.creditsIn(b, "Writing").map(TmdbIdentityLookups.hitOf))
      case ("api.themoviedb.org", Seq("3", "movie", id), Some(answer)) if id.forall(_.isDigit) =>
        val append = params.getOrElse("append_to_response", "")
        val partial =
          if (append.split(',').contains("credits")) Some(Partial.Local)
          else if (append == "alternative_titles" && params.get("language").contains("en-US")) Some(Partial.English)
          else None
        partial.foreach(p => store.filmPartial(id.toInt, p, answer.fold(_ => Json.obj("crew" -> JsArray()), b => TmdbNormalizer.minimal(bodies.parse(b)))))
      case ("api.themoviedb.org", Seq("3", "find", imdbId), Some(Right(b))) if params.get("external_source").contains("imdb_id") =>
        store.question(TmdbStore.findId(imdbId), TmdbClient.parseFindMovieResults(b).map(TmdbIdentityLookups.hitOf))
      case (host, _, Some(answer)) if url.startsWith(ImdbClient.SuggestionBase) =>
        store.suggestions(TmdbStore.suggestionsId(url), answer.fold(_ => Nil, b => ImdbClient.movieSuggestions(Json.parse(b))))
      case _ => ()
    }
  }

}

object TmdbNormalizer {
  /** A `/movie/{id}` body cut to what `TmdbFilmRecord.parse` reads — title, original title, release day,
   *  runtime, IMDb id, popularity (as its bucket), countries, alternative titles, the directors
   *  among the crew, the top-billed cast's names and which countries date a release of it, and its cinema releases (country, year, edition) — keeping the shape it reads them in (a `credits` block stays a `credits` block:
   *  its presence is what marks the localized response, and "directors known"). */
  def minimal(body: JsValue): JsValue = {
    val keep = Seq("title", "original_title", "runtime", "imdb_id", "origin_country")
      .flatMap(k => (body \ k).toOption.map(k -> _))
    // the whole day: a broadcast airs on it ([[services.identity.agreement.Broadcast]])
    val date = (body \ "release_date").asOpt[String].map(d => "release_date" -> Json.toJson(d.take(10)))
    val popularity = (body \ "popularity").asOpt[Double].map(p => "popularity" -> Json.toJson(PopularityBucket.representative(PopularityBucket.of(p))))
    val countries = (body \ "production_countries").asOpt[Seq[JsValue]].map(cs =>
      "production_countries" -> JsArray(cs.flatMap(c => (c \ "iso_3166_1").asOpt[String]).map(iso => Json.obj("iso_3166_1" -> iso))))
    val alternatives = (body \ "alternative_titles" \ "titles").asOpt[Seq[JsValue]].map(ts =>
      "alternative_titles" -> Json.obj("titles" -> JsArray(ts.flatMap(t => (t \ "title").asOpt[String]).map(t => Json.obj("title" -> t)))))
    def directors(crew: Seq[JsValue]) = JsArray(TmdbJson.crewWith(crew, TmdbFilmRecord.DirectorJobs)
      .flatMap(c => for { job <- (c \ "job").asOpt[String]; name <- (c \ "name").asOpt[String] } yield Json.obj("job" -> job, "name" -> name)))
    // the top-billed cast's names, in billing order: all the cast evidence reads of it (`TmdbFilmRecord.cast`)
    def cast(c: JsValue) = TmdbFilmRecord.cast(Seq(Json.obj("credits" -> c))).map(names => "cast" -> JsArray(names.map(name => Json.obj("name" -> name))))
    val credits = (body \ "credits").toOption.map(c => "credits" -> JsObject(Seq("crew" -> directors((c \ "crew").asOpt[Seq[JsValue]].getOrElse(Nil))) ++ cast(c)))
    val crew    = (body \ "crew").asOpt[Seq[JsValue]].map(c => "crew" -> directors(c))
    // which countries date a release, never the dates: the release veto's one question (`Film.releasedIn`)
    val released = (body \ "release_dates").toOption.map(block => TmdbFilmRecord.ReleaseCountries -> Json.toJson(TmdbFilmRecord.releaseCountries(block)))
    // and the cinema releases' countries, years and editions: the years a listing's year is read against (`Film.releaseYears`)
    val releases = (body \ "release_dates").toOption.map(block => TmdbFilmRecord.Releases -> Json.toJson(TmdbFilmRecord.releases(block)))
    JsObject(keep ++ date ++ popularity ++ countries ++ alternatives ++ credits ++ crew ++ released ++ releases)
  }
}

/** `underlying`, every response it answers also normalized into the identity store — what the
 *  pipeline's and the fill's clients fetch is filed as the model reads it, and never kept raw. */
final class NormalizingHttpFetch(underlying: HttpFetch, normalizer: TmdbNormalizer) extends HttpFetch {
  override def get(url: String): String                              = filed(url)(underlying.get(url))
  override def get(url: String, headers: Map[String, String]): String = filed(url)(underlying.get(url, headers))
  override def getBytes(url: String): Array[Byte]                   = underlying.getBytes(url)
  override def post(url: String, body: String, contentType: String): String = {
    val outcome = Try(underlying.post(url, body, contentType))
    normalizer.filedPost(url, body, outcome)
    outcome.get
  }

  private def filed(url: String)(call: => String): String = {
    val outcome = Try(call)
    normalizer.filed("GET", url, outcome)
    outcome.get
  }
}
