package services.enrichment

import play.api.libs.json._
import services.movies.SamePerson
import services.resolution.TitleMatch
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

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

  /** Live IMDb rating. `None` means IMDb ANSWERED and the film has no usable
   *  rating (no such title, or a title carrying none). A read that failed — the
   *  CDN block, a throttle, a server error, a timeout — throws, because the
   *  caller must be able to tell "no rating" from "we never found out": see
   *  [[tools.ReadOutcome]] and the 2026-07-30 IMDb outage it documents. */
  def lookup(imdbId: String): Option[Double] =
    graphQl(queryBody(imdbId)).flatMap(ratingOf)

  /** POST one GraphQL query and return its response. `None` when IMDb answered that it
   *  has no such title (a 404, or `data.title` null); a response that is not GraphQL's
   *  `{"data":…}` object — a CDN error page relayed as a 200, a bare `{"errors":…}` —
   *  throws: it used to be parsed in `Try(...).toOption`, which read a page that is not
   *  IMDb's answer as "no rating". */
  private def graphQl(query: String): Option[JsObject] =
    HttpRead.postJsonObject(http, Endpoint, query)(titleAnswer).toOptionOrThrow

  private def titleAnswer(js: JsObject): ReadOutcome[JsObject] =
    (js \ "data").asOpt[JsObject] match {
      case Some(data) if (data \ "title").asOpt[JsObject].isDefined => ReadOutcome.Answered(js)
      case Some(_) => ReadOutcome.none("IMDb has no such title")
      case None    => ReadOutcome.unexpectedBody(Endpoint, "no GraphQL data", js.toString)
    }

  def parseRating(body: String): Option[Double] = ratingOf(Json.parse(body))

  private def ratingOf(js: JsValue): Option[Double] = {
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
    graphQl(detailsQueryBody(imdbId)).map(detailsOf)

  def parseDetails(body: String): ImdbClient.Details = detailsOf(Json.parse(body))

  private def detailsOf(js: JsValue): ImdbClient.Details = {
    val title = js \ "data" \ "title"
    val rating = ratingOf(js)
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
   *  case-insensitive deburr-normalised title match wins (year-closest breaks
   *  ties, rank — lower = more popular — is the final tie-breaker). Failing
   *  that, a foreign film whose IMDb display title is its international/English
   *  name (e.g. Polish "Kumotry" → IMDb "Double Trouble") is accepted only
   *  when it is IMDb's top movie suggestion AND its year matches the one we
   *  already know from TMDB. Otherwise None — a wrong id pollutes ratings +
   *  RT lookups downstream.
   *
   *  The `directors` overload adds a last-resort: when neither the title-match
   *  nor the year-corroborated foreign-title path fires, all movie candidates
   *  are checked for a director overlap via `disambiguateByDirector`. */
  def findId(title: String, year: Option[Int]): Option[String] = findId(title, year, Set.empty)

  def findId(title: String, year: Option[Int], directors: Set[String]): Option[String] = {
    if (title.trim.isEmpty) None
    else {
      suggestions(title).flatMap { js =>
        bestSuggestion(js, title, year).orElse(
          if (directors.nonEmpty) disambiguateByDirector(js, directors) else None
        )
      }
    }
  }

  /** The IMDb ids of the first [[ImdbClient.SuggestedMovies]] films IMDb's suggestion endpoint suggests for
   *  `title`, in IMDb's order — whatever title it DISPLAYS them under: it matches a query against a film's
   *  other-language titles too, and shows its original ("Superfutrzak i złośliwa wiewiórka" suggests only
   *  tt35166699, displayed as the Finnish "Supermarsu ja suuri huijaus"). No choice between them: the identity
   *  resolver's candidate path (`TmdbIdentityLookups`), which weighs each on the listing's facts. Empty for a
   *  blank title or a query IMDb does not know; a failed read throws, as `http` threw it, and so does a body
   *  that is not JSON — it is no answer, and an empty list here is stored as one. */
  def suggestedIds(title: String): Seq[String] =
    if (title.trim.isEmpty) Nil
    else suggestions(title).toSeq.flatMap(js => ImdbClient.suggested(movieSuggestions(js)))

  /** The ids among IMDb's suggestions for `title` that IMDb lists under `title` itself in some language — its
   *  title, its original or one of its AKAs ([[ImdbClient.titled]]): "Camino dla opornych" is tt39814688's Polish
   *  title. A suggestion displayed under another title is asked for its titles; a failed read throws. */
  def titledIds(title: String): Seq[String] =
    if (title.trim.isEmpty) Nil else ImdbClient.titled(title, suggestedMovies(title), id => Some(titlesOf(id))).getOrElse(Nil)

  /** IMDb's movie suggestions for `title`, as read: empty for a blank title; a failed read throws. */
  private[services] def suggestedMovies(title: String): Seq[Suggestion] =
    if (title.trim.isEmpty) Nil else suggestions(title).toSeq.flatMap(movieSuggestions)

  /** Every title IMDb lists `imdbId` under: its title, its original and its AKAs, in IMDb's order. Empty for a
   *  title IMDb does not have; a failed read throws. */
  def titlesOf(imdbId: String): Seq[String] = graphQl(titlesQueryBody(imdbId)).fold(Seq.empty[String])(titlesIn)

  /** IMDb's record of `imdbId` as the identity measures read it ([[identityRecordIn]]); `None` when IMDb has no such
   *  title; a failed read throws. */
  def identityRecord(imdbId: String): Option[services.identity.IdentityMeasures.Film] =
    graphQl(ImdbClient.identityRecordBody(imdbId)).flatMap(ImdbClient.identityRecordIn)

  /** IMDb's own title search for `text`, best first: GraphQL `mainSearch`, which matches a title in ANY language IMDb
   *  lists it under (its AKAs: "Księga pustyni" finds "L'enfant du désert"), unlike the suggestion endpoint's
   *  prefix match on the English display title. Films and film-like titles only ([[IdentityTitleTypes]]). Empty for
   *  a blank text or when IMDb finds nothing; a failed read throws. */
  def searchTitles(text: String): Seq[SearchedTitle] =
    if (text.trim.isEmpty) Nil else mainSearch(titleSearchBody(text)).fold(Seq.empty[SearchedTitle])(searchedTitlesIn)

  /** The films the people IMDb's own name search finds for `name` directed — the first [[DirectorsAsked]] people,
   *  each with their directing credits in IMDb's order: the director path IMDb offers beside its title search.
   *  Empty when IMDb finds nobody by that name; a failed read throws. */
  def directedBy(name: String): Seq[String] =
    if (name.trim.isEmpty) Nil
    else mainSearch(nameSearchBody(name)).fold(Seq.empty[String])(peopleIn).take(DirectorsAsked)
      .flatMap(person => graphQlData(directingCreditsBody(person)).fold(Seq.empty[String])(directingCreditsIn)).distinct

  /** A GraphQL answer's `data` object, whatever it holds; a body that is not GraphQL's `{"data":…}` throws. */
  private def graphQlData(query: String): Option[JsObject] =
    HttpRead.postJsonObject(http, Endpoint, query) { js =>
      (js \ "data").asOpt[JsObject] match {
        case Some(data) => ReadOutcome.Answered(data)
        case None       => ReadOutcome.unexpectedBody(Endpoint, "no GraphQL data", js.toString)
      }
    }.toOptionOrThrow

  private def mainSearch(query: String): Option[JsObject] = graphQlData(query).filter(data => (data \ "mainSearch").asOpt[JsObject].isDefined)

  /** The suggestion endpoint's answer for `title`: an object carrying its `d` array
   *  (empty when IMDb knows nothing by that name). Anything else is a failed read. */
  private def suggestions(title: String): Option[JsObject] = {
    val url = suggestionUrl(title)
    HttpRead.jsonObject(http, url) { js =>
      if ((js \ "d").asOpt[JsArray].isDefined) ReadOutcome.Answered(js)
      else ReadOutcome.unexpectedBody(url, "no suggestion array 'd'", js.toString)
    }.toOptionOrThrow
  }

  /** Director-based fallback: when `parseSuggestions` finds no title match (the
   *  film may be listed under a different or international title on IMDb), consider
   *  ALL suggestion-API movie candidates and pick the one whose director list
   *  overlaps with `directors`. This covers AKA/foreign-title cases where the year
   *  also isn't yet set on IMDb, so neither the exact-title nor the year-corroborated
   *  foreign-title paths in `parseSuggestions` can fire.
   *
   *  Returns None when 0 or multiple candidates match — never guesses.
   *  Requires director data on the IMDb side; skips candidates with empty director
   *  lists so an undocumented entry doesn't accidentally match. */
  private def disambiguateByDirector(js: JsValue, directors: Set[String]): Option[String] = {
    val candidates = (js \ "d").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap { entry =>
        for {
          id  <- (entry \ "id").asOpt[String] if id.startsWith("tt")
          qid <- (entry \ "qid").asOpt[String] if qid == "movie"
        } yield id
      }.distinct.take(5)
    if (candidates.isEmpty) return None
    val matching = candidates.filter { id =>
      details(id) match {
        case None    => false
        case Some(d) =>
          // Require director data on IMDb's side — an undocumented entry provides no
          // evidence and must not be accepted without the confirmatory signal.
          d.director.nonEmpty && d.director.exists(dir => directors.exists(SamePerson(dir, _)))
      }
    }
    if (matching.sizeIs == 1) Some(matching.head) else None
  }

  /** Parse the IMDb suggestion JSON. See `findId` for the matching rules.
   *  Public for testability — the parsing has enough corner cases (non-tt
   *  ids, video games sharing the title, missing optional fields) that we
   *  want fixture-driven assertions independent of HTTP. */
  def parseSuggestions(body: String, title: String, year: Option[Int]): Option[String] =
    bestSuggestion(Json.parse(body), title, year)

  private def bestSuggestion(js: JsValue, title: String, year: Option[Int]): Option[String] = {
    val movies       = movieSuggestions(js)
    val titleMatches = ImdbClient.titleMatches(movies, title)
    val ranked = titleMatches.sortBy { s =>
      val yearDistance = year.flatMap(request => s.year.map(yi => math.abs(yi - request))).getOrElse(Int.MaxValue)
      (yearDistance, s.rank)
    }
    // With a YEAR the ranking above is evidence and the closest year wins. WITHOUT one,
    // several title matches are indistinguishable — refuse rather than let `rank` (IMDb's
    // own popularity ordering) decide, the same discipline `TmdbClient.searchUnique`
    // applies to a multi-hit yearless search on the TMDB side. A wrong id here is not a
    // wrong rating, it is the wrong FILM: it drives the year, director, cast and every
    // rating lookup downstream.
    //
    // One title match is not "unambiguous" either when IMDb itself ranks a DIFFERENT
    // film above it: the endpoint matches the query against AKAs too, and never shows
    // them, so a local title that is another film's AKA looks like one exact hit further
    // down. "Opętanie" answers Żuławski's "Possession" (1981) first — its Polish title —
    // and a 1973 TV film of that exact name at rank ~1M; binding the latter misnamed a
    // 4K revival of the former.
    //
    // The closest year is evidence only when it is CLOSE: a title match IMDb dates more than
    // [[ImdbClient.YearTolerance]] from the row's year is another film of that name ("Tosca"
    // 1941 for a 2027 opera relay, "It" 2017 for Cultplex's "It (1990)"). One IMDb has not
    // dated yet still binds, as before.
    val exact =
      if (year.isDefined) ranked.headOption.filter(s => ImdbClient.yearAgrees(s.year, year)).map(_.id)
      else if (titleMatches.sizeIs == 1 && movies.headOption.contains(titleMatches.head)) titleMatches.headOption.map(_.id)
      else None
    // Foreign-title fallback: when nothing matches the local title, accept
    // IMDb's #1 suggestion only if it is a movie and its year corroborates the one TMDB
    // gave us. That pair of signals (query relevance + exact year) is enough
    // to bind e.g. "Kumotry"→"Double Trouble" without wild-guessing. IMDb's top TITLE
    // (a `tt` id of any kind; a promo or a person listed ahead of it is no answer — "Twoje
    // imię" leads with a festival link), not its first MOVIE: "It (1990)" answers the 1990
    // miniseries first, and the first movie after it, "Strike It Rich" (1990), merely
    // shares the year.
    val topAnswer = (js \ "d").asOpt[JsArray].map(_.value.toSeq).getOrElse(Nil)
      .flatMap(e => (e \ "id").asOpt[String]).find(_.startsWith("tt"))
    exact.orElse(year.flatMap(request => movies.headOption.collect {
      case s if s.year.contains(request) && topAnswer.contains(s.id) => s.id
    }))
  }
}

object ImdbClient {
  /** How far IMDb's year may sit from the row's and still be the same film: a production
   *  year a year or two before the release a venue or TMDB reports. */
  val YearTolerance: Int = 2

  /** Whether IMDb's `found` year can be the row's `wanted` one — true when either is unknown. */
  private[services] def yearAgrees(found: Option[Int], wanted: Option[Int]): Boolean =
    (for (f <- found; w <- wanted) yield math.abs(f - w) <= YearTolerance).getOrElse(true)

  val Endpoint                = "https://caching.graphql.imdb.com/"

  /** The GraphQL query [[ImdbClient.titlesOf]] asks: a title's own, original and alternative titles. */
  private val TitlesQuery = "query Titles($id:ID!){title(id:$id){titleText{text} originalTitleText{text} akas(first:100){edges{node{text}}}}}"
  def titlesQueryBody(imdbId: String): String =
    Json.stringify(Json.obj("query" -> TitlesQuery, "variables" -> Json.obj("id" -> imdbId)))
  /** The IMDb id a [[titlesQueryBody]] asks about; `None` for any other body. */
  def titlesQueryId(body: String): Option[String] =
    Option.when(body.contains(Json.stringify(JsString(TitlesQuery))))(Json.parse(body)).flatMap(js => (js \ "variables" \ "id").asOpt[String])
  /** The GraphQL query [[identityRecord]] asks: what the identity measures read of a title — its titles in every
   *  language, year, running time, directors and countries (ISO codes) — in one round-trip. */
  private val IdentityRecordQuery = "query IdentityRecord($id:ID!){title(id:$id){titleText{text} originalTitleText{text} " +
    "releaseYear{year} runtime{seconds} countriesOfOrigin{countries{id}} principalCredits{category{id} credits{name{nameText{text}}}} " +
    "akas(first:100){edges{node{text}}}}}"
  def identityRecordBody(imdbId: String): String =
    Json.stringify(Json.obj("query" -> IdentityRecordQuery, "variables" -> Json.obj("id" -> imdbId)))
  /** The IMDb id an [[identityRecordBody]] asks about; `None` for any other body. */
  def identityRecordId(body: String): Option[String] =
    Option.when(body.contains(Json.stringify(JsString(IdentityRecordQuery))))(Json.parse(body)).flatMap(js => (js \ "variables" \ "id").asOpt[String])
  /** An [[IdentityRecordQuery]] answer as the identity measures read a film; `None` when IMDb has no such title. Its
   *  directors and countries are unknown, not "none", when the answer carries no credits or countries at all. */
  def identityRecordIn(js: JsValue): Option[services.identity.IdentityMeasures.Film] = {
    val title    = js \ "data" \ "title"
    val titles   = titlesIn(js)
    val own      = (title \ "titleText" \ "text").asOpt[String].filter(_.trim.nonEmpty).orElse(titles.headOption)
    val credits  = (title \ "principalCredits").asOpt[Seq[JsValue]]
    val directed = credits.map(_.filter(c => (c \ "category" \ "id").asOpt[String].contains("director"))
      .flatMap(c => (c \ "credits").asOpt[Seq[JsValue]].getOrElse(Nil)).flatMap(c => (c \ "name" \ "nameText" \ "text").asOpt[String])
      .filter(_.trim.nonEmpty).distinct)
    val countries = (title \ "countriesOfOrigin" \ "countries").asOpt[Seq[JsValue]].map(_.flatMap(c => (c \ "id").asOpt[String]).filter(_.nonEmpty))
    own.map { name =>
      val original = (title \ "originalTitleText" \ "text").asOpt[String].filter(_.trim.nonEmpty)
      services.identity.IdentityMeasures.Film(
        title             = name,
        originalTitle     = original,
        alternativeTitles = titles.filterNot(t => t == name || original.contains(t)),
        year              = (title \ "releaseYear" \ "year").asOpt[Int],
        runtime           = (title \ "runtime" \ "seconds").asOpt[Int].map(_ / 60).filter(_ > 0),
        directors         = directed,
        countries         = countries.filter(_.nonEmpty))
    }
  }

  /** One title IMDb's own search found: its id, display title, year and IMDb's type for it ("movie", "short", …). */
  final case class SearchedTitle(id: String, title: String, year: Option[Int], titleType: String)

  /** The title types a cinema listing can be: a film of any length, made for cinema, TV or video — never a series. */
  val IdentityTitleTypes: Set[String] = Set("movie", "tvMovie", "short", "video", "tvSpecial", "tvShort")
  /** How many of the people IMDb's name search finds for a credited name have their directing credits read. */
  val DirectorsAsked: Int = 2
  /** How many results a [[titleSearchBody]] asks for. */
  val TitleSearchResults: Int = 8

  private val TitleSearchQuery = "query TitleSearch($text:String!,$first:Int!){mainSearch(first:$first, options:{searchTerm:$text, " +
    "type:TITLE, includeAdult:false}){edges{node{entity{... on Title{id titleText{text} releaseYear{year} titleType{id}}}}}}}"
  def titleSearchBody(text: String): String =
    Json.stringify(Json.obj("query" -> TitleSearchQuery, "variables" -> Json.obj("text" -> text, "first" -> TitleSearchResults)))
  /** The film-like titles a [[titleSearchBody]] answer's `data` lists, in IMDb's order. */
  def searchedTitlesIn(data: JsObject): Seq[SearchedTitle] =
    (data \ "mainSearch" \ "edges").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { edge =>
      val entity = edge \ "node" \ "entity"
      for {
        id    <- (entity \ "id").asOpt[String].filter(_.startsWith("tt"))
        kind  <- (entity \ "titleType" \ "id").asOpt[String] if IdentityTitleTypes(kind)
        title <- (entity \ "titleText" \ "text").asOpt[String].filter(_.trim.nonEmpty)
      } yield SearchedTitle(id, title, (entity \ "releaseYear" \ "year").asOpt[Int], kind)
    }.distinctBy(_.id)

  private val NameSearchQuery = "query NameSearch($text:String!){mainSearch(first:3, options:{searchTerm:$text, type:NAME}){edges{node{entity{... on Name{id}}}}}}"
  def nameSearchBody(name: String): String = Json.stringify(Json.obj("query" -> NameSearchQuery, "variables" -> Json.obj("text" -> name)))
  /** The people a [[nameSearchBody]] answer's `data` lists, in IMDb's order. */
  def peopleIn(data: JsObject): Seq[String] =
    (data \ "mainSearch" \ "edges").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(e => (e \ "node" \ "entity" \ "id").asOpt[String]).filter(_.startsWith("nm"))

  private val DirectingCreditsQuery = "query DirectingCredits($id:ID!){name(id:$id){credits(first:100, filter:{categories:[\"director\"]})" +
    "{edges{node{title{id titleType{id}}}}}}}"
  def directingCreditsBody(personId: String): String =
    Json.stringify(Json.obj("query" -> DirectingCreditsQuery, "variables" -> Json.obj("id" -> personId)))
  /** The film-like titles a [[directingCreditsBody]] answer's `data` credits the person with directing. */
  def directingCreditsIn(data: JsObject): Seq[String] =
    (data \ "name" \ "credits" \ "edges").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { edge =>
      val title = edge \ "node" \ "title"
      (title \ "id").asOpt[String].filter(_ => (title \ "titleType" \ "id").asOpt[String].exists(IdentityTitleTypes))
    }.distinct

  /** The titles a [[TitlesQuery]] answer lists, each once. */
  def titlesIn(js: JsValue): Seq[String] = {
    val title = js \ "data" \ "title"
    ((title \ "titleText" \ "text").asOpt[String].toSeq ++ (title \ "originalTitleText" \ "text").asOpt[String] ++
      (title \ "akas" \ "edges").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(e => (e \ "node" \ "text").asOpt[String])).filter(_.trim.nonEmpty).distinct
  }

  /** Which of IMDb's first [[SuggestedMovies]] movie suggestions for `title` IMDb lists under `title` itself, in
   *  any language: the one it displays (`l`), or else one of the titles `titlesOf` reads for it — compared as the
   *  resolver keys titles (`IdentityMeasures.key`: case, accents and punctuation aside, so "Kuźma" is "Kuzma").
   *  `None` while a suggestion's titles are unknown. ONE reading for the live lookups and the store's. */
  def titled(title: String, movies: Seq[Suggestion], titlesOf: String => Option[Seq[String]]): Option[Seq[String]] = {
    val wanted = services.identity.IdentityMeasures.key(title)
    val first  = movies.distinctBy(_.id).take(SuggestedMovies)
    val named  = first.map { s =>
      if (wanted.isEmpty) Some(false)
      else if (s.title.exists(services.identity.IdentityMeasures.key(_) == wanted)) Some(true)
      else titlesOf(s.id).map(_.exists(services.identity.IdentityMeasures.key(_) == wanted))
    }
    Option.when(named.forall(_.isDefined))(first.zip(named.flatten).collect { case (s, true) => s.id })
  }
  val SuggestionBase          = "https://v3.sg.media-imdb.com/suggestion"
  /** Leading English article, for treating "The Bodyguard" and "Bodyguard" as the same
   *  claim on a title. English only: IMDb's primary titles are English, and Polish has no
   *  articles to strip. */
  private val LeadingArticle  = """^(?:the|a|an)\s+""".r
  // Mirror the threshold TMDB suppression used: rating with <5 votes is noise.
  val MinVotes: Int    = 5
  // Top-N cap for IMDb's principal cast — matches TMDB's shape.
  val MaxCastNames: Int = 5

  /** The suggestion endpoint's URL for `title`: the query, and a `prefix` segment that is
   *  conventionally its lowercase first letter (`x` for a non-ASCII one, which the endpoint also
   *  accepts). */
  def suggestionUrl(title: String): String = {
    val encoded = URLEncoder.encode(title, StandardCharsets.UTF_8)
    val prefix  = title.trim.headOption.filter(c => c.isLetter && c.toInt < 128).map(_.toLower).getOrElse('x')
    s"$SuggestionBase/$prefix/$encoded.json"
  }

  /** Real film candidates: tt-id, qid "movie". Document order — IMDb returns the best query match
   *  first, popularity padding after. */
  private[services] def movieSuggestions(js: JsValue): Seq[Suggestion] =
    (js \ "d").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap { entry =>
        for {
          id  <- (entry \ "id").asOpt[String] if id.startsWith("tt")
          qid <- (entry \ "qid").asOpt[String] if qid == "movie"
        } yield Suggestion(id, (entry \ "l").asOpt[String].map(TitleMatch.deburredFold), (entry \ "y").asOpt[Int], (entry \ "rank").asOpt[Int].getOrElse(Int.MaxValue))
      }

  /** The `movies` IMDb titles `title` itself. Deburred on both sides: IMDb stores titles in ASCII
   *  (ł→l, ą→a, ś→s, etc.) while our query titles retain Polish diacritics — and en-dash/em-dash
   *  titles must meet hyphen queries. Both are `TitleMatch.deburredFold`.
   *
   *  A leading article is not a difference. IMDb lists a film under its PRIMARY title, so the
   *  Polish release title "Bodyguard" belongs to "The Bodyguard" (1992) — an AKA this payload never
   *  shows — while an unrelated film carries "Bodyguard" as its own primary title. Bare equality
   *  therefore finds exactly one confident hit and it is the wrong film. Counting the
   *  article-stripped forms as matches too makes the ambiguity VISIBLE rather than letting it
   *  resolve silently. */
  private[services] def titleMatches(movies: Seq[Suggestion], title: String): Seq[Suggestion] = {
    val normalizedTitle = TitleMatch.deburredFold(title)
    def withoutArticle(t: String) = LeadingArticle.replaceFirstIn(t, "")
    movies.filter(_.title.exists { candidate =>
      candidate == normalizedTitle || withoutArticle(candidate) == withoutArticle(normalizedTitle)
    })
  }

  /** One parsed suggestion-endpoint movie row: tt-id plus the fields the
   *  matcher ranks on (lowercased display title, release year, popularity
   *  rank). */
  private[services] final case class Suggestion(id: String, title: Option[String], year: Option[Int], rank: Int)

  /** How many of IMDb's movie suggestions the identity resolver follows — the old pipeline's director rung read as many. */
  val SuggestedMovies = 5
  /** The ids of IMDb's first [[SuggestedMovies]] movie suggestions, in its order. */
  private[services] def suggested(movies: Seq[Suggestion]): Seq[String] = movies.map(_.id).distinct.take(SuggestedMovies)

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
