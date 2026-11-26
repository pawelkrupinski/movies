package services.enrichment

import play.api.libs.json.*
import tools.{HttpFetch, HttpRead, ReadOutcome}

/**
 * IMDb's own search, GraphQL `mainSearch` on [[ImdbClient.Endpoint]]: titles in any language IMDb lists them under, and
 * the people a name finds with the films they directed — what the identity resolver's IMDb family asks
 * (`services.agreementasks.ImdbFamily`), beside [[ImdbClient]]'s suggestions and records.
 */
final class ImdbSearch(http: HttpFetch) {
  import ImdbSearch._

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
    HttpRead.postJsonObject(http, ImdbClient.Endpoint, query) { js =>
      (js \ "data").asOpt[JsObject] match {
        case Some(data) => ReadOutcome.Answered(data)
        case None       => ReadOutcome.unexpectedBody(ImdbClient.Endpoint, "no GraphQL data", js.toString)
      }
    }.toOptionOrThrow

  private def mainSearch(query: String): Option[JsObject] = graphQlData(query).filter(data => (data \ "mainSearch").asOpt[JsObject].isDefined)
}

object ImdbSearch {
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
  def directingCreditsBody(personId: String): String = ImdbClient.askingId(DirectingCreditsQuery, personId)
  /** The film-like titles a [[directingCreditsBody]] answer's `data` credits the person with directing. */
  def directingCreditsIn(data: JsObject): Seq[String] =
    (data \ "name" \ "credits" \ "edges").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { edge =>
      val title = edge \ "node" \ "title"
      (title \ "id").asOpt[String].filter(_ => (title \ "titleType" \ "id").asOpt[String].exists(IdentityTitleTypes))
    }.distinct
}
