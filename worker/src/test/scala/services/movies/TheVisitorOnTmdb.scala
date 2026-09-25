package services.movies

/**
 * "The Visitor" (2022, TMDB 881487) as TMDB answers for it: the lone popular
 * search hit a director-less cinema row resolves to, and the IMDb id its
 * cross-references carry. Shared by the specs that drive one plain resolve
 * through `MovieService` and only care what happens around it (caching, a
 * failed carry-forward read, a dead candidate id).
 */
object TheVisitorOnTmdb {
  val Title  = "The Visitor"
  val Year   = Some(2022)
  val TmdbId = 881487

  val SearchPath      = "/search/movie"
  val ExternalIdsPath = s"/movie/$TmdbId/external_ids"

  val SearchBody: String = s"""{"results":[
    |{"id":$TmdbId,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.4}
    |]}""".stripMargin

  /** Search and cross-references, both answering: the resolve succeeds. */
  val Routes: Seq[(String, String)] = Seq(
    SearchPath      -> SearchBody,
    ExternalIdsPath -> s"""{"id":$TmdbId,"imdb_id":"tt15558152"}"""
  )
}
