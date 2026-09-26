package services.identity

import play.api.libs.json.{JsValue, Json}

import scala.util.Try

/**
 * TMDB's record of one film, as the identity measures read it, from every answer TMDB gave about
 * it: the deployment-language `/movie/{id}?append_to_response=credits,…` (title, credits) and the
 * en-US `/movie/{id}?append_to_response=alternative_titles`. ONE parser: the calibration reads the
 * recorded trees through it and the resolver's lookups read live (or replayed) answers through it,
 * so the fitted weights and the scored values are one definition.
 */
object TmdbFilmRecord {

  /** The film and its IMDb id, or `None` when no answer carries a title. A body a recorder
   *  wrapped as `{"text": …}` is unwrapped. */
  def parse(answers: Seq[JsValue]): Option[(IdentityMeasures.Film, Option[String])] = {
    val docs = answers.map(js => (js \ "text").asOpt[String].flatMap(t => Try(Json.parse(t)).toOption).getOrElse(js))
    val main = docs.filter(d => (d \ "title").isDefined)
    if (main.isEmpty) None
    else {
      val localized = main.find(d => (d \ "credits").isDefined).getOrElse(main.head)
      val crew = docs.flatMap(d => (d \ "crew").asOpt[Seq[JsValue]].orElse((d \ "credits" \ "crew").asOpt[Seq[JsValue]]).getOrElse(Nil))
      val directors = crew.filter(c => (c \ "job").asOpt[String].contains("Director")).flatMap(c => (c \ "name").asOpt[String]).distinct
      val hasCrew = docs.exists(d => (d \ "crew").isDefined || (d \ "credits" \ "crew").isDefined)
      val alternatives = main.flatMap(d => (d \ "alternative_titles" \ "titles").asOpt[Seq[JsValue]].getOrElse(Nil))
        .flatMap(t => (t \ "title").asOpt[String]) ++ main.flatMap(d => (d \ "title").asOpt[String])
      val title = (localized \ "title").as[String]
      val countries = main.flatMap(d => (d \ "production_countries").asOpt[Seq[JsValue]].getOrElse(Nil)
        .flatMap(c => (c \ "iso_3166_1").asOpt[String]) ++ (d \ "origin_country").asOpt[Seq[String]].getOrElse(Nil)).distinct
      Some(IdentityMeasures.Film(
        title             = title,
        originalTitle     = (localized \ "original_title").asOpt[String],
        alternativeTitles = alternatives.distinct.filterNot(_ == title),
        year              = yearOf((localized \ "release_date").asOpt[String]),
        runtime           = main.flatMap(d => (d \ "runtime").asOpt[Int]).find(_ > 0),
        directors         = Option.when(hasCrew)(directors),
        countries         = Option.when(countries.nonEmpty)(countries),
        popularity        = main.flatMap(d => (d \ "popularity").asOpt[Double]).headOption) ->
        main.flatMap(d => (d \ "imdb_id").asOpt[String]).find(_.nonEmpty))
    }
  }

  def yearOf(date: Option[String]): Option[Int] =
    date.filter(_.length >= 4).flatMap(d => Try(d.take(4).toInt).toOption)
}
