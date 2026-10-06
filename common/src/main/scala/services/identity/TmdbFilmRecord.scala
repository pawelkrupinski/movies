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
      val directors = clients.TmdbJson.crewNames(crew, DirectorJobs)
      val hasCrew = docs.exists(d => (d \ "crew").isDefined || (d \ "credits" \ "crew").isDefined)
      val alternatives = main.flatMap(d => (d \ "alternative_titles" \ "titles").asOpt[Seq[JsValue]].getOrElse(Nil))
        .flatMap(t => (t \ "title").asOpt[String]) ++ main.flatMap(d => (d \ "title").asOpt[String])
      val title = (localized \ "title").as[String]
      val countries = main.flatMap(d => (d \ "production_countries").asOpt[Seq[JsValue]].getOrElse(Nil)
        .flatMap(c => (c \ "iso_3166_1").asOpt[String]) ++ (d \ "origin_country").asOpt[Seq[String]].getOrElse(Nil)).distinct
      val imdbId = main.flatMap(d => (d \ "imdb_id").asOpt[String]).find(_.nonEmpty)
      Some(IdentityMeasures.Film(
        title             = title,
        originalTitle     = (localized \ "original_title").asOpt[String],
        alternativeTitles = alternatives.distinct.filterNot(_ == title),
        year              = clients.TmdbJson.releaseYear(localized),
        runtime           = main.flatMap(d => (d \ "runtime").asOpt[Int]).find(_ > 0),
        directors         = Option.when(hasCrew)(directors),
        countries         = Option.when(countries.nonEmpty)(countries),
        popularity        = main.flatMap(d => (d \ "popularity").asOpt[Double]).headOption,
        imdbNumber        = imdbId.fold(0)(IdentityMeasures.imdbNumber),
        released          = clients.TmdbJson.releaseDate(localized),
        releaseCountries  = main.flatMap(d => (d \ "release_dates").toOption.map(releaseCountries)
          .orElse((d \ ReleaseCountries).asOpt[String])).headOption) -> imdbId)
    }
  }

  /** The field a cut-down record (`TmdbNormalizer.minimal`) keeps of TMDB's `release_dates` block: [[releaseCountries]]. */
  val ReleaseCountries = "release_countries"

  /** The countries a `release_dates` block dates a release in, any kind of release, their ISO-3166-1 codes in order and
   *  run together ("ATDEPL"): all the release veto reads of the block (`IdentityMeasures.Film.releasedIn`), never the dates. */
  def releaseCountries(block: JsValue): String =
    (block \ "results").asOpt[Seq[JsValue]].getOrElse(Nil)
      .filter(result => (result \ "release_dates").asOpt[Seq[JsValue]].exists(_.nonEmpty))
      .flatMap(result => (result \ "iso_3166_1").asOpt[String]).filter(_.length == 2).distinct.sorted.mkString

  /** The crew jobs a film's record reads as its directors — and so the jobs the normalized store's
   *  cut-down responses keep (`TmdbNormalizer.minimal`): the two must name the same crew. Co-directors
   *  are directors to the venues that credit them ("Vincent. Legenda oceanu": Reza Memari directs,
   *  Pavel Hrubos and Steven Majaury co-direct), so a listing naming only them is not another film. A filmed
   *  stage production's stage director is who the venues credit ("Fallen Angels by Noël Coward": Scott
   *  Ellis, while TMDB's Director is Annette Jolles, who directed the filming). */
  val DirectorJobs: Set[String] = clients.TmdbJson.Director + "Co-Director" + "Stage Director"
}
