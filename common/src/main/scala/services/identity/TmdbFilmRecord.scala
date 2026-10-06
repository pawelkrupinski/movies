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
      // the localized answer's first: the cut the deployment's market screens, then the other translations' cuts
      val runtimes = TmdbFilmRecord.runtimes(main.flatMap(d => (d \ "runtime").asOpt[Int]))
      Some(IdentityMeasures.Film(
        title             = title,
        originalTitle     = (localized \ "original_title").asOpt[String],
        alternativeTitles = alternatives.distinct.filterNot(_ == title),
        year              = clients.TmdbJson.releaseYear(localized),
        runtime           = runtimes.headOption,
        directors         = Option.when(hasCrew)(directors),
        countries         = Option.when(countries.nonEmpty)(countries),
        popularity        = main.flatMap(d => (d \ "popularity").asOpt[Double]).headOption,
        imdbNumber        = imdbId.fold(0)(IdentityMeasures.imdbNumber),
        released          = clients.TmdbJson.releaseDate(localized),
        releaseCountries  = main.flatMap(d => (d \ "release_dates").toOption.map(releaseCountries)
          .orElse((d \ ReleaseCountries).asOpt[String])).headOption,
        alternativeRuntimes = runtimes.drop(1),
        releases          = main.flatMap(d => (d \ "release_dates").toOption.map(releases).orElse((d \ Releases).asOpt[String])).headOption) -> imdbId)
    }
  }

  /** The running times TMDB's answers about one film state, in the answers' order and each once: TMDB keeps a runtime per
   *  translation, so its localized and English answers can state different cuts (`IdentityMeasures.Film.runtimes`). */
  def runtimes(stated: Seq[Int]): Seq[Int] = stated.filter(_ > 0).distinct

  /** The film's top-billed cast, the first [[TopBilled]] names in TMDB's billing order, from the localized response's
   *  `credits` block — `None` when no answer holds a cast: not fetched, or cut away by a store filing records before they
   *  kept it (`TmdbNormalizer.minimal`). An empty one is TMDB crediting nobody. */
  def cast(answers: Seq[JsValue]): Option[Seq[String]] = {
    val docs = answers.map(js => (js \ "text").asOpt[String].flatMap(t => Try(Json.parse(t)).toOption).getOrElse(js))
    docs.flatMap(d => (d \ "credits" \ "cast").asOpt[Seq[JsValue]]).headOption.map(cast =>
      cast.sortBy(c => (c \ "order").asOpt[Int].getOrElse(Int.MaxValue)).flatMap(c => (c \ "name").asOpt[String]).map(_.trim).filter(_.nonEmpty)
        .take(TopBilled))
  }

  /** How many of a film's cast count as its top-billed: as many as the offline measure read (2026-10-06). */
  val TopBilled = 10

  /** The field a cut-down record (`TmdbNormalizer.minimal`) keeps of TMDB's `release_dates` block: [[releaseCountries]]. */
  val ReleaseCountries = "release_countries"

  /** The countries a `release_dates` block dates a release in, any kind of release, their ISO-3166-1 codes in order and
   *  run together ("ATDEPL"): all the release veto reads of the block (`IdentityMeasures.Film.releasedIn`), never the dates. */
  def releaseCountries(block: JsValue): String =
    (block \ "results").asOpt[Seq[JsValue]].getOrElse(Nil)
      .filter(result => (result \ "release_dates").asOpt[Seq[JsValue]].exists(_.nonEmpty))
      .flatMap(result => (result \ "iso_3166_1").asOpt[String]).filter(_.length == 2).distinct.sorted.mkString

  /** The field a cut-down record (`TmdbNormalizer.minimal`) keeps of TMDB's `release_dates` block's dates: [[releases]]. */
  val Releases = "releases"

  /** The cinema releases a `release_dates` block dates — premieres, limited and wide theatrical releases, the re-releases
   *  and editions among them (TMDB files "Apocalypse Now"'s 2019 Final Cut and 2001 Redux as dated releases of its one
   *  1979 record) — each as its country, its year and whether its note names an edition ([[Release]]), run together in
   *  order ("GB1979-GB2019E"). Digital, physical and TV releases are no screening's. */
  def releases(block: JsValue): String =
    (block \ "results").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap { result =>
      (result \ "iso_3166_1").asOpt[String].filter(_.length == 2).toSeq.flatMap { country =>
        (result \ "release_dates").asOpt[Seq[JsValue]].getOrElse(Nil)
          .filter(release => (release \ "type").asOpt[Int].exists(CinemaReleaseTypes))
          .flatMap(release => (release \ "release_date").asOpt[String].flatMap(_.take(4).toIntOption).map { year =>
            Release(country, year, (release \ "note").asOpt[String].exists(note => DecorationSegments.billsAnEdition(Seq(note), Nil)))
          })
      }
    }.distinct.sortBy(r => (r.country, r.year, r.edition)).map(_.code).mkString

  /** TMDB's release types a cinema screens: 1 premiere, 2 limited theatrical, 3 theatrical. */
  private val CinemaReleaseTypes = Set(1, 2, 3)

  /** One dated cinema release of a film: its country (ISO-3166-1), its year, and whether its note names an edition. */
  final case class Release(country: String, year: Int, edition: Boolean) {
    def code: String = s"$country$year${if (edition) 'E' else '-'}"
  }
  object Release {
    /** The releases [[releases]] ran together. */
    def all(codes: String): Seq[Release] = codes.grouped(7).filter(_.length == 7).flatMap(code =>
      code.substring(2, 6).toIntOption.map(Release(code.take(2), _, code(6) == 'E'))).toSeq
  }

  /** The crew jobs a film's record reads as its directors — and so the jobs the normalized store's
   *  cut-down responses keep (`TmdbNormalizer.minimal`): the two must name the same crew. Co-directors
   *  are directors to the venues that credit them ("Vincent. Legenda oceanu": Reza Memari directs,
   *  Pavel Hrubos and Steven Majaury co-direct), so a listing naming only them is not another film. A filmed
   *  stage production's stage director is who the venues credit ("Fallen Angels by Noël Coward": Scott
   *  Ellis, while TMDB's Director is Annette Jolles, who directed the filming). */
  val DirectorJobs: Set[String] = clients.TmdbJson.Director + "Co-Director" + "Stage Director"
}
