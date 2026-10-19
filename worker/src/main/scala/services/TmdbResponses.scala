package clients

import play.api.libs.json._

import java.util.Locale

/**
 * How each TMDB response body is read: ONE parser per response, shared by [[TmdbClient]]'s own
 * calls and by the normalized identity store (`services.identity.adapters.TmdbStore`), so a stored answer is
 * by construction what the client would have made of the body. Pure — no HTTP, no key.
 */
object TmdbResponses {

  /** A title search's results, most popular first. */
  def popularFirst(body: String): Seq[TmdbClient.SearchResult] =
    rankedResults(body).sortBy(-_.popularity)

  /** A title search's results in TMDB's OWN order. */
  def rankedResults(body: String): Seq[TmdbClient.SearchResult] =
    (Json.parse(body) \ "results").asOpt[JsArray].map(movieArray).getOrElse(Seq.empty)

  /** A person search's people, those TMDB knows for directing first, in its order otherwise. */
  def personCandidates(body: String): Seq[Int] = {
    val rows = arrayAt(Json.parse(body) \ "results")
    val (directing, others) = rows.partition(r => (r \ "known_for_department").asOpt[String].contains("Directing"))
    (directing ++ others).flatMap(r => (r \ "id").asOpt[Int]).distinct
  }

  /** A change list's page: the films (or people) TMDB edited in the window, and how many pages it has. */
  def changedIds(body: String): (Seq[Int], Int) = {
    val js = Json.parse(body)
    (arrayAt(js \ "results").flatMap(r => (r \ "id").asOpt[Int]),
      (js \ "total_pages").asOpt[Int].getOrElse(1))
  }

  /** One film's edits in the window, a [[TmdbClient.MovieEdit]] per changed key. */
  def movieEdits(body: String): Seq[TmdbClient.MovieEdit] =
    arrayAt(Json.parse(body) \ "changes").flatMap { change =>
      (change \ "key").asOpt[String].map { key =>
        val items  = (change \ "items").asOpt[Seq[JsValue]].getOrElse(Nil)
        val values = items.flatMap(i => Seq(i \ "value", i \ "original_value").flatMap(_.toOption))
        TmdbClient.MovieEdit(key, items.flatMap(i => (i \ "iso_639_1").asOpt[String]).filter(_.nonEmpty).toSet,
          values.flatMap(v => (v \ "job").asOpt[String]).toSet,
          values.flatMap(v => (v \ "person_id").asOpt[Int].zip((v \ "department").asOpt[String])).distinct)
      }
    }

  /** A person's films credited in `department` (crew), once each, in TMDB's order. */
  def creditsIn(body: String, department: String): Seq[TmdbClient.SearchResult] =
    arrayAt(Json.parse(body) \ "crew")
      .filter(c => (c \ "department").asOpt[String].contains(department))
      .flatMap(searchResult(_, withOverview = false))
      .distinctBy(_.id)

  /** A find-by-external-id's films, most popular first. */
  def findMovieResults(body: String): Seq[TmdbClient.SearchResult] =
    (Json.parse(body) \ "movie_results").asOpt[JsArray]
      .map(movieArray).getOrElse(Seq.empty).sortBy(-_.popularity)

  private def movieArray(array: JsArray): Seq[TmdbClient.SearchResult] =
    array.value.toSeq.flatMap(searchResult(_, withOverview = true))

  /** One film row of a search, a find or a person's credits — those carry no overview. */
  private def searchResult(js: JsValue, withOverview: Boolean): Option[TmdbClient.SearchResult] =
    (js \ "id").asOpt[Int].map { id =>
      TmdbClient.SearchResult(
        id            = id,
        title         = (js \ "title").asOpt[String].getOrElse(""),
        originalTitle = (js \ "original_title").asOpt[String],
        releaseYear   = TmdbJson.releaseYear(js),
        popularity    = (js \ "popularity").asOpt[Double].getOrElse(0.0),
        overview      = if (withOverview) (js \ "overview").asOpt[String].filter(_.nonEmpty) else None
      )
    }

  /** The array at `at`, or none. */
  def arrayAt(at: JsLookupResult): Seq[JsValue] = at.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

  /** The `certification` for `country` (ISO-3166-1, e.g. "GB"/"DE"/"PL") from a
   *  release-dates block: the matching `results[].iso_3166_1` entry's first non-blank
   *  `release_dates[].certification`, verbatim. None when the country isn't listed or
   *  carries only blank certs. Pure (no I/O) so the country-selection is unit-tested
   *  without a live call.
   *
   *  Takes the BLOCK, not the whole response, so it reads the same either way: TMDB's
   *  `/movie/{id}/release_dates` body and the `release_dates` value appended to a detail
   *  response are the same `{id, results[]}` shape. */
  def certificationFor(js: JsValue, country: String): Option[String] =
    arrayAt(js \ "results")
      .find(r => (r \ "iso_3166_1").asOpt[String].contains(country))
      .flatMap(r => arrayAt(r \ "release_dates")
        .flatMap(rd => (rd \ "certification").asOpt[String]).map(_.trim).find(_.nonEmpty))

  /** `/movie/{id}?language=en-US&append_to_response=alternative_titles` → the English title, year
   *  and US release title the ratings resolve their URLs by. */
  def details(js: JsValue): TmdbClient.Details = {
    // Prefer the "untyped" US alt-title (an actual release title) over
    // ones tagged as "alternative spelling" / "working title" / "informal".
    val usAltTitles = arrayAt(js \ "alternative_titles" \ "titles")
      .filter(t => (t \ "iso_3166_1").asOpt[String].contains("US"))
    val usTitle = usAltTitles
      .find(t => (t \ "type").asOpt[String].forall(_.isEmpty))
      .orElse(usAltTitles.headOption)
      .flatMap(t => (t \ "title").asOpt[String].filter(_.nonEmpty))
    TmdbClient.Details(
      englishTitle = (js \ "title").asOpt[String].filter(_.nonEmpty),
      releaseYear  = TmdbJson.releaseYear(js),
      usTitle      = usTitle
    )
  }

  /** `/movie/{id}?language=<deployment>&append_to_response=credits,release_dates` → the Tmdb slot's
   *  fields, in `language`. `poster` is the best portrait poster `/images` offered, if any; without one
   *  the record's own `poster_path` stands. */
  def fullDetails(js: JsValue, language: Locale, poster: Option[String]): TmdbClient.FullDetails = {
    val crew = arrayAt(js \ "credits" \ "crew")
    val cast = arrayAt(js \ "credits" \ "cast")
    val directors = TmdbJson.crewWith(crew, TmdbJson.Director)
      .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
    // Writers too: a venue prints the writer as often as the director ("Drzewo
    // magii" is directed by Ben Gregor and written by Simon Farnaby), and the
    // verdict on a candidate asks whether the credited name is on the CREW.
    val writers = crew
      .filter(c => (c \ "job").asOpt[String].exists(TmdbClient.WritingJobs.contains))
      .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty).distinct
    val topCast = cast
      .sortBy(c => (c \ "order").asOpt[Int].getOrElse(Int.MaxValue))
      .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
      .take(MaxCastNames)
    val countries = arrayAt(js \ "production_countries")
      .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
    // Deployment-language genre names. TMDB returns the `genres` field
    // pre-localised in the requested language ("Dramat"/"Drama") —
    // no separate id→name lookup needed.
    val genres = arrayAt(js \ "genres")
      .flatMap(g => (g \ "name").asOpt[String]).filter(_.nonEmpty)
    // Per-country age rating (BBFC "12A", FSK "16", …) out of the APPENDED
    // `release_dates` block, selecting the DEPLOYMENT country's certification. A
    // cinema-scraped rating still wins (`MovieRecord.ageRating` is cinema-first);
    // this is the fallback that gives every resolved film a per-country rating even
    // where no cinema exposes one — in Poland that is every arthouse and independent
    // venue, since only the three big chains label a certificate. Best-effort: a
    // body without the block yields None. Verbatim — TMDB's own label for that country.
    val ageRating = Option(language.getCountry).filter(_.nonEmpty).flatMap(country =>
      (js \ "release_dates").toOption.flatMap(certificationFor(_, country)))
    TmdbClient.FullDetails(
      title         = (js \ "title").asOpt[String].filter(_.nonEmpty),
      originalTitle = (js \ "original_title").asOpt[String].filter(_.nonEmpty),
      synopsis      = (js \ "overview").asOpt[String].filter(_.nonEmpty),
      director      = directors,
      writers       = writers,
      cast          = topCast,
      runtimeMinutes= (js \ "runtime").asOpt[Int].filter(_ > 0),
      releaseYear   = TmdbJson.releaseYear(js),
      countries     = countries,
      genres        = genres,
      posterUrl     = poster.orElse((js \ "poster_path").asOpt[String].filter(_.nonEmpty).map(p => s"${TmdbClient.PosterBase}$p")),
      ageRating     = ageRating
    )
  }

  // Top-N cast cap for `fullDetails.cast`. TMDB's `cast` is the whole role
  // list; keeping the top 5 matches the length cinemas typically ship.
  private val MaxCastNames = 5

  /** `/movie/{id}/images` → its poster variants, each with its language, shape and votes. */
  def posters(body: String): Seq[TmdbClient.PosterImage] =
    arrayAt(Json.parse(body) \ "posters")
      .flatMap { js =>
        (js \ "file_path").asOpt[String].filter(_.nonEmpty).map { fp =>
          TmdbClient.PosterImage(
            filePath    = fp,
            language    = (js \ "iso_639_1").asOpt[String].filter(_.nonEmpty),
            aspectRatio = (js \ "aspect_ratio").asOpt[Double].getOrElse(0.0),
            voteAverage = (js \ "vote_average").asOpt[Double].getOrElse(0.0),
            width       = (js \ "width").asOpt[Int].getOrElse(0),
            voteCount   = (js \ "vote_count").asOpt[Int].getOrElse(0)
          )
        }
      }

  /** Pick the best *portrait* poster URL from a `/images` poster set,
   *  preferring artwork tagged with the DEPLOYMENT's language. Drops
   *  landscape/square variants (aspect ratio above `TmdbClient.MaxPortraitAspectRatio`),
   *  then orders by (own language first, highest community vote, then
   *  resolution) and returns the winner as a w500 CDN URL. None when nothing
   *  portrait survives — the caller falls back to `poster_path`.
   *
   *  `language` is the bare subtag ("pl", "de", "en") matching `/images`'
   *  `iso_639_1`. It used to be hardcoded "pl", which was inert only as long as
   *  `posters` was already scoped to `<lang>,null`: any set that also carried
   *  Polish artwork (a widened `include_image_language`, a default-language
   *  client in a spec) handed a German deployment the Polish poster. */
  def bestPortraitPosterUrl(posters: Seq[TmdbClient.PosterImage], language: String): Option[String] =
    posters
      .filter(p => p.aspectRatio > 0 && p.aspectRatio <= TmdbClient.MaxPortraitAspectRatio)
      .sortBy(p => (if (p.language.contains(language)) 0 else 1, -p.voteAverage, -p.width))
      .headOption
      .map(p => s"${TmdbClient.PosterBase}${p.filePath}")
}
