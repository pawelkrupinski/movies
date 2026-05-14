package clients

import play.api.libs.json._
import tools.{Env, HttpFetch, RealHttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Bridges Polish film titles to IMDB ids via The Movie DB.
 *
 * TMDB has multilingual title indexing — `/search/movie?query=…&language=pl-PL`
 * returns the same movies you'd see on imdb.com but matched by their Polish
 * release title. From a hit we follow `/movie/{id}/external_ids` to read off
 * the IMDB id (e.g. `tt15239678`), which is the stable cross-system key.
 *
 * Requires TMDB_API_KEY (free, register at https://www.themoviedb.org/settings/api).
 */
class TmdbClient(http: HttpFetch = new RealHttpFetch(), apiKey: => Option[String] = TmdbClient.ApiKey) {

  import TmdbClient.{ApiBase, urlEncode}

  /**
   * Resolve a Polish title (+ optional year) to its best-match TMDB record.
   * First tries a year-restricted search; if that's empty (common — the year a
   * cinema reports as "release year" can be the production year while TMDB
   * stores the theatrical date, off by 1 in either direction), falls back to a
   * year-less search and picks the candidate whose own year is closest to the
   * requested one. Ties broken by popularity.
   */
  def search(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] = apiKey.flatMap { key =>
    def searchOnce(yearParam: Option[Int]): Seq[TmdbClient.SearchResult] = {
      val yp = yearParam.map(y => s"&year=$y&primary_release_year=$y").getOrElse("")
      val url = s"$ApiBase/search/movie?api_key=$key&language=pl-PL&include_adult=false&query=${urlEncode(title)}$yp"
      parseSearchResults(http.get(url))
    }

    // Try year-restricted first (more precise when TMDB has the film for that
    // year), then year-less fallback. pickBest runs at BOTH levels so we don't
    // silently grab the most popular film that merely contains the query word:
    //   - "Camper" (year=None) → would otherwise pick "Sleepaway Camper"
    //   - "Odlot"  (year=None) → would otherwise pick Pixar's "Up" (pop 21)
    //   - "Rocznica" (year=None) → would otherwise pick "Harry Potter 20th
    //     Anniversary: Return to Hogwarts"
    val yearScoped = if (year.isDefined) pickBest(searchOnce(year), title, year) else None
    yearScoped.orElse(pickBest(searchOnce(None), title, year))
  }

  /**
   * From a set of TMDB hits, pick the best match for the query.
   *   1. Exact title matches (Polish OR original) win over everything else.
   *   2. Among exact matches (or when none exist), prefer the result closest
   *      to the requested year.
   *   3. With no year and no tie-break needed, the parseSearchResults
   *      insertion order (popularity-descending) wins.
   */
  private[clients] def pickBest(
    results: Seq[TmdbClient.SearchResult],
    title:   String,
    year:    Option[Int]
  ): Option[TmdbClient.SearchResult] = {
    if (results.isEmpty) None
    else {
      val cleanQuery = title.toLowerCase.trim
      val exactMatches = results.filter(r =>
        r.title.toLowerCase.trim == cleanQuery ||
          r.originalTitle.exists(_.toLowerCase.trim == cleanQuery)
      )
      val candidates = if (exactMatches.nonEmpty) exactMatches else results
      candidates
        .sortBy(r => year.flatMap(y => r.releaseYear.map(ry => math.abs(ry - y))).getOrElse(Int.MaxValue))
        .headOption
    }
  }

  /** Look up the IMDB id of a TMDB movie. Returns None when TMDB knows the
   *  movie but hasn't been told its IMDB cross-reference yet (rare for theatrical
   *  releases, common for film festival items).
   */
  def imdbId(tmdbId: Int): Option[String] = apiKey.flatMap { key =>
    val body = http.get(s"$ApiBase/movie/$tmdbId/external_ids?api_key=$key")
    (Json.parse(body) \ "imdb_id").asOpt[String].filter(_.startsWith("tt"))
  }

  /** Reverse lookup: find a TMDB movie record by its IMDB id. Used when the
   *  Polish exhibitor title doesn't resolve via TMDB's search (no Polish
   *  translation registered for the film) and a manual override pins the
   *  right IMDb id — we still want TMDB's `tmdbId` and `originalTitle` for
   *  the rest of the enrichment row.
   */
  def findByImdbId(imdbId: String): Option[TmdbClient.SearchResult] = apiKey.flatMap { key =>
    val body = http.get(s"$ApiBase/find/$imdbId?api_key=$key&external_source=imdb_id&language=pl-PL")
    parseFindMovieResults(body).headOption
  }

  /** TMDB's production-language original title for a known movie. Used as a
   *  more accurate source than OMDb's English-localised `Title` (which can
   *  drift for non-English films). Hits `/movie/{id}` with no language so the
   *  field comes back in the production language.
   */
  def originalTitle(tmdbId: Int): Option[String] = apiKey.flatMap { key =>
    val body = http.get(s"$ApiBase/movie/$tmdbId?api_key=$key")
    (Json.parse(body) \ "original_title").asOpt[String].filter(_.nonEmpty)
  }

  /** TMDB's English release title for a movie. `/movie/{id}?language=en-US`
   *  returns the localized `title` for the requested language, which for
   *  non-English films is the English release title (e.g. tmdbid 1290432
   *  returns "A Private Life" in en-US, "Vie privée" as original_title).
   *  Used as a fallback for MC/RT URL probes because both sites index foreign
   *  films under their English release title, not the production-language one.
   */
  def englishTitle(tmdbId: Int): Option[String] = details(tmdbId).flatMap(_.englishTitle)

  /** One TMDB `/movie/{id}?language=en-US&append_to_response=alternative_titles`
   *  call returning everything the ratings classes need to do URL resolution:
   *    - `englishTitle`: en-US localized `title` (MC/RT slug fallback for
   *      non-English films).
   *    - `releaseYear`: production release year (RT's year-suffix
   *      disambiguation, MC's search-scrape year tie-break).
   *    - `usTitle`: the US entry from `/alternative_titles` (MC/RT slug
   *      fallback for UK/US release-title divergence — e.g. HP1's
   *      "Philosopher's" → "Sorcerer's" Stone, where TMDB keeps the British
   *      title in `title` even for en-US).
   *  Single HTTP for all three fields via `append_to_response`. */
  def details(tmdbId: Int): Option[TmdbClient.Details] = apiKey.flatMap { key =>
    Try(http.get(s"$ApiBase/movie/$tmdbId?api_key=$key&language=en-US&append_to_response=alternative_titles"))
      .toOption.map { body =>
        val js = Json.parse(body)
        // Prefer the "untyped" US alt-title (an actual release title) over
        // ones tagged as "alternative spelling" / "working title" / "informal".
        val usAltTitles = (js \ "alternative_titles" \ "titles").asOpt[JsArray]
          .map(_.value.toSeq).getOrElse(Seq.empty)
          .filter(t => (t \ "iso_3166_1").asOpt[String].contains("US"))
        val usTitle = usAltTitles
          .find(t => (t \ "type").asOpt[String].forall(_.isEmpty))
          .orElse(usAltTitles.headOption)
          .flatMap(t => (t \ "title").asOpt[String].filter(_.nonEmpty))
        TmdbClient.Details(
          englishTitle = (js \ "title").asOpt[String].filter(_.nonEmpty),
          releaseYear  = (js \ "release_date").asOpt[String].filter(_.length >= 4).flatMap(s => Try(s.take(4).toInt).toOption),
          usTitle      = usTitle
        )
      }
  }

  /** Director name(s) credited on a TMDB movie. Empty when TMDB has no
   *  credits or the call fails. Used to verify a title-search candidate
   *  against the cinema-reported director — when a film's title and a
   *  cinema's title happen to collide (Polish "Niedźwiedzica" matches both
   *  Grizzly Falls 1999 and the 2026 Helgestad doc) the directors disagree. */
  def directorsFor(tmdbId: Int): Set[String] = apiKey.map { key =>
    Try {
      val body = http.get(s"$ApiBase/movie/$tmdbId/credits?api_key=$key")
      (Json.parse(body) \ "crew").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        .filter(c => (c \ "job").asOpt[String].contains("Director"))
        .flatMap(c => (c \ "name").asOpt[String])
        .filter(_.nonEmpty)
        .toSet
    }.getOrElse(Set.empty)
  }.getOrElse(Set.empty)

  /** TMDB person id for a name search, or None when the search returns no
   *  Directing-known hit. Picks the highest-popularity match whose
   *  `known_for_department` is "Directing" — keeps us from matching an actor
   *  who happens to share a name with a director. */
  def findPerson(name: String): Option[Int] = apiKey.flatMap { key =>
    Try {
      val body = http.get(s"$ApiBase/search/person?api_key=$key&query=${urlEncode(name)}")
      val rows = (Json.parse(body) \ "results").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      rows.find(r => (r \ "known_for_department").asOpt[String].contains("Directing"))
        .orElse(rows.headOption)
        .flatMap(r => (r \ "id").asOpt[Int])
    }.toOption.flatten
  }

  /** A person's movies as a director — the films they're credited for in the
   *  Directing department. Returns the same `SearchResult` shape so the
   *  caller can reuse picking / sorting logic. */
  def personDirectorCredits(personId: Int): Seq[TmdbClient.SearchResult] = apiKey.map { key =>
    Try {
      val body = http.get(s"$ApiBase/person/$personId/movie_credits?api_key=$key&language=pl-PL")
      (Json.parse(body) \ "crew").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        .filter(c => (c \ "department").asOpt[String].contains("Directing"))
        .flatMap { js =>
          for {
            id <- (js \ "id").asOpt[Int]
          } yield TmdbClient.SearchResult(
            id            = id,
            title         = (js \ "title").asOpt[String].getOrElse(""),
            originalTitle = (js \ "original_title").asOpt[String],
            releaseYear   = (js \ "release_date").asOpt[String].filter(_.length >= 4).flatMap(s => Try(s.take(4).toInt).toOption),
            popularity    = (js \ "popularity").asOpt[Double].getOrElse(0.0)
          )
        }
        .distinctBy(_.id)
    }.toOption.getOrElse(Seq.empty)
  }.getOrElse(Seq.empty)

  private[clients] def parseSearchResults(body: String): Seq[TmdbClient.SearchResult] =
    decodeMovieArray((Json.parse(body) \ "results").as[JsArray])

  // /find/{external_id} returns matches under "movie_results" in the same row
  // shape as /search/movie's "results". Both decoders share this body.
  private[clients] def parseFindMovieResults(body: String): Seq[TmdbClient.SearchResult] =
    (Json.parse(body) \ "movie_results").asOpt[JsArray]
      .map(decodeMovieArray).getOrElse(Seq.empty)

  private def decodeMovieArray(arr: JsArray): Seq[TmdbClient.SearchResult] =
    arr.value.flatMap { js =>
      for {
        id <- (js \ "id").asOpt[Int]
      } yield TmdbClient.SearchResult(
        id            = id,
        title         = (js \ "title").asOpt[String].getOrElse(""),
        originalTitle = (js \ "original_title").asOpt[String],
        releaseYear   = (js \ "release_date").asOpt[String].filter(_.length >= 4).map(_.take(4).toInt),
        popularity    = (js \ "popularity").asOpt[Double].getOrElse(0.0)
      )
    }.sortBy(-_.popularity).toSeq
}

object TmdbClient {
  private val ApiBase = "https://api.themoviedb.org/3"
  val ApiKey: Option[String] = Env.get("TMDB_API_KEY")

  case class SearchResult(
    id:            Int,
    title:         String,
    originalTitle: Option[String],
    releaseYear:   Option[Int],
    popularity:    Double
  )

  /** Slim shape returned by `details(tmdbId)` — just the fields the ratings
   *  classes need for MC/RT URL resolution.
   *
   *  `usTitle` is TMDB's US-localised alternative title, used when the
   *  en-US `title` field is actually the British release title (HP series
   *  is the canonical case). None when TMDB has no US-tagged alternative. */
  case class Details(
    englishTitle: Option[String],
    releaseYear:  Option[Int],
    usTitle:      Option[String] = None
  )

  private[clients] def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
