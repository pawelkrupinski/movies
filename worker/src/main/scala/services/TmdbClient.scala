package clients

import play.api.libs.json._
import tools.{Env, HttpFetch}

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
 * `TMDB_API_KEY` can be either:
 *   - legacy v3 API key (32-char hex), passed as `?api_key=…` query param, or
 *   - v4 application bearer token (JWT-shaped), passed as
 *     `Authorization: Bearer …`.
 * The client sends BOTH on every request. TMDB picks whichever is valid for
 * the key it actually receives, so prod (currently a v4 token) and CI
 * (currently a v3 key) can both run unchanged against the same code path
 * without the deploy pipeline knowing which one is configured. The detector
 * is purely additive — a v3 key sent in `Authorization: Bearer` is ignored
 * by TMDB, and vice-versa.
 */
class TmdbClient(http: HttpFetch, apiKey: => Option[String] = TmdbClient.ApiKey) {

  import TmdbClient.{ApiBase, urlEncode}

  /** Auth header for every request. Built lazily off `apiKey` so a missing
   *  key returns None and the calling method short-circuits via flatMap
   *  without ever hitting the network. */
  private def authHeader: Option[Map[String, String]] =
    apiKey.map(k => Map("Authorization" -> s"Bearer $k"))

  /** Query-string suffix carrying the legacy v3 `api_key=` parameter.
   *  Empty when no key is configured (the calling method short-circuits
   *  via `authHeader.flatMap` before reaching here, so this is only
   *  evaluated alongside a real key). Appended to every URL that already
   *  has at least one query parameter (`?language=…&api_key=…`); for
   *  query-less URLs the caller switches `?` for the leading `&` via
   *  `apiKeyParam(separator)`. */
  private def apiKeyParam(separator: String): String =
    apiKey.map(k => s"${separator}api_key=$k").getOrElse("")

  /**
   * Resolve a Polish title (+ optional year) to its best-match TMDB record.
   * First tries a year-restricted search; if that's empty (common — the year a
   * cinema reports as "release year" can be the production year while TMDB
   * stores the theatrical date, off by 1 in either direction), falls back to a
   * year-less search and picks the candidate whose own year is closest to the
   * requested one. Ties broken by popularity.
   */
  def search(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] = authHeader.flatMap { auth =>
    def searchOnce(yearParam: Option[Int]): Seq[TmdbClient.SearchResult] = {
      val yp = yearParam.map(y => s"&year=$y&primary_release_year=$y").getOrElse("")
      val url = s"$ApiBase/search/movie?language=pl-PL&include_adult=false&query=${urlEncode(title)}$yp${apiKeyParam("&")}"
      parseSearchResults(http.get(url, auth))
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
  def imdbId(tmdbId: Int): Option[String] = authHeader.flatMap { auth =>
    val body = http.get(s"$ApiBase/movie/$tmdbId/external_ids${apiKeyParam("?")}", auth)
    (Json.parse(body) \ "imdb_id").asOpt[String].filter(_.startsWith("tt"))
  }

  /** Reverse lookup: find a TMDB movie record by its IMDB id. Used when the
   *  Polish exhibitor title doesn't resolve via TMDB's search (no Polish
   *  translation registered for the film) and a manual override pins the
   *  right IMDb id — we still want TMDB's `tmdbId` and `originalTitle` for
   *  the rest of the enrichment row.
   */
  def findByImdbId(imdbId: String): Option[TmdbClient.SearchResult] = authHeader.flatMap { auth =>
    val body = http.get(s"$ApiBase/find/$imdbId?external_source=imdb_id&language=pl-PL${apiKeyParam("&")}", auth)
    parseFindMovieResults(body).headOption
  }

  /** TMDB's production-language original title for a known movie. Used as a
   *  more accurate source than OMDb's English-localised `Title` (which can
   *  drift for non-English films). Hits `/movie/{id}` with no language so the
   *  field comes back in the production language.
   */
  def originalTitle(tmdbId: Int): Option[String] = authHeader.flatMap { auth =>
    val body = http.get(s"$ApiBase/movie/$tmdbId${apiKeyParam("?")}", auth)
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
  def details(tmdbId: Int): Option[TmdbClient.Details] = authHeader.flatMap { auth =>
    Try(http.get(s"$ApiBase/movie/$tmdbId?language=en-US&append_to_response=alternative_titles${apiKeyParam("&")}", auth))
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
  def directorsFor(tmdbId: Int): Set[String] = authHeader.map { auth =>
    Try {
      val body = http.get(s"$ApiBase/movie/$tmdbId/credits${apiKeyParam("?")}", auth)
      (Json.parse(body) \ "crew").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        .filter(c => (c \ "job").asOpt[String].contains("Director"))
        .flatMap { c =>
          // Include both the localised `name` and the native-script `original_name`
          // so that cinemas reporting a director in their native script (e.g. "张钢"
          // for a Chinese director TMDB lists as "Gang Zhang") still match.
          Seq("name", "original_name").flatMap(f => (c \ f).asOpt[String])
        }
        .filter(_.nonEmpty)
        .toSet
    }.getOrElse(Set.empty)
  }.getOrElse(Set.empty)

  /** One TMDB `/movie/{id}?language=pl-PL&append_to_response=credits` call
   *  returning everything the TMDB enrichment stage needs to fill a
   *  `SourceData(Tmdb)` slot:
   *    - Polish title + production-language original title
   *    - Polish-language synopsis (`overview`)
   *    - Director(s) + top-billed cast
   *    - Year (`release_date`) + runtime
   *    - Production countries (raw names — caller canonicalises)
   *    - Poster URL (constructed from `poster_path`)
   *
   *  Returns None on network failure / unknown id — callers fall back to
   *  whatever the previous slot held. Designed as a single round-trip so
   *  the TMDB stage doesn't grow into N+1 calls per resolve. */
  def fullDetails(tmdbId: Int): Option[TmdbClient.FullDetails] = authHeader.flatMap { auth =>
    Try(http.get(s"$ApiBase/movie/$tmdbId?language=pl-PL&append_to_response=credits${apiKeyParam("&")}", auth))
      .toOption.map { body =>
        val js   = Json.parse(body)
        val crew = (js \ "credits" \ "crew").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        val cast = (js \ "credits" \ "cast").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
        val directors = crew
          .filter(c => (c \ "job").asOpt[String].contains("Director"))
          .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
        val topCast = cast
          .sortBy(c => (c \ "order").asOpt[Int].getOrElse(Int.MaxValue))
          .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
          .take(TmdbClient.MaxCastNames)
        val countries = (js \ "production_countries").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
          .flatMap(c => (c \ "name").asOpt[String]).filter(_.nonEmpty)
        // Polish-language genre names. TMDB returns the `genres` field
        // pre-localised in the requested language ("Dramat", "Sci-Fi") —
        // no separate id→name lookup needed.
        val genres = (js \ "genres").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
          .flatMap(g => (g \ "name").asOpt[String]).filter(_.nonEmpty)
        TmdbClient.FullDetails(
          title         = (js \ "title").asOpt[String].filter(_.nonEmpty),
          originalTitle = (js \ "original_title").asOpt[String].filter(_.nonEmpty),
          synopsis      = (js \ "overview").asOpt[String].filter(_.nonEmpty),
          director      = directors,
          cast          = topCast,
          runtimeMinutes= (js \ "runtime").asOpt[Int].filter(_ > 0),
          releaseYear   = (js \ "release_date").asOpt[String].filter(_.length >= 4).flatMap(s => Try(s.take(4).toInt).toOption),
          countries     = countries,
          genres        = genres,
          posterUrl     = (js \ "poster_path").asOpt[String].filter(_.nonEmpty).map(p => s"${TmdbClient.PosterBase}$p")
        )
      }
  }

  /** TMDB person id for a name search, or None when the search returns no
   *  Directing-known hit. Picks the highest-popularity match whose
   *  `known_for_department` is "Directing" — keeps us from matching an actor
   *  who happens to share a name with a director. */
  def findPerson(name: String): Option[Int] = authHeader.flatMap { auth =>
    Try {
      val body = http.get(s"$ApiBase/search/person?query=${urlEncode(name)}${apiKeyParam("&")}", auth)
      val rows = (Json.parse(body) \ "results").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      rows.find(r => (r \ "known_for_department").asOpt[String].contains("Directing"))
        .orElse(rows.headOption)
        .flatMap(r => (r \ "id").asOpt[Int])
    }.toOption.flatten
  }

  /** A person's movies as a director — the films they're credited for in the
   *  Directing department. Returns the same `SearchResult` shape so the
   *  caller can reuse picking / sorting logic. */
  def personDirectorCredits(personId: Int): Seq[TmdbClient.SearchResult] = authHeader.map { auth =>
    Try {
      val body = http.get(s"$ApiBase/person/$personId/movie_credits?language=pl-PL${apiKeyParam("&")}", auth)
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
    (Json.parse(body) \ "results").asOpt[JsArray].map(decodeMovieArray).getOrElse(Seq.empty)

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
  // TMDB's CDN-served poster path. w500 is the largest "good" size for our
  // cards (next step up is "original" which can be 2000+ px and isn't worth
  // the bytes). Matches the size Multikino's own posters ship at.
  private val PosterBase = "https://image.tmdb.org/t/p/w500"
  // Top-N cast cap for `fullDetails.cast`. TMDB's `cast` is the whole role
  // list; keeping the top 5 matches the length cinemas typically ship.
  private val MaxCastNames = 5

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

  /** Full TMDB record consumed by the TMDB enrichment stage. Returned by
   *  `fullDetails(tmdbId)` in one HTTP round-trip; the MovieService maps
   *  this into a `SourceData(Tmdb)` slot on the row. Polish-localised
   *  (Polish title + Polish synopsis), original-language `originalTitle`,
   *  and the canonicalised production countries downstream. */
  case class FullDetails(
    title:          Option[String],
    originalTitle:  Option[String],
    synopsis:       Option[String],
    director:       Seq[String],
    cast:           Seq[String],
    runtimeMinutes: Option[Int],
    releaseYear:    Option[Int],
    countries:      Seq[String],
    genres:         Seq[String],
    posterUrl:      Option[String]
  )

  private[clients] def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
