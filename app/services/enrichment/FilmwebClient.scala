package services.enrichment

import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Looks up Filmweb metadata (canonical page URL + 1–10 user rating) for a Polish
 * film title. Filmweb's public site is a SPA, but its mobile/JSON API is
 * reachable without auth at:
 *   - GET /api/v1/live/search?query=...   → list of (id, type, matchedTitle)
 *   - GET /api/v1/film/{id}/info          → { title, originalTitle, year }
 *   - GET /api/v1/film/{id}/preview       → { directors:[…], mainCast:[…] }
 *   - GET /api/v1/film/{id}/rating        → { rate, count, … }
 *
 * Matching is intentionally conservative (mirrors `MetacriticClient` /
 * `RottenTomatoesClient`): Filmweb's `live/search` is fuzzy and routinely
 * returns hits whose canonical title bears no resemblance to the query — id
 * 838929 is `matchedTitle`'d for "Wartość sentymentalna" but its /info `title`
 * is "It's About Time" (2015). Picking such a hit and pasting its URL silently
 * mis-labels the film. We:
 *
 *   1. Fetch `/info` for each search candidate.
 *   2. Accept a candidate only when its `title` OR `originalTitle` matches
 *      the query exactly (case-insensitive, trimmed) OR as a modifier-suffix
 *      ("Title - Re-Release", "Title: Restored").
 *   3. When the caller supplies directors AND the candidate has directors
 *      (fetched via `/preview`), require at least one overlap — disambiguates
 *      same-titled films across years (multiple "Belle"s, multiple "Loop"s).
 *   4. Year is a tie-breaker among accepted candidates, not a primary
 *      criterion. Returns None when no candidate clears the bar — callers
 *      MUST treat None as "no canonical URL" and never substitute a search
 *      URL (CLAUDE.md: "Never persist Metacritic or Rotten Tomatoes search
 *      URLs"; the same rationale applies to Filmweb).
 */
class FilmwebClient(http: HttpFetch = new RealHttpFetch()) {
  import FilmwebClient._

  /** Best-effort lookup. Returns None when no candidate clears the title +
   *  optional director bar. `fallback` is tried only after the primary query
   *  fails — typically TMDB's `originalTitle` or `englishTitle` for non-Polish
   *  films whose cinema-reported title doesn't surface a Filmweb hit.
   *  `directors` is matched against `/preview` per candidate; pass the empty
   *  set to skip director verification (and the per-candidate /preview round-
   *  trip that goes with it).
   */
  def lookup(
    title:     String,
    year:      Option[Int]    = None,
    fallback:  Option[String] = None,
    directors: Set[String]    = Set.empty
  ): Option[FilmwebInfo] = {
    val primary = lookupWithQuery(title, year, directors)
    val effectiveFallback = fallback.filterNot(_.equalsIgnoreCase(title))
    primary.orElse(effectiveFallback.flatMap(t => lookupWithQuery(t, year, directors)))
  }

  private def lookupWithQuery(query: String, year: Option[Int], directors: Set[String]): Option[FilmwebInfo] =
    Try {
      val hits = search(query).take(MaxCandidates)
      // Step 1 (cheap): /info per candidate → title acceptance bar.
      val infoCandidates = hits.flatMap(id =>
        info(id).map(i => Candidate(id, i.title, i.originalTitle, i.year, Set.empty))
      )
      // Step 2 (only when caller wants director verification): /preview per
      // candidate that already passed the title bar. Skipping unaccepted
      // candidates saves the round-trip on Filmweb's worst search-noise hits.
      val candidates =
        if (directors.isEmpty) infoCandidates
        else {
          val titleAccepted = infoCandidates.filter(c => matchesByTitle(c, query))
          titleAccepted.map(c => c.copy(directors = preview(c.id).map(_.directors).getOrElse(Set.empty)))
        }
      pickBest(candidates, query, year, directors).flatMap { c =>
        val url = canonicalUrl(c.id, c.title, c.year)
        Some(FilmwebInfo(url, rating(c.id)))
      }
    }.toOption.flatten

  /** Returns film ids in the order Filmweb ranks them. */
  def search(title: String): Seq[Int] =
    parseSearch(http.get(s"$ApiBase/live/search?query=${urlEncode(title)}"))

  def info(id: Int): Option[FilmInfo] =
    Try(parseInfo(http.get(s"$ApiBase/film/$id/info"))).toOption.flatten

  def preview(id: Int): Option[FilmPreview] =
    Try(parsePreview(http.get(s"$ApiBase/film/$id/preview"))).toOption

  def rating(id: Int): Option[Double] =
    Try(parseRating(http.get(s"$ApiBase/film/$id/rating"))).toOption.flatten

  /** Refresh just the rating for a stored canonical Filmweb URL. The URL ends
   *  in `-{id}` (or `-{id}/`); we parse the trailing id and hit only the
   *  rating endpoint — no search + info round-trips. Used by the hourly
   *  `FilmwebRatings` refresh, which has already paid for the URL discovery
   *  on first enrichment. Returns None when the URL doesn't look like a
   *  Filmweb canonical or the rating fetch fails. */
  def ratingFor(url: String): Option[Double] =
    FilmwebClient.IdFromUrl.findFirstMatchIn(url)
      .flatMap(m => Try(m.group(1).toInt).toOption)
      .flatMap(rating)

  def parseSearch(body: String): Seq[Int] =
    (Json.parse(body) \ "searchHits").asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap { js =>
      val isFilm = (js \ "type").asOpt[String].contains("film")
      val id     = (js \ "id").asOpt[Int]
      if (isFilm) id else None
    }.toSeq

  def parseInfo(body: String): Option[FilmInfo] = {
    val json = Json.parse(body)
    (json \ "title").asOpt[String].map { t =>
      FilmInfo(
        title         = t,
        originalTitle = (json \ "originalTitle").asOpt[String].filter(_.nonEmpty),
        year          = (json \ "year").asOpt[Int]
      )
    }
  }

  def parsePreview(body: String): FilmPreview = {
    val json = Json.parse(body)
    val directors = (json \ "directors").asOpt[JsArray].map(_.value).getOrElse(Nil)
      .flatMap(j => (j \ "name").asOpt[String].filter(_.nonEmpty))
      .toSet
    FilmPreview(directors)
  }

  def parseRating(body: String): Option[Double] =
    (Json.parse(body) \ "rate").asOpt[Double]

  /** Pick the best candidate from /info (+ optional /preview) data.
   *
   *  Title bar: candidate accepted iff its `title` or `originalTitle`
   *  exactly equals the query (case-insensitive, trimmed) OR is a
   *  modifier-suffix start (`Title - Re-Release`).
   *
   *  Director bar: applied only when BOTH sides have data — when the caller
   *  passed a non-empty `directors` set AND Filmweb has directors for the
   *  candidate. Otherwise we have nothing to compare and don't penalise.
   *  Names are compared diacritic-stripped + case-folded; substring match in
   *  either direction passes so the cinema reporting "M. Szumowska" still
   *  hits "Małgorzata Szumowska".
   *
   *  Year breaks ties among accepted candidates.
   */
  def pickBest(
    candidates: Seq[Candidate],
    query:      String,
    year:       Option[Int],
    directors:  Set[String]
  ): Option[Candidate] = {
    if (candidates.isEmpty || query.trim.isEmpty) return None
    val titleAccepted = candidates.filter(c => matchesByTitle(c, query))
    val directorAccepted = titleAccepted.filter(c => matchesByDirector(c, directors))
    directorAccepted
      .sortBy(c => year.flatMap(y => c.year.map(yy => math.abs(yy - y))).getOrElse(Int.MaxValue))
      .headOption
  }

  private[enrichment] def matchesByTitle(c: Candidate, query: String): Boolean = {
    val q = query.toLowerCase.trim
    val titles = (c.title +: c.originalTitle.toSeq).map(_.toLowerCase.trim)
    titles.exists(_ == q) || titles.exists(t => MetacriticClient.isModifierSuffix(t, q))
  }

  private[enrichment] def matchesByDirector(c: Candidate, directors: Set[String]): Boolean = {
    if (directors.isEmpty || c.directors.isEmpty) true
    else {
      val callerNorms = directors.map(deburr)
      val fwNorms     = c.directors.map(deburr)
      callerNorms.exists(d => fwNorms.exists(f => d == f || d.contains(f) || f.contains(d)))
    }
  }
}

object FilmwebClient {
  private val ApiBase        = "https://www.filmweb.pl/api/v1"
  private val MaxCandidates  = 5
  // Canonical Filmweb URLs end in `-{id}` (optionally trailing slash):
  //   https://www.filmweb.pl/film/Title+Words-2024-12345
  //   https://www.filmweb.pl/film/Title-12345/
  private val IdFromUrl      = "-(\\d+)/?$".r

  /** /info response — canonical title + optional originalTitle + year. */
  case class FilmInfo(title: String, originalTitle: Option[String], year: Option[Int])

  /** /preview response — director names. Other fields (cast, plot, duration,
   *  countries) are present in the JSON but unused by the matcher; trim the
   *  case class if a future caller needs them. */
  case class FilmPreview(directors: Set[String])

  /** Resolved Filmweb metadata for a film — canonical URL + optional 1–10
   *  user rating. */
  case class FilmwebInfo(url: String, rating: Option[Double])

  /** One candidate distilled from /info + (optionally) /preview. `directors`
   *  is empty when the caller didn't ask for verification — we then skip the
   *  /preview HTTP entirely. */
  case class Candidate(
    id:            Int,
    title:         String,
    originalTitle: Option[String],
    year:          Option[Int],
    directors:     Set[String]
  )

  /**
   * Build the canonical page URL the way Filmweb encodes it. The site replaces
   * spaces with `+` and percent-encodes everything else.
   */
  def canonicalUrl(id: Int, title: String, year: Option[Int]): String = {
    val slug = URLEncoder.encode(title, StandardCharsets.UTF_8).replace("%20", "+")
    val y    = year.map(_.toString).getOrElse("")
    s"https://www.filmweb.pl/film/$slug-$y-$id"
  }

  private def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  /** Lower-case + strip diacritics for fuzzy director comparison. Cinemas
   *  sometimes drop diacritics ("Malgorzata Szumowska"); Filmweb keeps them
   *  ("Małgorzata Szumowska"). Normalising both sides catches that case. */
  private def deburr(s: String): String = TextNormalization.deburr(s).toLowerCase.trim
}
