package services.enrichment

import java.util.Locale

import play.api.libs.json._
import services.movies.SamePerson
import services.resolution.{TitleMatch, YearWindow}
import tools.{HttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * Feature-gated OMDb (omdbapi.com) client that recovers an IDENTIFIER, not a
 * rating value: an IMDb id, resolved with the same rigor as TMDB resolution.
 * [[ImdbRatings]] then fetches the score FROM it, so OMDb never writes a rating
 * value — one canonical writer per value. Rating-site links are their rating
 * tasks' own ([[RottenTomatoesRatings]], [[MetascoreRatings]]).
 *
 * IMDb-id resolution ([[findImdbId]]) mirrors `TmdbClient` / `ImdbIdResolver`:
 *   - `type=movie` (never a series), year-scoped when a year is known;
 *   - a candidate is ACCEPTED only when CORROBORATED — exact (normalised) title,
 *     OR an overlapping director, OR a matching year with a containing title —
 *     and never when its director or year CONTRADICTS ours;
 *   - a director-walk backstop (`?s=` search → per-candidate director check)
 *     when the single best match abstains, accepting only the LONE director
 *     match (never guessing among several).
 * This refuses the loose same-title / wrong-year / series matches a bare title
 * lookup would bind.
 *
 * Feature gate: the `OMDB_API_KEY` secret. Unset → every method short-circuits
 * to `None` WITHOUT any HTTP call (the TmdbClient pattern).
 */
// `apiKey` is OMDB_API_KEY — handed in: the worker wiring passes its resolved value, a spec its
// stub. Unset turns the backfill off.
class OMDbClient(http: HttpFetch, apiKey: Option[settings.OmdbApiKey]) {
  import OMDbClient._

  /** Resolve an IMDb id for a film. Tries each title spelling in turn (pass the
   *  original/English title first — OMDb is an English DB). None when the key is
   *  unset (no HTTP) or OMDb answered and nothing is corroborated. A call that
   *  fails — refused, an exhausted daily quota (OMDb answers that with a 401), a
   *  body that is not JSON — THROWS: it is no answer, so it must not read as
   *  "OMDb has no such film" and back the film off for days. */
  def findImdbId(titles: Seq[String], year: Option[Int], directors: Set[String]): Option[String] =
    apiKey.map(_.value).flatMap { key =>
      titles.map(_.trim).filter(_.nonEmpty).distinct.iterator
        .flatMap(t => resolveTitle(t, year, directors, key).iterator)
        .nextOption()
    }

  private def resolveTitle(title: String, year: Option[Int], directors: Set[String], key: String): Option[String] = {
    // Cheap path: OMDb's single best MOVIE match, accepted only if corroborated.
    val best = byTitle(title, year, key).filter(c => corroborated(c, title, year, directors)).map(_.imdbId)
    // Director-walk backstop: when the cheap path abstains and we know a director,
    // search candidates and accept the LONE director match (never guess).
    best.orElse(if (directors.nonEmpty) directorWalk(title, year, directors, key) else None)
  }

  /** OMDb's single best `type=movie` match (`?t=`), with its director credits. */
  private def byTitle(title: String, year: Option[Int], key: String): Option[Candidate] = {
    candidateFrom(Json.parse(http.get(titleUrl(title, year, key))))
  }

  private def directorWalk(title: String, year: Option[Int], directors: Set[String], key: String): Option[String] = {
    val js   = Json.parse(http.get(searchUrl(title, year, key)))
    val hits = (js \ "Search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(h => (h \ "imdbID").asOpt[String].filter(_.startsWith("tt"))).distinct.take(MaxCandidates)
    val matches = hits
      .flatMap(id => detail(id, key))
      .filter(c => directorsOverlap(directors, c.directors) && !YearWindow.contradicts(year, c.year, YearTolerance))
      .map(_.imdbId).distinct
    // Accept ONLY when exactly one candidate's director corroborates — same
    // "never guess among several" rule as ImdbClient.disambiguateByDirector.
    if (matches.sizeIs == 1) matches.headOption else None
  }

  /** Full record for an imdb id (director credits + title + year). */
  private def detail(imdbId: String, key: String): Option[Candidate] =
    candidateFrom(Json.parse(http.get(idUrl(imdbId, key))))

  /** Accept a candidate iff it is NOT contradicted (different director, or a
   *  year off by >1 with no exact title) AND a positive signal corroborates it:
   *  an exact normalised title, an overlapping director, or a matching year with
   *  a containing title. Mirrors the TMDB/Filmweb corroboration gate. */
  private def corroborated(c: Candidate, queryTitle: String, year: Option[Int], directors: Set[String]): Boolean = {
    val exact          = norm(c.title) == norm(queryTitle)
    val dirOverlap     = directorsOverlap(directors, c.directors)
    val dirContradicts = directors.nonEmpty && c.directors.nonEmpty && !dirOverlap
    val yearMatch      = YearWindow.agrees(year, c.year, YearTolerance).contains(true)
    val titleContains  = TitleMatch.oneStartsWithTheOther(norm(queryTitle), norm(c.title))
    !dirContradicts && !(YearWindow.contradicts(year, c.year, YearTolerance) && !exact) &&
      (exact || dirOverlap || (yearMatch && titleContains))
  }

  private def titleUrl(title: String, year: Option[Int], key: String): String =
    s"$ApiBase?t=${enc(title)}&type=movie${year.map(y => s"&y=$y").getOrElse("")}&apikey=$key"
  private def searchUrl(title: String, year: Option[Int], key: String): String =
    s"$ApiBase?s=${enc(title)}&type=movie${year.map(y => s"&y=$y").getOrElse("")}&apikey=$key"
  private def idUrl(imdbId: String, key: String): String =
    s"$ApiBase?i=${enc(imdbId)}&tomatoes=true&apikey=$key"
  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}

object OMDbClient {
  private val ApiBase       = "https://www.omdbapi.com/"
  private val MaxCandidates = 5
  private val YearTolerance = 1


  /** One OMDb film candidate — imdb id + (normalised-later) title, year, directors. */
  private[enrichment] case class Candidate(imdbId: String, title: String, year: Option[Int], directors: Set[String])

  /** Parse a `?t=` / `?i=` movie record into a Candidate; None unless OMDb said
   *  Response:"True" and carried a `tt…` id. */
  private[enrichment] def candidateFrom(js: JsValue): Option[Candidate] = {
    val ok = (js \ "Response").asOpt[String].contains("True")
    (js \ "imdbID").asOpt[String].filter(_ => ok).filter(_.startsWith("tt")).map { id =>
      Candidate(
        imdbId    = id,
        title     = (js \ "Title").asOpt[String].getOrElse(""),
        year      = (js \ "Year").asOpt[String].flatMap(y => y.take(4).toIntOption),
        directors = parseDirectors((js \ "Director").asOpt[String]))
    }
  }

  /** "Charlotte Wells" / "A, B" → Set; "N/A" / "" → empty. */
  private[enrichment] def parseDirectors(field: Option[String]): Set[String] =
    field.toSet.flatMap((s: String) => s.split(",").map(_.trim).filter(d => d.nonEmpty && d != "N/A"))

  /** Some director of ours naming some of OMDb's, per [[SamePerson]]. False when
   *  either side is empty — and the callers only call that a CONTRADICTION when
   *  both sides named somebody. */
  private[enrichment] def directorsOverlap(a: Set[String], b: Set[String]): Boolean =
    a.exists(x => b.exists(SamePerson(x, _)))

  private def norm(s: String): String =
    TextNormalization.deburr(s).toLowerCase(Locale.ROOT).filter(_.isLetterOrDigit)
}
