package services.enrichment

import play.api.libs.json._
import tools.{Env, HttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Feature-gated OMDb (omdbapi.com) client that recovers IDENTIFIERS, not rating
 * values: an IMDb id (resolved with the same rigor as TMDB resolution) and the
 * canonical Rotten Tomatoes URL. The canonical refreshers ([[ImdbRatings]] /
 * [[RottenTomatoesRatings]]) then fetch the actual scores FROM those, so OMDb
 * never writes a rating value — one canonical writer per value.
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
class OMDbClient(http: HttpFetch, apiKey: => Option[String] = OMDbClient.ApiKey) {
  import OMDbClient._

  /** Resolve an IMDb id for a film. Tries each title spelling in turn (pass the
   *  original/English title first — OMDb is an English DB). None when the key is
   *  unset (no HTTP), nothing is corroborated, or every call fails. */
  def findImdbId(titles: Seq[String], year: Option[Int], directors: Set[String]): Option[String] =
    apiKey.flatMap { key =>
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
    val js = Try(Json.parse(http.get(titleUrl(title, year, key)))).getOrElse(JsNull)
    candidateFrom(js)
  }

  private def directorWalk(title: String, year: Option[Int], directors: Set[String], key: String): Option[String] = {
    val js   = Try(Json.parse(http.get(searchUrl(title, year, key)))).getOrElse(JsNull)
    val hits = (js \ "Search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(h => (h \ "imdbID").asOpt[String].filter(_.startsWith("tt"))).distinct.take(MaxCandidates)
    val matches = hits
      .flatMap(id => detail(id, key))
      .filter(c => directorsOverlap(directors, c.directors) && !yearContradicts(c.year, year))
      .map(_.imdbId).distinct
    // Accept ONLY when exactly one candidate's director corroborates — same
    // "never guess among several" rule as ImdbClient.disambiguateByDirector.
    if (matches.sizeIs == 1) matches.headOption else None
  }

  /** Full record for an imdb id (director credits + title + year). */
  private def detail(imdbId: String, key: String): Option[Candidate] =
    candidateFrom(Try(Json.parse(http.get(idUrl(imdbId, key)))).getOrElse(JsNull))

  /** Recover the canonical Rotten Tomatoes URL for an imdb id — OMDb's
   *  `tomatoURL`, present only when OMDb holds RT data for the film. */
  def rottenTomatoesUrl(imdbId: String): Option[String] =
    apiKey.flatMap { key =>
      if (imdbId.trim.isEmpty) None
      else {
        val js = Try(Json.parse(http.get(idUrl(imdbId.trim, key)))).getOrElse(JsNull)
        (js \ "tomatoURL").asOpt[String].map(_.trim)
          .filter(u => u.nonEmpty && u != "N/A" && u.startsWith("http"))
      }
    }

  /** Accept a candidate iff it is NOT contradicted (different director, or a
   *  year off by >1 with no exact title) AND a positive signal corroborates it:
   *  an exact normalised title, an overlapping director, or a matching year with
   *  a containing title. Mirrors the TMDB/Filmweb corroboration gate. */
  private def corroborated(c: Candidate, queryTitle: String, year: Option[Int], directors: Set[String]): Boolean = {
    val exact          = norm(c.title) == norm(queryTitle)
    val dirOverlap     = directorsOverlap(directors, c.directors)
    val dirContradicts = directors.nonEmpty && c.directors.nonEmpty && !dirOverlap
    val yearMatch      = (for { y <- year; cy <- c.year } yield math.abs(y - cy) <= 1).getOrElse(false)
    val titleContains  = { val a = norm(queryTitle); val b = norm(c.title); a.nonEmpty && b.nonEmpty && (a.startsWith(b) || b.startsWith(a)) }
    !dirContradicts && !(yearContradicts(c.year, year) && !exact) &&
      (exact || dirOverlap || (yearMatch && titleContains))
  }

  private def yearContradicts(candYear: Option[Int], year: Option[Int]): Boolean =
    (for { y <- year; cy <- candYear } yield math.abs(y - cy) > 1).getOrElse(false)

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

  /** Feature flag: the backfill is OFF whenever this is unset. */
  val ApiKey: Option[String] = Env.get("OMDB_API_KEY")

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

  private[enrichment] def directorsOverlap(a: Set[String], b: Set[String]): Boolean = {
    if (a.isEmpty || b.isEmpty) false
    else {
      val na = a.map(norm); val nb = b.map(norm)
      na.exists(x => nb.exists(y => x == y || (x.length > 4 && y.contains(x)) || (y.length > 4 && x.contains(y))))
    }
  }

  private def norm(s: String): String =
    TextNormalization.deburr(s).toLowerCase.filter(_.isLetterOrDigit)
}
