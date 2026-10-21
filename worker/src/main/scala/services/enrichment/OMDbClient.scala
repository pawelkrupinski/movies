package services.enrichment

import java.util.Locale

import play.api.libs.json._
import services.movies.SamePerson
import services.resolution.{TitleMatch, YearWindow}
import tools.{HttpFetch, HttpRead, ReadOutcome, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.{Clock, Instant}
import scala.concurrent.duration._

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
 *
 * A DEAD KEY is not asked again and again: after [[OMDbClient.RefusalsBeforePause]] refusals
 * of the key in a row (HTTP 401, "Request limit reached!", "Invalid API key!") every read fails
 * fast, without a call, for [[OMDbClient.RefusalPause]]; then ONE read probes it again (the rest
 * keep failing fast while it is in flight), and any answer — the probe's, or one from a read already
 * in flight when the pause began — lifts the pause. The free
 * key's daily quota is shared by everything holding it, and a spent one answered 401 to hundreds
 * of a recording's asks, every one of which the quota would count again once it reset (record run
 * 469, 2026-10-20). A fast failure is still a failure — never booked as "no such film".
 */
// `apiKey` is OMDB_API_KEY — handed in: the worker wiring passes its resolved value, a spec its
// stub. Unset turns the backfill off.
class OMDbClient(http: HttpFetch, apiKey: Option[settings.OmdbApiKey], clock: Clock)
    extends play.api.Logging {
  import OMDbClient._

  // Refusals of the key in a row, and until when it is not asked; any answer resets both. `probing`: a read
  // past the pause is asking OMDb whether the key works again — the only one let through until it ends.
  private val refusals    = new java.util.concurrent.atomic.AtomicInteger(0)
  private val pausedUntil = new java.util.concurrent.atomic.AtomicReference[Option[Instant]](None)
  private val probing     = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Resolve an IMDb id for a film. Tries each title spelling in turn (pass the
   *  original/English title first — OMDb is an English DB). None when the key is
   *  unset (no HTTP) or nothing is corroborated. A read that FAILED — a block, a 5xx,
   *  OMDb's own "Request limit reached!" document, a body that is not OMDb's JSON —
   *  throws, so a dead source is never booked as a miss and backed off for days. */
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
  private def byTitle(title: String, year: Option[Int], key: String): Option[Candidate] =
    record(titleUrl(title, year, key)).flatMap(candidateFrom)

  private def directorWalk(title: String, year: Option[Int], directors: Set[String], key: String): Option[String] = {
    val hits = record(searchUrl(title, year, key)).toSeq
      .flatMap(js => (js \ "Search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty))
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
    record(idUrl(imdbId, key)).flatMap(candidateFrom)

  /** One OMDb answer: its record, `None` when OMDb says it has none, and a throw for
   *  anything else (see [[OMDbClient.answer]]). */
  private def record(url: String): Option[JsObject] = {
    // Still paused, or past the pause with another read already probing: fail fast, without a call.
    val probe = pausedUntil.get.exists { until =>
      if (clock.instant().isBefore(until) || !probing.compareAndSet(false, true))
        throw new KeyRefusedException(s"OMDb refused its key ${refusals.get} times in a row (spent quota or bad key); " +
          s"not asked again until $until${if (clock.instant().isBefore(until)) "" else " and a probe has answered"}")
      true
    }
    try {
      val read = HttpRead.jsonObject(http, url)(answer(url, _)).toOptionOrThrow
      // An answer — this probe's, or a read in flight since before the pause — means the key works.
      refusals.set(0)
      pausedUntil.set(None)
      read
    } catch {
      case failure: Throwable if refusesKey(failure) =>
        if (refusals.incrementAndGet() >= RefusalsBeforePause) {
          pausedUntil.set(Some(clock.instant().plusMillis(RefusalPause.toMillis)))
          logger.warn(s"OMDb refused its key ${refusals.get} times in a row — not asking it for ${RefusalPause.toMinutes} min " +
            s"(last: ${tools.RedactedUrl(Option(failure.getMessage).getOrElse(failure.toString)).linesIterator.nextOption().getOrElse("")})")
        }
        throw failure
    } finally if (probe) probing.set(false)
  }

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


  /** OMDb answers every query with HTTP 200 and says "none" inside the JSON:
   *  `{"Response":"False","Error":"Movie not found!"}`. Only the errors that mean
   *  "nothing matches" are an absence; any other error document — "Request limit
   *  reached!", "Invalid API key!", or one with no error at all — is a failed read. */
  private val NoneErrors: Set[String] = Set("Movie not found!", "Incorrect IMDb ID.", "Too many results.")

  /** Refusals of the key in a row before OMDb stops being asked, and for how long (see the class doc). */
  val RefusalsBeforePause: Int            = 5
  val RefusalPause:        FiniteDuration = 30.minutes

  /** A read failed because OMDb refused the KEY, not the request: HTTP 401 (OMDb's spent quota), its
   *  "Request limit reached!" / "Invalid API key!" documents, or a fallback chain's composite naming a 401
   *  from a leg (FallbackHttpFetch throws a plain RuntimeException carrying each leg's failure). */
  private[enrichment] def refusesKey(failure: Throwable): Boolean = failure match {
    case s: tools.HttpStatusException => s.code == 401
    case other =>
      val message = Option(other.getMessage).getOrElse("")
      message.contains("Request limit reached!") || message.contains("Invalid API key!") || message.contains("HTTP 401 for ")
  }

  /** A read not made because the key is paused — a failed read, as the refusals before it were. */
  final class KeyRefusedException(message: String) extends RuntimeException(message)

  private[enrichment] def answer(url: String, js: JsObject): ReadOutcome[JsObject] =
    (js \ "Response").asOpt[String] match {
      case Some("True") => ReadOutcome.Answered(js)
      case _ =>
        (js \ "Error").asOpt[String] match {
          case Some(error) if NoneErrors.contains(error) => ReadOutcome.none(s"OMDb: $error")
          case error => ReadOutcome.unexpectedBody(url, s"OMDb error ${error.getOrElse("(none given)")}", js.toString)
        }
    }

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
