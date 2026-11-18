package clients

import play.api.libs.json._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.Locale

/**
 * THE TMDB PORT: every read the worker asks of The Movie DB -- title searches, a film's records and cross-ids, posters,
 * the change feed, a person's credits -- as the enrichment, the ratings' URL probes and the identity lookups depend on
 * it. [[HttpTmdbClient]] is its one implementation, over the paced HTTP wire, wired at the worker's composition root
 * (`modules.wiring.HttpWiring`); each read's contract is documented there. The reads derived from others
 * ([[imdbId]], [[englishTitle]], [[personFilmography]]) are this trait's, one rule for every implementation.
 */
trait TmdbClient {
  /** The deployment's language every localized request is asked in. */
  def language: Locale

  def searchAsRanked(title: String): Option[Seq[TmdbClient.SearchResult]]
  def searchUnique(title: String, year: Option[Int]): Option[TmdbClient.SearchResult]
  def searchYearExactTop(title: String, year: Option[Int]): Option[TmdbClient.SearchResult]
  def externalIds(tmdbId: Int): TmdbClient.ExternalIds
  def findByImdbId(imdbId: String): Option[TmdbClient.SearchResult]
  def originalTitle(tmdbId: Int): Option[String]
  def filmAnswers(tmdbId: Int): Option[Seq[JsValue]]
  def localizedAnswer(tmdbId: Int): Option[JsValue]
  def readRecordAgain(tmdbId: Int): Unit
  def details(tmdbId: Int): Option[TmdbClient.Details]
  def crewIds(tmdbId: Int): Set[Int]
  def directorsFor(tmdbId: Int): Set[String]
  def fullDetails(tmdbId: Int): Option[TmdbClient.FullDetails]
  def posters(tmdbId: Int, also: Seq[String] = Nil): Seq[TmdbClient.PosterImage]
  def changedMovies(start: java.time.LocalDate, end: java.time.LocalDate, page: Int): (Seq[Int], Int)
  def movieChanges(tmdbId: Int, start: java.time.LocalDate, end: java.time.LocalDate): Seq[TmdbClient.MovieEdit]
  def findPersonCandidates(name: String): Seq[Int]
  def personDirectorCredits(personId: Int): Seq[TmdbClient.SearchResult]
  def personWriterCredits(personId: Int): Seq[TmdbClient.SearchResult]

  /** Look up the IMDB id of a TMDB movie. Returns None when TMDB knows the
   *  movie but hasn't been told its IMDB cross-reference yet (rare for theatrical
   *  releases, common for film festival items). Thin accessor over
   *  [[externalIds]] for callers that only want the IMDb id.
   */
  final def imdbId(tmdbId: Int): Option[String] = externalIds(tmdbId).imdbId

  /** TMDB's English release title for a movie. `/movie/{id}?language=en-US`
   *  returns the localized `title` for the requested language, which for
   *  non-English films is the English release title (e.g. tmdbid 1290432
   *  returns "A Private Life" in en-US, "Vie privée" as original_title).
   *  Used as a fallback for MC/RT URL probes because both sites index foreign
   *  films under their English release title, not the production-language one.
   */
  final def englishTitle(tmdbId: Int): Option[String] = details(tmdbId).flatMap(_.englishTitle)

  /** The films a director walk searches for one person: their directing credits or — with none —
   *  what they wrote, since a venue may print the writer. The one rule both resolvers walk by. */
  final def personFilmography(personId: Int): Seq[TmdbClient.SearchResult] = {
    val directed = personDirectorCredits(personId)
    if (directed.nonEmpty) directed else personWriterCredits(personId)
  }
}

object TmdbClient {

  /** How many same-name people the director walk will try before giving up. Two
   *  covers the observed failures (a credit-less duplicate or an alias ranked
   *  above the real person); the rest is headroom that still bounds the extra
   *  credit fetches a common name like "John Smith" would otherwise cost. */
  val MaxPersonCandidates = 4

  val ApiBase = "https://api.themoviedb.org/3"
  // TMDB's CDN-served poster path. w500 is the largest "good" size for our
  // cards (next step up is "original" which can be 2000+ px and isn't worth
  // the bytes). Matches the size Multikino's own posters ship at.
  private[clients] val PosterBase = "https://image.tmdb.org/t/p/w500"
  // Widest aspect ratio (width/height, TMDB's convention) we still treat as a
  // portrait poster. A 2:3 poster is ~0.667; anything above this is squarer or
  // landscape and would crop badly in the 2:3 card, so we skip it.
  private[clients] val MaxPortraitAspectRatio = 0.72


  /** The request language when a construction doesn't specify one. Polish keeps
   *  every existing single-country (Poland-only) call site — and every test
   *  fixture keyed on `language=pl-PL` URLs — byte-identical. */
  val DefaultLanguage: Locale = Locale.forLanguageTag("pl-PL")

  /** One key of a film's edits: the languages its items name and — for crew — each credited
   *  person with their department, and the jobs (from an item's new or original value). */
  final case class MovieEdit(key: String, languages: Set[String], jobs: Set[String], credits: Seq[(Int, String)] = Nil)

  /** The cross-reference ids TMDB's `/external_ids` endpoint carries for a movie. */
  case class ExternalIds(imdbId: Option[String], wikidataId: Option[String])

  case class SearchResult(
    id:            Int,
    title:         String,
    originalTitle: Option[String],
    releaseYear:   Option[Int],
    popularity:    Double,
    // Deployment-language `overview` from the search row, carried because
    // `/search/movie?language=<deployment>` already returns it and a caller that wants
    // to judge synopsis closeness should not pay a second HTTP call for it. Empty for
    // the person-credits decoder. No resolution path reads it today — the synopsis
    // tie-break went with `pickBest`.
    overview:      Option[String] = None
  ) {
    /** Every title TMDB knows this hit by: its deployment-language title, then its original. */
    def titles: Seq[String] = title +: originalTitle.toSeq
  }

  /** A TMDB hit whose Polish OR original title matches the query, ignoring case,
   *  surrounding whitespace AND punctuation — the exactness
   *  [[TmdbClient.searchYearExactTop]] gates on.
   *
   *  Punctuation-blind because TMDB's Polish title for a film routinely carries a
   *  trailing period (or other punctuation) the exhibitor's title omits: tmdb
   *  950028 ("The Invite", 2026, dir. Olivia Wilde) is titled "Zaproszenie." in
   *  pl-PL, while cinemas report "Zaproszenie". A raw-text gate dropped it from
   *  the exact-match set, leaving the punctuation-clean same-title different film
   *  — "Zaproszenie" (tmdb 830788, "The Invitation", 2022) — to win instead. We
   *  strip to alphanumerics only; deliberately NOT the full
   *  `TitleNormalizer.sanitize` (which also romanises numerals, etc.) so this
   *  stays a near-verbatim match, just blind to punctuation. */
  private val NonAlphanumeric = java.util.regex.Pattern.compile("[^\\p{L}\\p{N}]+")
  private def titleMatchKey(s: String): String =
    NonAlphanumeric.matcher(s).replaceAll("").toLowerCase(java.util.Locale.ROOT)

  private[clients] def isExactTitleMatch(r: SearchResult, title: String): Boolean = {
    val q = titleMatchKey(title)
    q.nonEmpty && r.titles.exists(titleMatchKey(_) == q)
  }

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
    posterUrl:      Option[String],
    ageRating:      Option[String] = None,
    /** Writing credits (writer, screenplay, story) — part of the crew a venue may
     *  print as the director; see `services.resolution.Candidate`. */
    writers:        Seq[String]    = Nil
  ) {
    /** Everyone a cinema could credit as the film's maker. */
    def crew: Seq[String] = (director ++ writers).distinct
  }

  /** TMDB crew `job`s that count as writing the film. */
  val WritingJobs: Set[String] = Set("Writer", "Screenplay", "Story")

  /** One poster variant from `/movie/{id}/images`. `aspectRatio` is
   *  width/height (TMDB's convention) — a 2:3 portrait poster is ~0.667.
   *  `language` is the `iso_639_1` tag ("pl"), or None for language-neutral
   *  artwork. */
  case class PosterImage(
    filePath:    String,
    language:    Option[String],
    aspectRatio: Double,
    voteAverage: Double,
    width:       Int,
    voteCount:   Int = 0
  )

  private[clients] def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
