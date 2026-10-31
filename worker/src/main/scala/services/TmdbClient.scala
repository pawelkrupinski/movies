package clients

import play.api.libs.json._
import _root_.tools.{HttpFetch, HttpStatusException}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.util.Try

/**
 * Bridges exhibitor film titles to IMDB ids via The Movie DB.
 *
 * TMDB has multilingual title indexing — `/search/movie?query=…&language=pl-PL`
 * returns the same movies you'd see on imdb.com but matched by their local
 * release title. From a hit we follow `/movie/{id}/external_ids` to read off
 * the IMDB id (e.g. `tt15239678`), which is the stable cross-system key.
 *
 * The request `language` is the DEPLOYMENT's language ([[language]], from the
 * country's `Locale`), so overview/genres/localized-title come back in that
 * language — Polish for `kinowo`, English for the UK deployment, German for
 * Germany — instead of a hardcoded `pl-PL`. Defaults to Polish so every
 * existing single-country construction (and test fixture keyed on `pl-PL`
 * URLs) is unchanged. The `en-US` calls that fetch the ENGLISH release title
 * (MC/RT URL probes) stay pinned to English regardless of deployment.
 *
 * `TMDB_API_KEY` can be either:
 *   - legacy v3 API key (32-char hex), passed as `?api_key=…` query parameter, or
 *   - v4 application bearer token (JWT-shaped), passed as
 *     `Authorization: Bearer …`.
 * The client sends BOTH on every request. TMDB picks whichever is valid for
 * the key it actually receives, so prod (currently a v4 token) and CI
 * (currently a v3 key) can both run unchanged against the same code path
 * without the deploy pipeline knowing which one is configured. The detector
 * is purely additive — a v3 key sent in `Authorization: Bearer` is ignored
 * by TMDB, and vice-versa.
 */
class TmdbClient(
  http: HttpFetch,
  // TMDB_API_KEY — handed in: the worker wiring passes its Env's value, a spec its stub.
  apiKey: Option[settings.TmdbApiKey],
  // The deployment's language, threaded into every localized TMDB request so a
  // non-Polish deployment gets non-Polish overview/genres/titles. Exposed as a
  // `val` so the enrichment (`MovieService`) can canonicalise the country names
  // TMDB returns in the SAME language it fetched them in.
  val language: Locale = TmdbClient.DefaultLanguage,
  // How a film's two responses (`details`, `fullDetails`, `identityRecord`) are parsed: shared with
  // the identity store's normalizer, which parsed the same bodies a moment before on this thread
  // (`tools.JsonBodies`) — every reader of them takes that parse, so none stays on the thread.
  bodies: _root_.tools.JsonBodies = new _root_.tools.JsonBodies,
) {

  import TmdbClient.{ApiBase, urlEncode}

  // TMDB's `language=` wants a BCP-47 tag ("pl-PL", "en-GB", "de-DE"); the
  // `/images` endpoint's `include_image_language` wants the bare language
  // subtag ("pl", "en", "de") plus `null` for language-neutral artwork.
  private val languageTag: String   = language.toLanguageTag
  private val imageLanguages: String = s"${language.getLanguage},null"

  /** Auth header for every request. Built lazily off `apiKey` so a missing
   *  key returns None and the calling method short-circuits via flatMap
   *  without ever hitting the network. */
  private def authHeader: Option[Map[String, String]] =
    apiKey.map(_.authorization)

  /** Query-string suffix carrying the legacy v3 `api_key=` parameter.
   *  Empty when no key is configured (the calling method short-circuits
   *  via `authHeader.flatMap` before reaching here, so this is only
   *  evaluated alongside a real key). Appended to every URL that already
   *  has at least one query parameter (`?language=…&api_key=…`); for
   *  query-less URLs the caller switches `?` for the leading `&` via
   *  `apiKeyParameter(separator)`. */
  private def apiKeyParameter(separator: String): String =
    apiKey.map(key => s"${separator}api_key=${key.value}").getOrElse("")

  /** A 404 on a by-id read is TMDB answering "no such film/person" — no credits, not a
   *  failed read — so it reads as an empty crew. Every other failure propagates. */
  private def orEmptyWhenUnknown(read: => String): String =
    try read catch { case e: HttpStatusException if e.code == 404 => """{"crew":[]}""" }

  /** Every TMDB call routes through here. A transient failure — a timeout, a lost connection, a 5xx — is asked
   *  again on the spot by the chain `http` is ([[_root_.tools.RetryingHttpFetch]] in `HttpWiring.pacedWire`), a 429 is
   *  waited out by its gate (`ThrottledHttpFetch`); a 404 for an id/title TMDB doesn't know fails at once. A
   *  `None`/empty parse result isn't an exception, so the graceful-degradation paths are untouched. */
  private def httpGet(url: String, auth: Map[String, String]): String = http.get(url, auth)

  private def searchUrl(title: String, yearParameter: Option[Int]): String = {
    val yp = yearParameter.map(y => s"&year=$y&primary_release_year=$y").getOrElse("")
    s"$ApiBase/search/movie?language=$languageTag&include_adult=false&query=${urlEncode(title)}$yp${apiKeyParameter("&")}"
  }

  /** One TMDB title search, popularity-ordered by [[TmdbResponses.popularFirst]]. Scoped to a
   *  year when one is given — the year a cinema reports can be the production year while
   *  TMDB stores the theatrical date, so the two forms return materially different result
   *  sets and the two callers choose deliberately between them. */
  private def searchOnce(title: String, yearParameter: Option[Int], auth: Map[String, String]): Seq[TmdbClient.SearchResult] = {
    val url = searchUrl(title, yearParameter)
    TmdbResponses.popularFirst(httpGet(url, auth))
  }

  /** One yearless title search in TMDB's OWN order, not re-sorted by popularity: where a film
   *  ranks in it is identity evidence (`PinnedGateMeasures.titleSearch`), measured exactly as the
   *  calibration measured it on recorded answers. `None` without a key; a failed request throws. */
  def searchAsRanked(title: String): Option[Seq[TmdbClient.SearchResult]] = authHeader.map { auth =>
    TmdbResponses.rankedResults(httpGet(searchUrl(title, None), auth))
  }

  /** Resolve ONLY when the title search is unambiguous — exactly one result.
   *  Used when the title is the only signal we have (no year / director /
   *  original-title hint): with several same-title films we'd otherwise fall to
   *  the popularity tie-break and guess (the "Zaproszenie" class of
   *  mis-resolution), so refuse rather than guess. Mirrors `search`'s
   *  year-restricted-then-yearless preference, checking uniqueness on whichever
   *  tier returns results. */
  def searchUnique(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] = authHeader.flatMap { auth =>
    val scoped  = if (year.isDefined) searchOnce(title, year, auth) else Seq.empty
    val results = if (scoped.nonEmpty) scoped else searchOnce(title, None, auth)
    if (results.lengthCompare(1) == 0) results.headOption else None
  }

  /** Resolve when a YEAR is present AND the year-scoped search returns EXACTLY
   *  ONE EXACT title match (Polish or original). Broader than [[searchUnique]]
   *  (which needs the whole result set to be a singleton): the year scopes to
   *  the right era and a verbatim title in the results is the confidence, so a
   *  row whose year-scoped search returns several same-year films but only ONE
   *  of them an exact title match ("Sundown" alongside "Sundown Town", "DJ at
   *  Sundown") still resolves to the one the cinema named. Still refuses when
   *  NO result is an exact match, or when MORE THAN ONE is — that's the same
   *  popularity guess [[searchUnique]] exists to avoid: a year-scoped "Lalka"
   *  search for 2026 returns two exact Polish-title matches, Maciej Kawalski's
   *  real upcoming film (tmdbId 1321666, low popularity) and an unrelated
   *  French film TMDB also titles "Lalka" in Polish (tmdbId 1309396, higher
   *  popularity) — picking the top of that pair by popularity silently misrouted
   *  every Polish cinema's real "Lalka" listing to the French film. Two exact
   *  matches is genuine ambiguity, not confidence; refuse rather than guess, the
   *  same as a bare-title search returning several results. No-op without a
   *  year, so yearless rows stay refused. */
  def searchYearExactTop(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] =
    if (year.isEmpty) None
    else authHeader.flatMap { auth =>
      val exact = searchOnce(title, year, auth).filter(TmdbClient.isExactTitleMatch(_, title))
      if (exact.lengthCompare(1) == 0) exact.headOption else None
    }

  /** Look up the IMDB id of a TMDB movie. Returns None when TMDB knows the
   *  movie but hasn't been told its IMDB cross-reference yet (rare for theatrical
   *  releases, common for film festival items). Thin accessor over
   *  [[externalIds]] for callers that only want the IMDb id.
   */
  def imdbId(tmdbId: Int): Option[String] = externalIds(tmdbId).imdbId

  /** Read every cross-reference id `/movie/{id}/external_ids` carries in one
   *  call. Besides the IMDb id we capture `wikidata_id` — a resolved tmdbId then
   *  gives a Wikidata entity directly (no Filmweb id needed), from which
   *  [[WikidataClient]] can harvest the remaining film-database ids. */
  def externalIds(tmdbId: Int): TmdbClient.ExternalIds = authHeader.map { auth =>
    val body = httpGet(s"$ApiBase/movie/$tmdbId/external_ids${apiKeyParameter("?")}", auth)
    val js   = Json.parse(body)
    TmdbClient.ExternalIds(
      imdbId     = (js \ "imdb_id").asOpt[String].filter(_.startsWith("tt")),
      wikidataId = (js \ "wikidata_id").asOpt[String].filter(_.startsWith("Q"))
    )
  }.getOrElse(TmdbClient.ExternalIds(None, None))

  /** Reverse lookup: find a TMDB movie record by its IMDB id. Used when the
   *  Polish exhibitor title doesn't resolve via TMDB's search (no Polish
   *  translation registered for the film) and a manual override pins the
   *  right IMDb id — we still want TMDB's `tmdbId` and `originalTitle` for
   *  the rest of the enrichment row.
   */
  def findByImdbId(imdbId: String): Option[TmdbClient.SearchResult] = authHeader.flatMap { auth =>
    val body = httpGet(s"$ApiBase/find/$imdbId?external_source=imdb_id&language=$languageTag${apiKeyParameter("&")}", auth)
    TmdbResponses.findMovieResults(body).headOption
  }

  /** TMDB's production-language original title for a known movie. Used as a
   *  more accurate source than OMDb's English-localised `Title` (which can
   *  drift for non-English films). Hits `/movie/{id}` with no language so the
   *  field comes back in the production language.
   */
  def originalTitle(tmdbId: Int): Option[String] = authHeader.flatMap { auth =>
    val body = httpGet(s"$ApiBase/movie/$tmdbId${apiKeyParameter("?")}", auth)
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
  private def detailsUrl(tmdbId: Int): String =
    s"$ApiBase/movie/$tmdbId?language=en-US&append_to_response=alternative_titles${apiKeyParameter("&")}"
  private def fullDetailsUrl(tmdbId: Int): String =
    s"$ApiBase/movie/$tmdbId?language=$languageTag&append_to_response=credits,release_dates${apiKeyParameter("&")}"

  /** The film as the identity measures read it (`services.identity.TmdbFilmRecord`), from the two
   *  answers this client already asks about it — [[fullDetails]]'s and [[details]]'s — so a tree
   *  recorded for either serves it. `None` when TMDB knows no such film; a failed read THROWS, so
   *  the identity resolver's lookups can tell "no film" from "not answered". */
  def identityRecord(tmdbId: Int): Option[services.identity.IdentityMeasures.Film] = authHeader.flatMap { auth =>
    services.identity.TmdbFilmRecord.parse(Seq(fullDetailsUrl(tmdbId), detailsUrl(tmdbId))
      .map(url => bodies.parse(orEmptyWhenUnknown(httpGet(url, auth))))).map(_._1)
  }

  /** The film's top-billed cast off its localized record ([[fullDetails]]' request, as [[identityRecord]] reads it): what
   *  the identity store files of it, and the cast evidence reads (`services.identity.CastEvidence`). */
  def identityCast(tmdbId: Int): Option[Seq[String]] = authHeader.flatMap { auth =>
    services.identity.TmdbFilmRecord.cast(Seq(bodies.parse(orEmptyWhenUnknown(httpGet(fullDetailsUrl(tmdbId), auth)))))
  }

  /** The words the film's overviews capitalise, off both answers [[identityRecord]] reads (`services.identity.TmdbFilmRecord.overview`):
   *  what the identity store files of them, and the synopsis evidence reads (`services.identity.agreement.SynopsisEvidence`). */
  def identityOverview(tmdbId: Int): Option[services.identity.ProseWords] = authHeader.flatMap { auth =>
    services.identity.TmdbFilmRecord.overview(Seq(fullDetailsUrl(tmdbId), detailsUrl(tmdbId))
      .map(url => bodies.parse(orEmptyWhenUnknown(httpGet(url, auth)))))
  }

  /** Asks TMDB for the film's localized record again ([[fullDetails]]' request, no poster call): what the identity
   *  store files as the record's localized response — a record filed with only its release year gains its day
   *  (`services.identity.agreement.AgreementStage.wanted`, its `Question.Record`s). A 404 is TMDB's answer; any other failure THROWS. */
  def readRecordAgain(tmdbId: Int): Unit = authHeader.foreach(auth => orEmptyWhenUnknown(httpGet(fullDetailsUrl(tmdbId), auth)))

  def details(tmdbId: Int): Option[TmdbClient.Details] = authHeader.flatMap { auth =>
    Try(httpGet(detailsUrl(tmdbId), auth))
      .toOption.map(body => TmdbResponses.details(bodies.parse(body)))
  }

  /** Every person id credited anywhere on a movie's crew — not only the Director
   *  job. A cinema routinely credits the film's OTHER director (Ethan for a film
   *  TMDB credits to Joel Coen) or someone TMDB files under a different job (Karl
   *  Freund shot the 1931 "Dracula" and is widely credited as co-directing it), and
   *  by ID rather than name so a pseudonym resolves to the same person. Empty when
   *  TMDB has no credits; THROWS when the call fails — a failed read is not "nobody". */
  def crewIds(tmdbId: Int): Set[Int] = authHeader.map { auth =>
    val body = orEmptyWhenUnknown(httpGet(s"$ApiBase/movie/$tmdbId/credits${apiKeyParameter("?")}", auth))
    TmdbResponses.arrayAt(Json.parse(body) \ "crew")
      .flatMap(c => (c \ "id").asOpt[Int])
      .toSet
  }.getOrElse(Set.empty)

  /** Director name(s) credited on a TMDB movie. Empty when TMDB has no
   *  credits; THROWS when the call fails. Used to verify a title-search candidate
   *  against the cinema-reported director — when a film's title and a
   *  cinema's title happen to collide (Polish "Niedźwiedzica" matches both
   *  Grizzly Falls 1999 and the 2026 Helgestad document) the directors disagree. */
  def directorsFor(tmdbId: Int): Set[String] = authHeader.map { auth =>
    val body = orEmptyWhenUnknown(httpGet(s"$ApiBase/movie/$tmdbId/credits${apiKeyParameter("?")}", auth))
    TmdbJson.crewWith(TmdbResponses.arrayAt(Json.parse(body) \ "crew"), TmdbJson.Director)
      .flatMap { c =>
        // Include both the localised `name` and the native-script `original_name`
        // so that cinemas reporting a director in their native script (e.g. "张钢"
        // for a Chinese director TMDB lists as "Gang Zhang") still match.
        Seq("name", "original_name").flatMap(f => (c \ f).asOpt[String])
      }
      .filter(_.nonEmpty)
      .toSet
  }.getOrElse(Set.empty)

  /** One TMDB `/movie/{id}?language=<deployment>&append_to_response=credits,release_dates`
   *  call returning everything the TMDB enrichment stage needs to fill a
   *  `SourceData(Tmdb)` slot:
   *    - Deployment-language title + production-language original title
   *    - Deployment-language synopsis (`overview`)
   *    - Director(s) + top-billed cast
   *    - Year (`release_date`) + runtime
   *    - Production countries (raw names — caller canonicalises)
   *    - Poster URL — the best deployment-language portrait poster from `/movie/{id}/images`
   *      (see `posters`), falling back to the default `poster_path` when that
   *      endpoint has no usable portrait variant.
   *    - Age rating — the deployment country's certification, out of the appended
   *      `release_dates` block.
   *
   *  Returns None on network failure / unknown id — callers fall back to
   *  whatever the previous slot held. The detail itself is one round-trip; the
   *  poster lookup adds a single failure-tolerant `/images` call, so a poster
   *  hiccup never breaks the resolve.
   *
   *  `release_dates` rides along on `append_to_response` rather than costing its own
   *  request. It shipped as a separate `/movie/{id}/release_dates` call to spare the
   *  detail fixtures a re-fingerprint — which bought a per-resolve round-trip AND left
   *  the endpoint with no fixture in the committed corpus at all, so `FakeHttpFetch`
   *  threw for it, `Try` swallowed that, and every film in CI resolved with no age
   *  rating while a developer holding a locally-recorded tree saw them appear. Folding
   *  it in costs one fixture migration and removes both problems: same request count as
   *  before the feature, and the rating now arrives in the body every test already replays. */
  def fullDetails(tmdbId: Int): Option[TmdbClient.FullDetails] = authHeader.flatMap { auth =>
    Try(httpGet(fullDetailsUrl(tmdbId), auth))
      .toOption.map { body =>
        // Prefer the best deployment-language portrait poster from `/images` over the
        // default `poster_path` (which is whatever TMDB flags primary,
        // regardless of language or shape). This poster only ever surfaces
        // as a backup — the Tmdb slot ranks below every cinema in
        // `MovieRecord.posterUrl` — so a better localised portrait improves
        // the `data-fallbacks` chain (and the no-cinema-poster case).
        // A failed `/images` read falls back to `poster_path` — the poster TMDB flags
        // primary is a real answer, so a poster hiccup never breaks the resolve.
        TmdbResponses.fullDetails(bodies.parse(body), language,
          Try(posters(tmdbId)).toOption.flatMap(TmdbResponses.bestPortraitPosterUrl(_, language.getLanguage)))
      }
  }

  /** Deployment-language + language-neutral poster variants for a movie, from
   *  TMDB's `/movie/{id}/images` endpoint. Each carries its language tag,
   *  aspect ratio and community vote, so `bestPortraitPosterUrl` can choose
   *  the best *portrait* localised poster — a better backup than the default
   *  `poster_path`, which is whatever TMDB flags primary regardless of shape.
   *
   *  `include_image_language=<lang>,null` restricts the response to the
   *  deployment language's artwork plus language-neutral — `also` adds other languages' (the
   *  identity's poster evidence reads English artwork too: a venue bills the international poster as
   *  often as its own country's). THROWS when the call fails; [[fullDetails]] then falls back to
   *  `poster_path`, by its own explicit choice. */
  def posters(tmdbId: Int, also: Seq[String] = Nil): Seq[TmdbClient.PosterImage] = authHeader.map { auth =>
    val languages = if (also.isEmpty) imageLanguages else (language.getLanguage +: also :+ "null").distinct.mkString(",")
    val body = httpGet(s"$ApiBase/movie/$tmdbId/images?include_image_language=$languages${apiKeyParameter("&")}", auth)
    TmdbResponses.posters(body)
  }.getOrElse(Seq.empty)

  /** The films TMDB edited between `start` and `end` (at most 14 days apart), one page. */
  def changedMovies(start: java.time.LocalDate, end: java.time.LocalDate, page: Int): (Seq[Int], Int) = authHeader.map { auth =>
    TmdbResponses.changedIds(httpGet(s"$ApiBase/movie/changes?start_date=$start&end_date=$end&page=$page${apiKeyParameter("&")}", auth))
  }.getOrElse((Nil, 1))

  /** What TMDB edited about one film between `start` and `end`: each key and its items' languages. */
  def movieChanges(tmdbId: Int, start: java.time.LocalDate, end: java.time.LocalDate): Seq[TmdbClient.MovieEdit] = authHeader.map { auth =>
    TmdbResponses.movieEdits(httpGet(s"$ApiBase/movie/$tmdbId/changes?start_date=$start&end_date=$end${apiKeyParameter("&")}", auth))
  }.getOrElse(Nil)

  /** Every plausible TMDB person for a name, Directing-known first and otherwise
   *  in TMDB's own (popularity) order.
   *
   *  The single top hit is wrong often enough to matter once the director walk is
   *  the whole resolution. Two shapes seen in the live corpus: a DUPLICATE stub
   *  with no credits ranked above the real person ("Chan-wook Park" → a "Park Chan
   *  Wook" entry with zero directing credits, while the real one is 5646), and an
   *  ALIAS collision ("Andrew Stanton" → Jim Wynorski, who lists it among his many
   *  pseudonyms). Both resolve to a person whose filmography simply doesn't contain
   *  the film — which is exactly the evidence needed to reject them, so the walk
   *  picks the person rather than trusting the ranking.
   *
   *  Capped: past a handful of same-name people the extra credit fetches cost more
   *  than they can plausibly recover. Directing-known first keeps the common case
   *  at one round-trip — an actor sharing a director's name is still skipped, just
   *  no longer fatally when TMDB ranks a credit-less stub above the real one. */
  def findPersonCandidates(name: String): Seq[Int] = authHeader.map { auth =>
    TmdbResponses.personCandidates(httpGet(s"$ApiBase/search/person?query=${urlEncode(name)}${apiKeyParameter("&")}", auth))
      .take(TmdbClient.MaxPersonCandidates)
  }.getOrElse(Seq.empty)

  /** A person's movies as a director — the films they're credited for in the
   *  Directing department. Returns the same `SearchResult` shape so the
   *  caller can reuse picking / sorting logic. */
  def personDirectorCredits(personId: Int): Seq[TmdbClient.SearchResult] =
    personCredits(personId, "Directing")

  /** The films this person is credited on as a WRITER.
   *
   *  Cinemas do not reliably print the director. "Drzewo magii" is directed by Ben
   *  Gregor and written by Simon Farnaby, and some cinemas print the writer — so a
   *  walk over directing credits alone finds nothing for them and the film goes
   *  unresolved. Same response, same round-trip (TMDB returns the whole crew), just
   *  the other department. */
  def personWriterCredits(personId: Int): Seq[TmdbClient.SearchResult] =
    personCredits(personId, "Writing")

  /** The films a director walk searches for one person: their directing credits or — with none —
   *  what they wrote, since a venue may print the writer. The one rule both resolvers walk by. */
  def personFilmography(personId: Int): Seq[TmdbClient.SearchResult] = {
    val directed = personDirectorCredits(personId)
    if (directed.nonEmpty) directed else personWriterCredits(personId)
  }

  private def personCredits(personId: Int, department: String): Seq[TmdbClient.SearchResult] = authHeader.map { auth =>
    TmdbResponses.creditsIn(orEmptyWhenUnknown(httpGet(s"$ApiBase/person/$personId/movie_credits?language=$languageTag${apiKeyParameter("&")}", auth)), department)
  }.getOrElse(Seq.empty)

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
