package clients

import play.api.libs.json._
import tools.{Env, HttpFetch, HttpStatusException, RetryWithBackoff, SynopsisSimilarity}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.concurrent.duration._
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
  apiKey: => Option[String] = TmdbClient.ApiKey,
  // The deployment's language, threaded into every localized TMDB request so a
  // non-Polish deployment gets non-Polish overview/genres/titles. Exposed as a
  // `val` so the enrichment (`MovieService`) can canonicalise the country names
  // TMDB returns in the SAME language it fetched them in.
  val language: Locale = TmdbClient.DefaultLanguage,
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
    apiKey.map(k => Map("Authorization" -> s"Bearer $k"))

  /** Query-string suffix carrying the legacy v3 `api_key=` parameter.
   *  Empty when no key is configured (the calling method short-circuits
   *  via `authHeader.flatMap` before reaching here, so this is only
   *  evaluated alongside a real key). Appended to every URL that already
   *  has at least one query parameter (`?language=…&api_key=…`); for
   *  query-less URLs the caller switches `?` for the leading `&` via
   *  `apiKeyParameter(separator)`. */
  private def apiKeyParameter(separator: String): String =
    apiKey.map(k => s"${separator}api_key=$k").getOrElse("")

  /** Every TMDB call routes through here. TMDB's API 5xxs and times out for a
   *  few minutes now and then (it took down a CI integration run on
   *  `/movie/{id}/external_ids`); a transient blip almost always succeeds on the
   *  next try, so retry it a couple of times with a short backoff. A 4xx — a 404
   *  for an id/title TMDB doesn't know — is NOT transient and fails fast (no point
   *  burning the remaining attempts). A `None`/empty parse result isn't an
   *  exception, so the existing graceful-degradation paths are untouched. */
  private def httpGet(url: String, auth: Map[String, String]): String =
    RetryWithBackoff("TMDB GET", maxAttempts = 3, initialBackoff = 300.millis,
      retryOn = TmdbClient.isTransient)(http.get(url, auth))

  /**
   * Resolve a Polish title (+ optional year) to its best-match TMDB record.
   * First tries a year-restricted search; if that's empty (common — the year a
   * cinema reports as "release year" can be the production year while TMDB
   * stores the theatrical date, off by 1 in either direction), falls back to a
   * year-less search and picks the candidate whose own year is closest to the
   * requested one. Ties broken by popularity.
   */
  private def searchOnce(title: String, yearParameter: Option[Int], auth: Map[String, String]): Seq[TmdbClient.SearchResult] = {
    val yp = yearParameter.map(y => s"&year=$y&primary_release_year=$y").getOrElse("")
    val url = s"$ApiBase/search/movie?language=$languageTag&include_adult=false&query=${urlEncode(title)}$yp${apiKeyParameter("&")}"
    parseSearchResults(httpGet(url, auth))
  }

  def search(
    title:             String,
    year:              Option[Int],
    referenceSynopsis: Option[String] = None,
    // Cinema-reported director(s), comma-separated. When present it corroborates a
    // NON-exact year-less / banner-stripped hit (see `corroboratedYearless`); when
    // absent only an exact-title hit survives those riskier tiers. Optional with a
    // default so existing callers compile unchanged.
    director:          Option[String] = None
  ): Option[TmdbClient.SearchResult] = authHeader.flatMap { auth =>
    // Tier 1 — year-restricted. Most precise: TMDB only returns films of that
    // year, so a non-exact pickBest hit there is same-year and safe. Unchanged.
    //   - "Camper" (year=None) → would otherwise pick "Sleepaway Camper"
    //   - "Odlot"  (year=None) → would otherwise pick Pixar's "Up" (pop 21)
    val yearScoped = if (year.isDefined) pickBest(searchOnce(title, year, auth), title, year, referenceSynopsis) else None

    // Tier 2 — year-less retry on the RAW title. Broader than Tier 1 (the cinema's
    // "year" can be a re-release/production year TMDB doesn't index), so it accepts
    // ONLY a corroborated candidate: an exact-title match, or — when a director is
    // reported — a hit whose TMDB credits overlap it. A bare fuzzy/year-distance hit
    // is REFUSED here (that's the "Zaproszenie / Sleepaway Camper" mis-resolve).
    def yearlessRaw = corroboratedYearless(title, year, referenceSynopsis, director, auth)

    // Tier 3 — banner-stripped / dash-normalised query VARIANTS, for decorated
    // exhibitor titles the raw query can't find ("POKAZ | Film (2024)", an em-dash
    // where TMDB indexes a hyphen). Each variant is tried year-restricted (exact
    // only) then year-less (corroborated), with the same precision gate as Tier 2.
    def variantRecovery = TmdbClient.bannerStrippedVariants(title).iterator.flatMap { variant =>
      val vScoped =
        if (year.isDefined)
          pickBest(searchOnce(variant, year, auth), variant, year, referenceSynopsis)
            .filter(TmdbClient.isExactTitleMatch(_, variant))
        else None
      vScoped.orElse(corroboratedYearless(variant, year, referenceSynopsis, director, auth))
    }.nextOption()

    yearScoped.orElse(yearlessRaw).orElse(variantRecovery)
  }

  /** Year-less search for `query` accepting ONLY a corroborated candidate:
   *    1. pickBest's choice when it's an exact-title match (keeps the exact +
   *       year-distance semantics — e.g. "Odlot" → the year-closest "Odlot");
   *    2. otherwise, when the result set has exactly ONE dated film, that singleton
   *       (the `searchUnique` precedent — an unambiguous lone hit is safe even when
   *       the verbatim title differs, e.g. a decorated cinema query whose only dated
   *       result is the real film, the dateless companion stub having been dropped);
   *    3. otherwise, when the row reports a director, the first hit whose TMDB
   *       credits overlap it.
   *  Returns None on a bare fuzzy hit picked from SEVERAL candidates by
   *  year-distance/popularity — the precision gate the year-less tier needs because
   *  it isn't scoped to a year (the "pick the year-closest of two unrelated films"
   *  mis-resolve). */
  private def corroboratedYearless(
    query:             String,
    year:              Option[Int],
    referenceSynopsis: Option[String],
    director:          Option[String],
    auth:              Map[String, String]
  ): Option[TmdbClient.SearchResult] = {
    val results       = searchOnce(query, None, auth)
    val onlyOneDated  = results.count(_.releaseYear.isDefined) == 1
    pickBest(results, query, year, referenceSynopsis)
      .filter(b => TmdbClient.isExactTitleMatch(b, query) || onlyOneDated)
      .orElse(directorOverlapHit(results, director))
  }

  /** First result whose TMDB credits overlap the cinema-reported director(s).
   *  None (and no HTTP) when no director is reported — so production rows that
   *  don't pass a director, and the precision-guard path, never accept a fuzzy
   *  hit here. `directorsFor` is failure-tolerant (missing credits → empty set →
   *  no match), so an unresolvable candidate is simply skipped. */
  private def directorOverlapHit(
    results:  Seq[TmdbClient.SearchResult],
    director: Option[String]
  ): Option[TmdbClient.SearchResult] = {
    val cinemaNames = director.toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
    if (cinemaNames.isEmpty) None
    else results.find(r => TmdbClient.directorNamesOverlap(cinemaNames, directorsFor(r.id)))
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

  /** Resolve when a YEAR is present AND the year-scoped search returns at least
   *  one EXACT title match (Polish or original). Returns the most-popular exact
   *  match (first after [[parseSearchResults]]'s popularity sort). Broader than
   *  [[searchUnique]] (which needs the whole result set to be a singleton): the
   *  year scopes to the right era and a verbatim title in the results is the
   *  confidence, so a row whose year-scoped search returns several same-year films
   *  ("Sundown" alongside "Sundown Town", "DJ at Sundown") still resolves to the
   *  one the cinema named. Still refuses when NO result is an exact match — that's
   *  the popularity guess [[searchUnique]] exists to avoid. No-op without a year,
   *  so yearless rows stay refused. */
  def searchYearExactTop(title: String, year: Option[Int]): Option[TmdbClient.SearchResult] =
    if (year.isEmpty) None
    else authHeader.flatMap { auth =>
      searchOnce(title, year, auth).filter(TmdbClient.isExactTitleMatch(_, title)).headOption
    }

  /**
   * From a set of TMDB hits, pick the best match for the query.
   *   1. Exact title matches (Polish OR original) win over everything else.
   *   2. Among exact matches (or when none exist), prefer the result closest
   *      to the requested year.
   *   3. When several survivors TIE at that closest year-distance and the caller
   *      supplied `referenceSynopsis` (the row's cinema-published Polish blurb),
   *      the candidate whose TMDB `overview` is the confident closest match to it
   *      wins — a content signal title+year can't provide (same-title films of
   *      the same year). Falls back to the popularity order when no candidate is
   *      a confident winner (see `SynopsisSimilarity.confidentTieBreak`), so the
   *      behaviour is byte-identical to before whenever the synopsis is absent,
   *      ambiguous, or weak.
   *   4. With no year and no tie-break needed, the parseSearchResults
   *      insertion order (popularity-descending) wins.
   */
  private[clients] def pickBest(
    results:           Seq[TmdbClient.SearchResult],
    title:             String,
    year:              Option[Int],
    referenceSynopsis: Option[String] = None
  ): Option[TmdbClient.SearchResult] = {
    // A TMDB entry with NO release date is a stub/placeholder — a making-of, a
    // featurette, an unreleased shell — never the theatrical film a cinema is
    // actually screening. 874482 "Brzezina - Andrzej Wajda o filmie" is exactly
    // this: a dateless, runtime-0, director-less companion entry that exact-matched
    // a decorated cinema title under the OLD un-stripped search and stuck as a
    // SECOND resolved id under "Brzezina", tripping clusterByFilm's ambiguity-refuse
    // so the real film's decorated editions never folded. Drop dateless entries
    // whenever a dated candidate exists so a stub can't win the exact-title match;
    // keep the raw set only when EVERY result is dateless (don't regress an
    // all-stub query to None).
    val dated = results.filter(_.releaseYear.isDefined)
    val pool  = if (dated.nonEmpty) dated else results
    if (pool.isEmpty) None
    else {
      def yearDistance(r: TmdbClient.SearchResult): Int =
        year.flatMap(y => r.releaseYear.map(ry => math.abs(ry - y))).getOrElse(Int.MaxValue)
      val exactMatches = pool.filter(r => TmdbClient.isExactTitleMatch(r, title))
      val candidates = if (exactMatches.nonEmpty) exactMatches else pool
      // Stable sort keeps the popularity order within an equal year-distance, so
      // `tied.head` is exactly the legacy winner.
      val sorted = candidates.sortBy(yearDistance)
      sorted.headOption.map { top =>
        val tied = sorted.takeWhile(r => yearDistance(r) == yearDistance(top))
        if (tied.lengthCompare(1) > 0)
          referenceSynopsis
            .flatMap(ref => SynopsisSimilarity.confidentTieBreak(ref, tied.map(_.overview.getOrElse(""))))
            .map(tied)
            .getOrElse(top)
        else top
      }
    }
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
    parseFindMovieResults(body).headOption
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
  def details(tmdbId: Int): Option[TmdbClient.Details] = authHeader.flatMap { auth =>
    Try(httpGet(s"$ApiBase/movie/$tmdbId?language=en-US&append_to_response=alternative_titles${apiKeyParameter("&")}", auth))
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
   *  Grizzly Falls 1999 and the 2026 Helgestad document) the directors disagree. */
  def directorsFor(tmdbId: Int): Set[String] = authHeader.map { auth =>
    Try {
      val body = httpGet(s"$ApiBase/movie/$tmdbId/credits${apiKeyParameter("?")}", auth)
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

  /** One TMDB `/movie/{id}?language=<deployment>&append_to_response=credits` call
   *  returning everything the TMDB enrichment stage needs to fill a
   *  `SourceData(Tmdb)` slot:
   *    - Deployment-language title + production-language original title
   *    - Deployment-language synopsis (`overview`)
   *    - Director(s) + top-billed cast
   *    - Year (`release_date`) + runtime
   *    - Production countries (raw names — caller canonicalises)
   *    - Poster URL — the best deployment-language portrait poster from `/movie/{id}/images`
   *      (see `posters`), falling back to the default `poster_path` when that
   *      endpoint has no usable portrait variant.
   *
   *  Returns None on network failure / unknown id — callers fall back to
   *  whatever the previous slot held. The detail itself is one round-trip; the
   *  poster lookup adds a single failure-tolerant `/images` call, so a poster
   *  hiccup never breaks the resolve. */
  def fullDetails(tmdbId: Int): Option[TmdbClient.FullDetails] = authHeader.flatMap { auth =>
    Try(httpGet(s"$ApiBase/movie/$tmdbId?language=$languageTag&append_to_response=credits${apiKeyParameter("&")}", auth))
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
        // Deployment-language genre names. TMDB returns the `genres` field
        // pre-localised in the requested language ("Dramat"/"Drama") —
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
          // Prefer the best deployment-language portrait poster from `/images` over the
          // default `poster_path` (which is whatever TMDB flags primary,
          // regardless of language or shape). This poster only ever surfaces
          // as a backup — the Tmdb slot ranks below every cinema in
          // `MovieRecord.posterUrl` — so a better localised portrait improves
          // the `data-fallbacks` chain (and the no-cinema-poster case).
          posterUrl     = TmdbClient.bestPortraitPosterUrl(posters(tmdbId))
                            .orElse((js \ "poster_path").asOpt[String].filter(_.nonEmpty).map(p => s"${TmdbClient.PosterBase}$p"))
        )
      }
  }

  /** Deployment-language + language-neutral poster variants for a movie, from
   *  TMDB's `/movie/{id}/images` endpoint. Each carries its language tag,
   *  aspect ratio and community vote, so `bestPortraitPosterUrl` can choose
   *  the best *portrait* localised poster — a better backup than the default
   *  `poster_path`, which is whatever TMDB flags primary regardless of shape.
   *
   *  `include_image_language=<lang>,null` restricts the response to the
   *  deployment language's artwork plus language-neutral. Wrapped so any failure — network, or a
   *  missing fixture on test replay — yields an empty list; callers then fall
   *  back to `poster_path` exactly as before. */
  def posters(tmdbId: Int): Seq[TmdbClient.PosterImage] = authHeader.map { auth =>
    Try {
      val body = httpGet(s"$ApiBase/movie/$tmdbId/images?include_image_language=$imageLanguages${apiKeyParameter("&")}", auth)
      TmdbClient.parsePosters(body)
    }.getOrElse(Seq.empty)
  }.getOrElse(Seq.empty)

  /** TMDB person id for a name search, or None when the search returns no
   *  Directing-known hit. Picks the highest-popularity match whose
   *  `known_for_department` is "Directing" — keeps us from matching an actor
   *  who happens to share a name with a director. */
  def findPerson(name: String): Option[Int] = authHeader.flatMap { auth =>
    Try {
      val body = httpGet(s"$ApiBase/search/person?query=${urlEncode(name)}${apiKeyParameter("&")}", auth)
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
      val body = httpGet(s"$ApiBase/person/$personId/movie_credits?language=$languageTag${apiKeyParameter("&")}", auth)
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

  private def decodeMovieArray(array: JsArray): Seq[TmdbClient.SearchResult] =
    array.value.flatMap { js =>
      for {
        id <- (js \ "id").asOpt[Int]
      } yield TmdbClient.SearchResult(
        id            = id,
        title         = (js \ "title").asOpt[String].getOrElse(""),
        originalTitle = (js \ "original_title").asOpt[String],
        releaseYear   = (js \ "release_date").asOpt[String].filter(_.length >= 4).map(_.take(4).toInt),
        popularity    = (js \ "popularity").asOpt[Double].getOrElse(0.0),
        overview      = (js \ "overview").asOpt[String].filter(_.nonEmpty)
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
  // Widest aspect ratio (width/height, TMDB's convention) we still treat as a
  // portrait poster. A 2:3 poster is ~0.667; anything above this is squarer or
  // landscape and would crop badly in the 2:3 card, so we skip it.
  private val MaxPortraitAspectRatio = 0.72

  val ApiKey: Option[String] = Env.get("TMDB_API_KEY")

  /** The request language when a construction doesn't specify one. Polish keeps
   *  every existing single-country (Poland-only) call site — and every test
   *  fixture keyed on `language=pl-PL` URLs — byte-identical. */
  val DefaultLanguage: Locale = Locale.forLanguageTag("pl-PL")

  /** A TMDB HTTP failure worth retrying: a 5xx server error or a 429 rate-limit
   *  (transient), or any network-level IOException — `HttpTimeoutException`,
   *  `ConnectException`, a reset socket. A 4xx other than 429 (a 404 for an id
   *  TMDB doesn't know, a 401 for a bad key) is permanent and must NOT retry.
   *
   *  A `FileNotFoundException` is excluded even though it's an `IOException`: prod's
   *  `RealHttpFetch` never throws it, but the fixture-replay `FakeHttpFetch` throws
   *  it on a missing fixture — a PERMANENT miss (matching `TestWiring`'s
   *  "missing fixture is permanent" rule). Retrying it just storms the replay harness
   *  with thousands of pointless backoff sleeps. */
  private[clients] def isTransient(t: Throwable): Boolean = t match {
    case _: java.io.FileNotFoundException => false
    case e: HttpStatusException           => e.code >= 500 || e.code == 429
    case _: java.io.IOException           => true
    case _                                => false
  }

  /** The cross-reference ids TMDB's `/external_ids` endpoint carries for a movie. */
  case class ExternalIds(imdbId: Option[String], wikidataId: Option[String])

  case class SearchResult(
    id:            Int,
    title:         String,
    originalTitle: Option[String],
    releaseYear:   Option[Int],
    popularity:    Double,
    // Deployment-language `overview` from the search row — carried so `pickBest`
    // can break a same-year same-title tie on synopsis closeness without an extra
    // HTTP call (`/search/movie?language=<deployment>` already returns it). Empty
    // for the person-credits decoder, which never feeds the synopsis tie-break.
    overview:      Option[String] = None
  )

  /** A TMDB hit whose Polish OR original title matches the query, ignoring case,
   *  surrounding whitespace AND punctuation — the exactness both
   *  [[TmdbClient.pickBest]] and [[TmdbClient.searchYearExactTop]] gate on.
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
    q.nonEmpty && (titleMatchKey(r.title) == q || r.originalTitle.exists(titleMatchKey(_) == q))
  }

  /** Fold every Unicode dash variant a cinema might emit — hyphen-minus through
   *  em-dash/horizontal-bar, the math minus sign (U+2212), and the fullwidth /
   *  small CJK-compat forms — to a plain ASCII hyphen. `titleMatchKey` already
   *  strips dashes for exact-match comparison, so this matters at the QUERY level:
   *  TMDB's search tokeniser treats "Wicked — Część 2" and "Wicked - Część 2" as
   *  different strings, so a verbatim em-dash query can miss a film that a
   *  hyphenated one finds. The normalised form is offered as a query variant. */
  private val DashVariants =
    java.util.regex.Pattern.compile("[\\u2010-\\u2015\\u2212\\u2043\\uFE58\\uFE63\\uFF0D]")
  private[clients] def normalizeDashes(s: String): String =
    DashVariants.matcher(s).replaceAll("-")

  /** Trailing parenthetical at the end of a title — "Freak Show (AD)", "Wymazać
   *  (2024)". Stripped to recover the bare film title. */
  private val TrailingParenthetical =
    java.util.regex.Pattern.compile("\\s*\\([^()]*\\)\\s*$")

  /** Decoration-stripped query variants for a decorated exhibitor title, used by
   *  `search`'s Tier-3 recovery. Folds dash variants, splits a "Banner | Film"
   *  pipe into each side, and drops a trailing parenthetical from each piece. The
   *  raw `title` itself is excluded — Tiers 1/2 already tried it — so an
   *  undecorated title yields no variants (Tier 3 is a no-op for it). Deliberately
   *  self-contained: no cross-client title-normalisation import. */
  private[clients] def bannerStrippedVariants(title: String): Seq[String] = {
    val dashNormalized = normalizeDashes(title).trim
    val pieces         = (Seq(dashNormalized) ++ dashNormalized.split("\\|").toSeq)
      .map(_.trim).filter(_.nonEmpty)
    pieces
      .flatMap(p => Seq(p, TrailingParenthetical.matcher(p).replaceAll("").trim))
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct
      .filterNot(_ == title)
  }

  /** Loose director-name overlap between the cinema's reported names and a TMDB
   *  film's credited directors: punctuation/case-blind, matching when either
   *  collapsed name contains the other (so "Asgeir Helgestad" ties TMDB's same
   *  name, and a single cinema name ties one of several co-directors). Self-
   *  contained — the richer token-set match lives in MovieService and isn't
   *  imported here. */
  private[clients] def directorNamesOverlap(cinemaNames: Seq[String], tmdbDirectors: Set[String]): Boolean = {
    def key(s: String): String = titleMatchKey(s)
    val cinemaKeys = cinemaNames.map(key).filter(_.nonEmpty)
    val tmdbKeys   = tmdbDirectors.map(key).filter(_.nonEmpty)
    cinemaKeys.exists(c => tmdbKeys.exists(t => c.contains(t) || t.contains(c)))
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
    posterUrl:      Option[String]
  )

  /** One poster variant from `/movie/{id}/images`. `aspectRatio` is
   *  width/height (TMDB's convention) — a 2:3 portrait poster is ~0.667.
   *  `language` is the `iso_639_1` tag ("pl"), or None for language-neutral
   *  artwork. */
  case class PosterImage(
    filePath:    String,
    language:    Option[String],
    aspectRatio: Double,
    voteAverage: Double,
    width:       Int
  )

  private[clients] def parsePosters(body: String): Seq[PosterImage] =
    (Json.parse(body) \ "posters").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap { js =>
        (js \ "file_path").asOpt[String].filter(_.nonEmpty).map { fp =>
          PosterImage(
            filePath    = fp,
            language    = (js \ "iso_639_1").asOpt[String].filter(_.nonEmpty),
            aspectRatio = (js \ "aspect_ratio").asOpt[Double].getOrElse(0.0),
            voteAverage = (js \ "vote_average").asOpt[Double].getOrElse(0.0),
            width       = (js \ "width").asOpt[Int].getOrElse(0)
          )
        }
      }

  /** Pick the best *portrait* poster URL from a `/images` poster set,
   *  preferring Polish-tagged artwork. Drops landscape/square variants
   *  (aspect ratio above `MaxPortraitAspectRatio`), then orders by
   *  (Polish first, highest community vote, then resolution) and returns the
   *  winner as a w500 CDN URL. None when nothing portrait survives — the
   *  caller falls back to `poster_path`. */
  def bestPortraitPosterUrl(posters: Seq[PosterImage]): Option[String] =
    posters
      .filter(p => p.aspectRatio > 0 && p.aspectRatio <= MaxPortraitAspectRatio)
      .sortBy(p => (if (p.language.contains("pl")) 0 else 1, -p.voteAverage, -p.width))
      .headOption
      .map(p => s"$PosterBase${p.filePath}")

  private[clients] def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
