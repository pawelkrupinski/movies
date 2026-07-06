package services.enrichment

import play.api.libs.json._
import tools.{HttpFetch, SynopsisSimilarity, TextNormalization}

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
class FilmwebClient(http: HttpFetch) {
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
    title:             String,
    year:              Option[Int]    = None,
    fallback:          Option[String] = None,
    directors:         Set[String]    = Set.empty,
    referenceSynopsis: Option[String] = None
  ): Option[FilmwebInfo] = {
    // Try the raw title first, then banner/cycle-stripped variants (so a title
    // polluted with a screening cycle — "Film | Poniedziałki z Konwickim: …" —
    // still surfaces the bare film), then the caller's fallback. `view` keeps it
    // lazy: an undecorated title yields no variants and costs no extra requests,
    // and the first query that resolves short-circuits the rest.
    val variants = searchQueryVariants(title)
    val effectiveFallback = fallback.filter(f => !f.equalsIgnoreCase(title) && !variants.exists(_.equalsIgnoreCase(f)))
    val queries = (title +: (variants ++ effectiveFallback.toSeq)).distinct
    queries.view.flatMap(q => lookupWithQuery(q, year, directors, referenceSynopsis)).headOption
  }

  private def lookupWithQuery(query: String, year: Option[Int], directors: Set[String], referenceSynopsis: Option[String]): Option[FilmwebInfo] =
    Try {
      val hits = search(query).take(MaxCandidates)
      // Step 1 (cheap): /info per candidate → title acceptance bar.
      val infoCandidates = hits.flatMap(h =>
        info(h.id).map(i => Candidate(h.id, h.kind, i.title, i.originalTitle, i.year, Set.empty, Seq.empty))
      )
      // Step 2 (only when caller wants director verification): /preview per
      // candidate. We fetch for ALL candidates, not just the title-accepted
      // ones, because a candidate whose Filmweb title is a transliteration of
      // the query ("Mavka. Spravzhnij mif" for "Mawka. Prawdziwy mit") fails
      // the title bar yet is confirmable by director+year — and that override
      // (in `pickBest`) needs the director data /preview carries.
      // `/preview` carries both directors AND genres; we fold both onto the
      // candidate in one shot so director-verification + genre extraction
      // share the same round-trip.
      val candidates =
        if (directors.isEmpty) infoCandidates
        else infoCandidates.map { c =>
          preview(c.id) match {
            case Some(p) => c.copy(directors = p.directors, genres = p.genres, plot = p.plot)
            case None    => c
          }
        }
      pickBest(candidates, query, year, directors, referenceSynopsis).map { c =>
        val url = canonicalUrl(c.id, c.kind, c.title, c.year)
        // Fall back to a winner-only /preview when director verification was
        // skipped (caller didn't supply directors) so the Filmweb slot carries
        // its full content (directors + genres + plot) — single extra round-trip,
        // only on the winner. Skipped when `c` already carries that data from the
        // director-verification path.
        val winnerPreview =
          if (c.directors.nonEmpty || c.genres.nonEmpty || c.plot.isDefined) None
          else preview(c.id)
        FilmwebInfo(
          url           = url,
          rating        = rating(c.id),
          genres        = if (c.genres.nonEmpty) c.genres else winnerPreview.map(_.genres).getOrElse(Seq.empty),
          title         = Some(c.title),
          originalTitle = c.originalTitle,
          year          = c.year,
          directors     = (if (c.directors.nonEmpty) c.directors else winnerPreview.map(_.directors).getOrElse(Set.empty)).toSeq.sorted,
          plot          = c.plot.orElse(winnerPreview.flatMap(_.plot))
        )
      }
    }.toOption.flatten

  /** Returns search hits (id + kind) in the order Filmweb ranks them.
   *  Both `film` and `serial` types are kept — some content cinemas screen
   *  as a one-off feature is filed under `/serial/` on Filmweb (children's
   *  episodic shows shown as a single screening, e.g. "Kicia Kocia w
   *  podróży"). The shared `/api/v1/film/{id}/info`+`/rating` endpoints
   *  serve both, so downstream handling is uniform; only the canonical
   *  page URL prefix differs (`/film/` vs `/serial/`). */
  def search(title: String): Seq[SearchHit] =
    // Query Filmweb's fuzzy `live/search` with a case- AND diacritic-folded title
    // (`deburr` = TextNormalization.deburr + lowercase). The endpoint is itself
    // case/diacritic-insensitive — "slodkie zycie" and "Słodkie życie" both return
    // film 30940 — so this loses no matches, while making the request (hence the
    // recorded-fixture fingerprint) INDEPENDENT of the caller's exact spelling. That
    // decouples the rating query from the canonical title's casing, so a settle that
    // re-spells `minSpelling`→`displayTitle` no longer changes which fixture is hit.
    parseSearch(http.get(s"$ApiBase/live/search?query=${urlEncode(deburr(title))}"))

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
    idFromUrl(url).flatMap(rating)

  /** Genres for a stored canonical Filmweb URL — the `/preview` half of what
   *  `lookup` returns, recovered from the URL alone (no search round-trips).
   *  Used when a cached url-discovery hit needs to rebuild the Filmweb slot. */
  def genresFor(url: String): Seq[String] =
    idFromUrl(url).flatMap(preview).map(_.genres).getOrElse(Seq.empty)

  /** Rebuild the FULL Filmweb metadata for a stored canonical URL — the
   *  resolution-cache HIT path, where only the URL survived (the search + pickBest
   *  ran on a prior enrichment). /info (title, originalTitle, year) + /preview
   *  (directors, genres, plot) + /rating, all keyed off the id parsed from the URL,
   *  so a cached hit still populates the same content slot a fresh `lookup` would. */
  def detailsFor(url: String): Option[FilmwebInfo] =
    idFromUrl(url).map { id =>
      val i = info(id)
      val p = preview(id)
      FilmwebInfo(
        url           = url,
        rating        = rating(id),
        genres        = p.map(_.genres).getOrElse(Seq.empty),
        title         = i.map(_.title),
        originalTitle = i.flatMap(_.originalTitle),
        year          = i.flatMap(_.year),
        directors     = p.map(_.directors.toSeq.sorted).getOrElse(Seq.empty),
        plot          = p.flatMap(_.plot)
      )
    }

  private def idFromUrl(url: String): Option[Int] =
    FilmwebClient.IdFromUrl.findFirstMatchIn(url).flatMap(m => Try(m.group(1).toInt).toOption)

  def parseSearch(body: String): Seq[SearchHit] =
    (Json.parse(body) \ "searchHits").asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap { js =>
      val kind = (js \ "type").asOpt[String]
      val id   = (js \ "id").asOpt[Int]
      for { k <- kind if k == "film" || k == "serial"; i <- id } yield SearchHit(i, k)
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
    // Filmweb's `genres` shape: [{id, name: {text}, nameKey}]. `name.text`
    // is the Polish display label ("Dramat", "Sci-Fi", "Biograficzny").
    val genres = (json \ "genres").asOpt[JsArray].map(_.value).getOrElse(Nil)
      .flatMap(j => (j \ "name" \ "text").asOpt[String].filter(_.nonEmpty))
      .toSeq
    // Filmweb nests the Polish plot under `plot.synopsis` (alongside sourceType
    // + author). Feeds the synopsis tie-break in `pickBest` AND is stored on the
    // Filmweb slot as a Polish synopsis fallback for films TMDB/IMDb didn't cover.
    val plot = (json \ "plot" \ "synopsis").asOpt[String].filter(_.nonEmpty)
    FilmPreview(directors, genres, plot)
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
   *  Director+year override: a candidate whose title does NOT match is still
   *  accepted when its director overlaps the caller's AND its year is within
   *  ±1 of the caller's. This recovers films Filmweb files under a
   *  transliterated/original title the cinema doesn't report ("Mavka.
   *  Spravzhnij mif" for "Mawka. Prawdziwy mit"). Both signals are required —
   *  never override on director alone — to keep precision.
   *
   *  Year breaks ties among accepted candidates; a `referenceSynopsis` (the
   *  row's TMDB Polish blurb) breaks a remaining tie on plot closeness.
   */
  def pickBest(
    candidates:        Seq[Candidate],
    query:             String,
    year:              Option[Int],
    directors:         Set[String],
    referenceSynopsis: Option[String] = None
  ): Option[Candidate] = {
    if (candidates.isEmpty || query.trim.isEmpty) return None
    val directorAccepted = candidates.filter { c =>
      (matchesByTitle(c, query) && matchesByDirector(c, directors) && yearGateOk(c, query, year, directors)) ||
        matchesByDirectorAndYear(c, year, directors)
    }
    // Prefer a `film` over a same-title `serial` BEFORE year-distance: a real
    // film and a TV series share many titles ("Ziemia obiecana" — Wajda's 1974
    // film + a 1975 series; "Beavis i Butt-Head"), and the year-closest hit was
    // often the series. A cinema is screening the film, so the film wins when one
    // clears the bar; a `serial` is still used when it's the only match (Polish
    // children's shows — Kicia Kocia, Basia, Pucio — are filed only as serials).
    def sortKey(c: Candidate): (Boolean, Int) =
      (c.kind != "film", year.flatMap(y => c.year.map(yy => math.abs(yy - y))).getOrElse(Int.MaxValue))
    val sorted = directorAccepted.sortBy(sortKey)
    // Among candidates TIED at the best (film-first, year-distance) key, the one
    // whose Filmweb `plot` is the confident closest match to the TMDB blurb wins;
    // with no reference, no plots, or no confident winner, the stable-sort head
    // (legacy behaviour) stands.
    sorted.headOption.map { top =>
      val tied = sorted.takeWhile(c => sortKey(c) == sortKey(top))
      if (tied.lengthCompare(1) > 0)
        referenceSynopsis
          .flatMap(ref => SynopsisSimilarity.confidentTieBreak(ref, tied.map(_.plot.getOrElse(""))))
          .map(tied)
          .getOrElse(top)
      else top
    }
  }

  private[enrichment] def matchesByTitle(c: Candidate, query: String): Boolean = {
    val normalizedQuery = normalizeTitle(query)
    val titles = (c.title +: c.originalTitle.toSeq).map(normalizeTitle)
    titles.exists(_ == normalizedQuery) || titles.exists(t => MetacriticClient.isModifierSuffix(t, normalizedQuery))
  }

  private[enrichment] def matchesByDirector(c: Candidate, directors: Set[String]): Boolean =
    directors.isEmpty || c.directors.isEmpty || directorsOverlap(c.directors, directors)

  /** True iff `c`'s canonical title (or originalTitle) EQUALS the query — the
   *  strict half of [[matchesByTitle]], excluding the looser modifier-suffix
   *  branch. */
  private[enrichment] def matchesByExactTitle(c: Candidate, query: String): Boolean = {
    val normalizedQuery = normalizeTitle(query)
    (c.title +: c.originalTitle.toSeq).map(normalizeTitle).contains(normalizedQuery)
  }

  /** False-positive guard for the no-director path. When neither the caller nor
   *  the merge gives us a director to disambiguate AND the row carries a year,
   *  an EXACT same-title candidate must sit within ±1 of that year — a large gap
   *  is the same-title-different-film collision signature (a yearless "Konwicki:
   *  Lawa (1989)" screening must not take the unrelated "Lawa"/orig "Lava" 2014;
   *  "Belle" 1973 must not answer a 2022 query). Exemptions, all already
   *  disambiguated by something other than the bare title+year:
   *    - caller HAS directors → director verification / the director+year
   *      override decides, not this gate;
   *    - year is unknown → nothing to gate against (legacy first-hit behaviour);
   *    - the match is a modifier-suffix, not exact → a "Title - Re-Release"
   *      legitimately re-dates and must survive;
   *    - the candidate carries no year → can't be gated. */
  private[enrichment] def yearGateOk(c: Candidate, query: String, year: Option[Int], directors: Set[String]): Boolean =
    directors.nonEmpty || year.isEmpty || !matchesByExactTitle(c, query) ||
      c.year.forall(cy => math.abs(cy - year.get) <= 1)

  /** Strict director+year override: accepts a candidate even when its title
   *  doesn't match, provided the caller supplied directors AND a year, the
   *  candidate carries directors that overlap, AND its year is within ±1 of
   *  the caller's. Both signals are mandatory — director alone never overrides
   *  (multiple unrelated films can share a director across years). */
  private[enrichment] def matchesByDirectorAndYear(c: Candidate, year: Option[Int], directors: Set[String]): Boolean =
    directors.nonEmpty && c.directors.nonEmpty &&
      year.exists(y => c.year.exists(cy => math.abs(cy - y) <= 1)) &&
      directorsOverlap(c.directors, directors)

  /** Diacritic-stripped, case-folded director-name overlap; substring match in
   *  either direction passes so "M. Szumowska" hits "Małgorzata Szumowska". */
  private def directorsOverlap(filmwebDirectors: Set[String], callerDirectors: Set[String]): Boolean = {
    val callerNorms = callerDirectors.map(deburr)
    val fwNorms     = filmwebDirectors.map(deburr)
    callerNorms.exists(d => fwNorms.exists(f => d == f || d.contains(f) || f.contains(d)))
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

  /** /preview response — director names + Polish genre labels + the Polish
   *  plot blurb (`plot.synopsis`). `plot` feeds the synopsis tie-break in
   *  [[FilmwebClient.pickBest]] and is stored on the Filmweb slot as a Polish
   *  synopsis fallback. Other /preview fields (cast, duration, countries) are
   *  present in the JSON but unused. */
  case class FilmPreview(directors: Set[String], genres: Seq[String] = Seq.empty, plot: Option[String] = None)

  /** Resolved Filmweb metadata for a film — the Filmweb counterpart of a TMDB /
   *  IMDb content slot. Canonical URL + optional 1–10 user rating + Polish genre
   *  labels, plus the winner's `/info` + `/preview` content: canonical `title`,
   *  `originalTitle`, `year`, `directors`, and the Polish `plot`. The extra fields
   *  are empty when Filmweb didn't return them; callers persist whatever is present
   *  onto the Filmweb `SourceData` slot so it can back the display fields and drive
   *  re-resolution when TMDB/IMDb are absent. */
  case class FilmwebInfo(
    url:           String,
    rating:        Option[Double],
    genres:        Seq[String]    = Seq.empty,
    title:         Option[String] = None,
    originalTitle: Option[String] = None,
    year:          Option[Int]    = None,
    directors:     Seq[String]    = Seq.empty,
    plot:          Option[String] = None
  )

  /** One search hit — id + Filmweb's content-type tag (`film` or `serial`).
   *  The same `/api/v1/film/{id}/info` and `/rating` endpoints serve both,
   *  so the type only matters when building the canonical page URL. */
  case class SearchHit(id: Int, kind: String)

  /** One candidate distilled from /info + (optionally) /preview. `kind`
   *  is the `film`/`serial` tag from `SearchHit`. `directors` is empty
   *  when the caller didn't ask for verification — we then skip the
   *  /preview HTTP entirely. */
  case class Candidate(
    id:            Int,
    kind:          String,
    title:         String,
    originalTitle: Option[String],
    year:          Option[Int],
    directors:     Set[String],
    genres:        Seq[String] = Seq.empty,
    // Polish plot blurb from /preview, present only on candidates whose
    // /preview was fetched (the director-verification path). Feeds the synopsis
    // tie-break in `pickBest`; empty otherwise (then the tie-break no-ops).
    plot:          Option[String] = None
  )

  /**
   * Build the canonical page URL the way Filmweb encodes it. The site replaces
   * spaces with `+` and percent-encodes everything else. `kind` picks the
   * URL segment: `film` → `/film/`, `serial` → `/serial/`.
   */
  def canonicalUrl(id: Int, kind: String, title: String, year: Option[Int]): String = {
    val slug = URLEncoder.encode(title, StandardCharsets.UTF_8).replace("%20", "+")
    val y    = year.map(_.toString).getOrElse("")
    s"https://www.filmweb.pl/$kind/$slug-$y-$id"
  }

  private def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  /** Lower-case + trim + fold every dash variant to '-', for title-equality
   *  comparison. Dash folding is shared with the MC/RT/IMDb matchers via
   *  [[MetacriticClient.foldDashes]]. Unlike [[deburr]] this keeps diacritics —
   *  Filmweb titles carry them and the `/info` titles we compare against do too. */
  private[enrichment] def normalizeTitle(s: String): String =
    MetacriticClient.foldDashes(s).toLowerCase.trim

  /** Extra search queries to try when the raw title carries a screening-cycle or
   *  festival banner the bare film title doesn't. Splits on `|` (each segment is
   *  a candidate title) and strips trailing parentheticals. Returns ONLY the
   *  variants that differ from the raw title — an undecorated title yields an
   *  empty sequence, so the common path costs no extra Filmweb requests. */
  def searchQueryVariants(title: String): Seq[String] = {
    val trimmed = title.trim
    val noParen = trimmed.replaceAll("\\s*\\([^)]*\\)", "").trim
    val pipeSegments = trimmed.split("\\|").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    (pipeSegments :+ noParen).map(_.trim).filter(_.nonEmpty).distinct.filterNot(_.equalsIgnoreCase(trimmed))
  }

  /** Lower-case + strip diacritics for fuzzy director comparison. Cinemas
   *  sometimes drop diacritics ("Malgorzata Szumowska"); Filmweb keeps them
   *  ("Małgorzata Szumowska"). Normalising both sides catches that case. */
  private def deburr(s: String): String = TextNormalization.deburr(s).toLowerCase.trim
}
