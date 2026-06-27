package models

import java.util.Locale

/**
 * Cross-cinema metadata about a film — ratings, IDs, and the per-source
 * content slots used to derive the merged display values. Looked up by
 * (cleaned title + year) once and then shared across every cinema-screening
 * of that same film.
 *
 * `imdbId` is optional because TMDB sometimes returns a hit for which it has
 * no IMDb cross-reference yet (very recent releases, niche films). The
 * MovieRecord is still useful — TMDB id + Filmweb / Metacritic / RT URLs all
 * work without an IMDb id; only the IMDb rating + IMDb link are unavailable.
 * The daily TMDB-retry tick re-checks rows with imdbId=None so they get
 * filled when IMDb eventually indexes the film.
 *
 * Per-source content (synopsis, cast, director, year, countries, …) lives in
 * `data: Map[Source, SourceData]`. Cinemas contribute slots via
 * `recordCinemaScrape`; the TMDB and IMDb enrichment stages contribute their
 * own slots keyed by `Tmdb` / `Imdb`. Merged values are derived on the fly
 * by the accessors below — Multikino first, then the rest of `Cinema.all`,
 * then `Tmdb`, then `Imdb`. Data is *added* to the map, never folded into
 * top-level fields, so each source's contribution stays inspectable.
 */
case class MovieRecord(
  // ── Single-source IDs, ratings, and external-site URLs ────────────────────
  // Each of these has exactly one canonical writer in the codebase, so they
  // stay top-level Option rather than going through the per-source map.
  imdbId:            Option[String]   = None,
  imdbRating:        Option[Double]   = None,
  metascore:         Option[Int]      = None,
  filmwebUrl:        Option[String]   = None,
  filmwebRating:     Option[Double]   = None,
  rottenTomatoes:    Option[Int]      = None,
  tmdbId:            Option[Int]      = None,
  metacriticUrl:     Option[String]   = None,
  rottenTomatoesUrl: Option[String]   = None,
  // Cased + decoration-stripped title, generated once at scrape ingestion
  // (`apiQuery(recase(<cinema slot title>))`). External-metadata services
  // (Filmweb / Metacritic / RT / IMDb suggestion) query with this stable
  // value instead of recomputing `apiQuery` over the now-raw cleanTitle.
  searchTitle:       Option[String]   = None,

  // ── Enrichment-conclusion markers (persisted) ─────────────────────────────
  // Gate read-model projection: a row is published only once its enrichment
  // has *concluded* — cinema detail done (where deferred) AND TMDB reached a
  // definitive answer (a hit, i.e. `tmdbId` set, or `tmdbNoMatch`). A purely
  // transient TMDB failure leaves both false, so the row stays held back and
  // keeps retrying. See `readyToProject`.
  tmdbNoMatch:       Boolean          = false,
  detailPending:     Boolean          = false,

  // Per-source data from the most recent refresh. Cinemas contribute on
  // every scrape tick (their slot gets replaced wholesale, and dropped if
  // the film leaves their listings); `Tmdb`/`Imdb` slots come from the
  // enrichment pipeline (and are kept until something re-enriches the row).
  // Merged top-level values (posterUrl, synopsis, …) are computed on the
  // fly from this map — see the accessors below.
  //
  // `cinemaTitles` is derived from the cinema slots' `title` values —
  // there's no separate per-tick provenance store, so a cinema that drops
  // a film mid-rotation loses its title variant from the derived view.
  // Acceptable trade-off: `displayTitle` falls back to the cache-key
  // `cleanTitle` anchor when no slot has a better candidate.
  data:              Map[Source, SourceData] = Map.empty,

  // Longest synopsis ever seen per source, kept alive after the source's live
  // slot is pruned (the cinema stopped listing the film). Unlike everything
  // else in `data`, which vanishes the tick a cinema drops the film, this
  // survives so the displayed `synopsis` stays STICKY: the best blurb we ever
  // had keeps showing instead of flipping to whatever shorter text remains.
  // Captured at the prune site (`MovieCache`) and unioned on merge
  // (`MovieRecordMerge`). Dropped only when the WHOLE row is deleted
  // (`UnscreenedCleanup`, when no cinema screens the film at all). See
  // `synopsis`.
  retainedSynopses:  Map[Source, String] = Map.empty
) {
  def imdbUrl: Option[String] = imdbId.map(id => s"https://www.imdb.com/title/$id/")
  def tmdbUrl: Option[String] = tmdbId.map(id => s"https://www.themoviedb.org/movie/$id")

  /** Equal-weight average of every external rating we hold, each normalised
   *  to a 0–10 scale so the four sources are comparable (Metacritic and
   *  Rotten Tomatoes are 0–100, so they're divided by 10). Sources without a
   *  value are skipped — present sources share the weight equally — and a
   *  record with no ratings at all scores 0. This is the sort key behind the
   *  grid's "Ocena" (weighted rating) sort order. */
  def weightedRating: Double = {
    val normalised: Seq[Double] = Seq(
      imdbRating,
      filmwebRating,
      metascore.map(_ / 10.0),
      rottenTomatoes.map(_ / 10.0)
    ).flatten
    if (normalised.isEmpty) 0.0 else normalised.sum / normalised.size
  }

  // ── Slices and views ──────────────────────────────────────────────────────

  /** Cinema-only view of `data` — used by the per-cinema controller paths
   *  (filmUrl per cinema, showtimes per cinema) where TMDB/IMDb slots are
   *  irrelevant. */
  def cinemaData: Map[Cinema, SourceData] =
    data.collect { case (c: Cinema, sd) => c -> sd }

  /** Derived view of every raw title currently reported by a cinema for
   *  this record. Empty when no cinema is scraping. */
  def cinemaTitles: Set[String] = cinemaData.values.flatMap(_.title).toSet

  /** A view of this record restricted to the given cinema sources, keeping
   *  every NON-cinema source (TMDB / IMDb / Filmweb) and their retained
   *  synopses untouched. The read-model split ([[services.readmodel.ReadModelProjection]])
   *  uses it to derive a single display-title variant's SYNOPSIS from only the
   *  cinemas that reported that title — while still falling back to the shared
   *  TMDB/IMDb blurb. The shared facts (year, director, cast, ratings, poster)
   *  are taken from the FULL record by the projector, never from this scoped
   *  view, so dropping the other variants' cinema slots here can't narrow them. */
  def scopedToCinemas(cinemas: Set[Cinema]): MovieRecord = {
    def keep(source: Source): Boolean = source match {
      case cinema: Cinema => cinemas.contains(cinema)
      case _              => true
    }
    copy(
      data             = data.filter { case (source, _) => keep(source) },
      retainedSynopses = retainedSynopses.filter { case (source, _) => keep(source) }
    )
  }

  /** Cities whose cinemas currently screen this film, in `City.all` order.
   *  Empty when no listed cinema maps to a city (a stored row with no live
   *  showtimes). The debug page lists the global corpus but the /film page is
   *  city-scoped, so it deep-links each row via the first of these — picking
   *  the page's own city would 404 for any film not playing there. */
  def cities: Seq[City] = City.all.filter(c => cinemaData.keySet.exists(c.cinemaSet.contains))

  // ── Merged top-level values derived from `data` ──────────────────────────
  //
  // Two flavours of merge:
  //   - **Priority-first** (`director`, `runtimeMinutes`, `releaseYear`,
  //     `posterUrl`, `countries`): iterate sources in `Source.priority`
  //     order — Multikino, then the rest of `Cinema.all`, then `Tmdb`, then
  //     `Imdb` — and pick the first non-empty value (or union for
  //     `countries`). This preserves the prior cinema-only behaviour while
  //     letting TMDB/IMDb fill in fields no cinema reported.
  //   - **Longest-wins** (`cast`): pick the longest non-empty value across all
  //     sources. Different sources write different-length lists; the longest
  //     tends to be the most complete.
  //   - **Paragraphed-then-longest** (`synopsis`): prefer a blurb that carries
  //     a paragraph break, falling back to longest among those that tie on it —
  //     a properly broken-up synopsis reads better than a longer wall of text.

  /** Iterate `data` in source-priority order — Multikino first, then the
   *  rest of `Cinema.all`, then `Tmdb`, then `Imdb`. Used by every "first
   *  non-empty" merged accessor. */
  private def prioritized: Seq[(Source, SourceData)] =
    data.toSeq.sortBy { case (s, _) => Source.priority.getOrElse(s, Int.MaxValue) }

  /** Cinema-only iteration in the same priority order — used by accessors
   *  that should *not* fall back to TMDB/IMDb (e.g. `cinemaOriginalTitle`,
   *  which is specifically the cinema-reported English title used as a
   *  TMDB-search hint). */
  private def prioritizedCinema: Seq[(Cinema, SourceData)] =
    cinemaData.toSeq.sortBy { case (c, _) => Source.priority.getOrElse(c, Int.MaxValue) }

  /** Display title for the row, derived deterministically (no scrape-order
   *  dependence) by the shared `TitleNormalizer.chooseDisplay` ladder: dominant
   *  clean form across the per-cinema reported titles → the `Tmdb` slot's Polish
   *  title (when it shares that identity and is well-formed) → the cinema
   *  `preferredDisplay` ladder → `recase`.
   *
   *  Caller supplies cleanTitle (the post-decoration-strip anchor the cache keys
   *  the row by) because the record itself doesn't carry it; it's the fallback
   *  when no cinema is scraping yet (TMDB-only row). */
  def displayTitle(cleanTitle: String): String =
    services.movies.TitleNormalizer.chooseDisplay(
      perCinemaTitles = cinemaData.values.flatMap(_.title).toSeq,
      fallback        = cleanTitle,
      tmdbTitle       = data.get(Tmdb).flatMap(_.title))

  /** TMDB-resolved original (production-language) title. None when TMDB
   *  hasn't filled this row yet. */
  def originalTitle: Option[String] = data.get(Tmdb).flatMap(_.originalTitle)

  /** The titles by which TMDB knows this film: its Polish title, its original
   *  (production-language) title, and its English release title. A film a cinema
   *  lists under its original/English title ("Tangled", "Left-Handed Girl") is
   *  the SAME film as the Polish-keyed row ("Zaplątani") — its key matches one of
   *  these aliases — so the cross-title merge (`FilmCanonicalizer.groupByFilm`)
   *  and alias-aware scrape-landing (`MovieCache.concludedKeyFor`) fold the two
   *  onto one row. The English title matters when `originalTitle` is non-Latin
   *  (Taiwanese, Korean, …) and so doesn't itself match the cinema's English
   *  listing. A decorated edition (dub / "+ Kinoteka dla rodziców") adds words
   *  beyond any alias, so it never matches and stays its own row. Empty until
   *  TMDB resolves. */
  def tmdbTitleAliases: Set[String] =
    data.get(Tmdb).toSet.flatMap((sd: SourceData) => Set(sd.title, sd.originalTitle, sd.englishTitle).flatten)

  /** Every poster URL we know about, de-duplicated, in source-priority order
   *  — Multikino, the rest of `Cinema.all`, then `Tmdb`, then `Imdb` — except
   *  that known "coming soon" placeholders (see [[PlaceholderPoster]]) are
   *  demoted to the very end. So a placeholder is only ever picked when no
   *  real poster (any cinema's or TMDB's) exists. `posterUrl` takes the head;
   *  `fallbackPosterUrls` is the rest. */
  private def postersByPreference: Seq[String] = {
    val all = prioritized.iterator.flatMap(_._2.posterUrl).distinct.toSeq
    val (placeholders, real) = all.partition(PlaceholderPoster.matches)
    real ++ placeholders
  }

  /** Best poster URL across sources: first real poster in priority order,
   *  falling back to a "coming soon" placeholder only when that's all we have. */
  def posterUrl: Option[String] = postersByPreference.headOption

  /** Every poster URL we know about *except* the primary, in preference
   *  order — Cinema City after Multikino, then Helios, Apollo, …, then Tmdb,
   *  then Imdb (placeholders last). The view ships these as a `data-fallbacks`
   *  chain; the client's `onerror` handler pops the next URL until the list is
   *  empty, then falls through to `.no-poster`. Empty when the primary is the
   *  only poster we have.
   *
   *  Why a chain rather than just TMDB: cinema CDNs intermittently
   *  403/404 (Multikino's CDN refuses cross-origin fetches at the
   *  moment; Cinema City has shipped posterLinks to images they
   *  hadn't uploaded yet — `8215S2R.jpg` for Drishyam 3). When the
   *  primary cinema fails we'd rather try another cinema's poster
   *  than jump straight to TMDB; the data already lives in `data`. */
  def fallbackPosterUrls: Seq[String] = postersByPreference.drop(1)

  /** First non-empty trailer URL across cinema sources in priority order.
   *  Cinema-only: Tmdb / Imdb slots currently don't carry trailers. */
  def trailerUrl: Option[String] =
    prioritized.iterator.flatMap(_._2.trailerUrl).nextOption()

  /** Every distinct trailer URL across cinema sources, in source-priority
   *  order. Same cinema giving the same URL across slots collapses; URL
   *  shapes that differ but point to the same video are NOT collapsed here
   *  (different cinemas occasionally surface YouTube / Vimeo links with
   *  different query parameters), the view layer collapses by embed URL via
   *  `TrailerEmbed.embedUrlFor`. */
  def trailerUrls: Seq[String] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]
    prioritized.foreach { case (_, sd) => sd.trailerUrl.foreach(seen.add) }
    seen.toSeq
  }

  /** Best non-empty synopsis across ALL sources (every cinema + TMDB/IMDb),
   *  URL tokens stripped — the global "best blurb we have", surfaced by the
   *  `/debug` view. DISPLAY goes through the city-scoped [[synopsisForCity]]
   *  instead, so a city page never shows a synopsis a cinema in some OTHER city
   *  wrote. See [[bestSynopsis]] for the paragraphed-then-longest selection and
   *  [[synopsisCandidatesFor]] for the (non-shrinking, sticky) candidate pool. */
  def synopsis: Option[String] = bestSynopsis(synopsisCandidatesFor(_ => true))

  /** City-scoped synopsis: the best blurb chosen among ONLY the synopses a
   *  cinema in `city` published for this film, plus the city-independent
   *  metadata sources (TMDB, IMDb, Filmweb). A cinema in another city never
   *  contributes — the Poznań page shows a Poznań cinema's blurb (or TMDB's),
   *  never one a Warszawa cinema wrote. Identical paragraphed-then-longest
   *  selection as [[synopsis]]; only the candidate pool narrows.
   *
   *  Cinema City is the one wrinkle: every venue shares ONE chain-wide detail
   *  slot ([[CinemaCityChain]], see `Cinema.chainDetailVenues`) that belongs to
   *  no city, so it's admitted for `city` only when a Cinema City venue there is
   *  actually screening the film — see [[synopsisAppliesToCity]]. */
  def synopsisForCity(city: City): Option[String] =
    bestSynopsis(synopsisCandidatesFor(synopsisAppliesToCity(city)))

  /** Best synopsis from the city-independent metadata sources ONLY (TMDB / IMDb
   *  / Filmweb — no cinema). The read model's per-film fallback ([[ResolvedMovie]]
   *  stores this as `synopsis`): served wherever no [[synopsisForCity]] override
   *  applies, so a city with no cinema blurb of its own shows TMDB's rather than
   *  leaking another city's cinema text. */
  def synopsisNonCinema: Option[String] =
    bestSynopsis(synopsisCandidatesFor { case _: Cinema => false; case _ => true })

  /** Best synopsis from CINEMA sources ONLY (no TMDB / IMDb / Filmweb). This is
   *  the same-language Polish reference the TMDB/Filmweb resolvers feed to the
   *  synopsis tie-break — comparing a candidate against the row's OWN TMDB blurb
   *  would be circular, so the enrichment sources are excluded. None when no
   *  cinema published a blurb (then the tie-break is simply not applied). */
  def synopsisCinema: Option[String] =
    bestSynopsis(synopsisCandidatesFor { case _: Cinema => true; case _ => false })

  /** Does `source`'s synopsis count for `city`? A venue does iff it's one of
   *  the city's cinemas; a chain-detail source (Cinema City) iff one of its
   *  member venues in this city is screening the film; every non-cinema source
   *  (TMDB / IMDb / Filmweb) always does — its blurb is city-independent. */
  private def synopsisAppliesToCity(city: City)(source: Source): Boolean = source match {
    case cinema: Cinema =>
      city.cinemaSet.contains(cinema) ||
        Cinema.chainDetailVenues.get(cinema).exists { venues =>
          cinemaData.keySet.exists(v => venues.contains(v) && city.cinemaSet.contains(v))
        }
    case _ => true
  }

  /** Best synopsis over `candidates` — the paragraphed-then-longest selection
   *  shared by [[synopsis]], [[synopsisForCity]] and [[synopsisNonCinema]].
   *  "Best" = a paragraphed blurb (one carrying a `\n`/`\n\n` break, see
   *  `ScraperParse.blockText`) beats an unbroken single block, and only among
   *  candidates that tie on that does the longest win — a wall of text reads
   *  worse than a properly broken-up one even when marginally longer.
   *
   *  A CMS-duplicated blurb (some cinemas paste the synopsis N× into one field —
   *  see `SynopsisMarkdown.collapseRepeats`) is collapsed to one copy FIRST, on
   *  the raw value before `stripUrls` (which trims, breaking the exact periodicity
   *  collapse needs), so it can't win the length race on its inflated size. URL
   *  tokens are then stripped (a cinema detail page that inlines a trailer link
   *  folds the bare URL into the blurb) so a link-padded blurb can't win on
   *  length either, and any source left empty (nothing but a link) is dropped.
   *  The length compared is the VISIBLE length — markdown emphasis markers are
   *  stripped for the comparison so a blurb can't win just by carrying more
   *  `<b>`/`<i>` tags. The winner is run through `SynopsisMarkdown.sanitize` so
   *  every consumer (web HTML, mobile `/api/details`, OG card, og:description)
   *  sees well-formed markdown regardless of which source produced it. */
  private def bestSynopsis(candidates: Seq[String]): Option[String] =
    candidates
      .map(tools.SynopsisMarkdown.collapseRepeats)
      .map(tools.TextNormalization.stripUrls)
      .filter(_.nonEmpty)
      .map(candidate => candidate -> tools.SynopsisMarkdown.strip(candidate))
      .sortBy { case (_, plainText) => (if (plainText.contains('\n')) 0 else 1, -plainText.length) }
      .headOption.map { case (candidate, _) => tools.SynopsisMarkdown.sanitize(candidate) }

  /** Synopsis candidates whose source passes `keep`, in source-priority order —
   *  each source's live slot synopsis followed by its retained (post-prune) one.
   *  Candidates come from BOTH the live `data` slots AND `retainedSynopses`
   *  (synopses kept after a cinema dropped the film), so the pool only grows over
   *  the row's life and shrinks only when the whole row is deleted — the result
   *  is sticky/monotonic, never downgrading when a cinema stops listing the film.
   *  Built in priority order (not raw `data`/`Set` iteration, whose order is
   *  JVM/platform-dependent) so that when two sources tie on length the STABLE
   *  `sortBy` in [[bestSynopsis]] keeps the higher-priority source —
   *  deterministic across machines (raw order drifted the whole-corpus snapshot
   *  between a dev box and CI). */
  private def synopsisCandidatesFor(keep: Source => Boolean): Seq[String] =
    (data.keySet ++ retainedSynopses.keySet).toSeq
      .filter(keep)
      .sortBy(s => Source.priority.getOrElse(s, Int.MaxValue))
      .flatMap(s => data.get(s).flatMap(_.synopsis).iterator ++ retainedSynopses.get(s).iterator)

  /** Longest non-empty cast list across all sources (ties broken by source
   *  priority — see `synopsis`). */
  def cast: Seq[String] =
    prioritized.iterator.map(_._2.cast).filter(_.nonEmpty).toSeq.sortBy(-_.length).headOption.getOrElse(Seq.empty)

  /** First non-empty director list across sources in priority order. */
  def director: Seq[String] =
    prioritized.iterator.map(_._2.director).find(_.nonEmpty).getOrElse(Seq.empty)

  /** First non-None runtime across sources in priority order. */
  def runtimeMinutes: Option[Int] =
    prioritized.iterator.flatMap(_._2.runtimeMinutes).nextOption()

  /** First non-None release year across sources in priority order. */
  def releaseYear: Option[Int] =
    prioritized.iterator.flatMap(_._2.releaseYear).nextOption()

  /** TMDB's release year specifically — the authoritative theatrical year for a
   *  resolved film, regardless of what cinemas report (they frequently list the
   *  production year). `None` until the row is TMDB-resolved. */
  def tmdbYear: Option[Int] = data.get(Tmdb).flatMap(_.releaseYear)

  /** The year used for the row's IDENTITY (cache / Mongo key) and DISPLAY: TMDB's
   *  when resolved, else the priority-merged cinema-reported year. TMDB wins so
   *  cinemas disagreeing on production vs theatrical year can't split a resolved
   *  film across two year-keys (the "Dzień objawienia" 2025-vs-2026 split). */
  def resolvedYear: Option[Int] = tmdbYear.orElse(releaseYear)

  /** TMDB enrichment has concluded — a hit (`tmdbId` set) or a definitive
   *  no-match (`tmdbNoMatch`). A purely transient failure leaves both false, so
   *  the row stays held back (`readyToProject` false) and keeps retrying. */
  def tmdbConcluded: Boolean = tmdbId.isDefined || tmdbNoMatch

  /** Cinema detail enrichment is done — true unless a deferred detail fetch is
   *  still outstanding. Inline / no-detail cinemas never set `detailPending`,
   *  so this is trivially true for them. */
  def detailDone: Boolean = !detailPending

  /** Ready to publish to the read model. TMDB must have concluded (a hit, or a
   *  definitive `tmdbNoMatch`) — holding un-concluded rows back is what keeps the
   *  pre-enrichment yearless orphan (`sanitize(title)|`) out of `web_movies`.
   *
   *  Cinema detail (`detailPending`) only gates a row that has NO TMDB data: a
   *  `tmdbNoMatch` row's poster/synopsis can come only from the cinema detail
   *  page, so it waits for that. A TMDB-resolved row already carries TMDB's
   *  poster/synopsis/ratings and its showtimes come from the listing tick, so a
   *  still-pending — or permanently failing — cinema detail must NOT hold it
   *  back; otherwise one flaky detail page hides an otherwise-complete film from
   *  every cinema (the "Dzień objawienia" disappearance). The detail, when it
   *  lands, just adds cinema-specific extras. */
  def readyToProject: Boolean = tmdbConcluded && (tmdbId.isDefined || detailDone)

  /** Polish-language genre names — first non-empty list in genre-priority
   *  order: TMDB → Filmweb → cinema slots (in their normal priority order).
   *  Unlike `countries`, we don't union across sources because each source
   *  has its own taxonomy and spelling (TMDB's "Sci-Fi" vs Helios'
   *  "science fiction"); unioning would surface inconsistent labels for the
   *  same concept. The list is taken verbatim from whichever source wins. */
  def genres: Seq[String] = {
    def slotGenres(s: Source): Seq[String] = data.get(s).map(_.genres).getOrElse(Seq.empty)
    val tmdb    = slotGenres(Tmdb)
    val filmweb = slotGenres(Filmweb)
    if (tmdb.nonEmpty)    tmdb
    else if (filmweb.nonEmpty) filmweb
    else prioritizedCinema.iterator.map(_._2.genres).find(_.nonEmpty).getOrElse(Seq.empty)
  }

  /** Production countries — union across sources, deduplicated
   *  case-insensitively while preserving the priority-first order. Sources
   *  spell country names in their own way, so the same country might appear
   *  twice ("USA" + "Stany Zjednoczone") if two of them disagree on spelling
   *  — that's accepted; the dedup only catches exact-match dupes modulo
   *  case. */
  def countries: Seq[String] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]
    prioritized.flatMap(_._2.countries)
      .filter(services.cinemas.CountryNames.isPolish)
      .foreach { c =>
        val key = c.toLowerCase(Locale.ROOT)
        if (!seen.exists(_.toLowerCase(Locale.ROOT) == key)) seen += c
      }
    seen.toSeq
  }

  /** Cinema → film deep-link, when that cinema reports one. */
  def filmUrlFor(cinema: Cinema): Option[String] =
    cinemaData.get(cinema).flatMap(_.filmUrl)

  /** Cinema → showtimes (empty when that cinema isn't screening). */
  def showtimesFor(cinema: Cinema): Seq[Showtime] =
    cinemaData.get(cinema).map(_.showtimes).getOrElse(Seq.empty)

  /** Cinema-reported original/international title — first non-empty with
   *  Multikino preferred. Separate from `originalTitle` (the TMDB-resolved
   *  production-language title): this is what the cinema's own API exposed,
   *  used as a fallback hint for the TMDB search. */
  def cinemaOriginalTitle: Option[String] =
    prioritizedCinema.iterator.flatMap(_._2.originalTitle).nextOption()

  /** Best available original (production-language) title across sources —
   *  the TMDB-resolved one first, then IMDb's `originalTitleText`, then
   *  whatever a cinema's own API exposed (only Multikino does). None when no
   *  source supplied one. Distinct from `originalTitle`, which is TMDB-only
   *  and stays that way for the search/URL fallbacks that rely on it. */
  def anyOriginalTitle: Option[String] =
    originalTitle
      .orElse(data.get(Imdb).flatMap(_.originalTitle))
      .orElse(cinemaOriginalTitle)

  /** The original title worth showing alongside `displayed` — present, and
   *  not merely a case/whitespace re-spelling of the title already on screen.
   *  An English-language film carries the same string as both its cinema
   *  title and its original title, so showing it would just repeat the line.
   *  Centralising the "is it redundant?" test here lets every frontend render
   *  the result unconditionally. */
  def distinctOriginalTitle(displayed: String): Option[String] =
    anyOriginalTitle.map(_.trim).filter(_.nonEmpty)
      .filterNot(_.equalsIgnoreCase(displayed.trim))

  /** Display-time URL: validated stored URL if we have one, else an on-the-fly
   *  search URL (for legacy records that pre-date URL persistence). */
  def metacriticHref(fallbackTitle: String): String = metacriticUrl.getOrElse {
    val encodedQuery = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.metacritic.com/search/$encodedQuery/?category=2"
  }

  def rottenTomatoesHref(fallbackTitle: String): String = rottenTomatoesUrl.getOrElse {
    val encodedQuery = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.rottentomatoes.com/search?search=$encodedQuery"
  }

  def filmwebHref(fallbackTitle: String): String = filmwebUrl.getOrElse {
    val encodedQuery = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.filmweb.pl/search?query=$encodedQuery"
  }
}
