package services.cinemas.common

import services.movies.ScreeningTokens
import models.{Cinema, MovieRecord, Source, SourceData}
import services.cinemas.pl.FilmwebShowtimesClient

/**
 * The per-film detail fields a cinema fetches *separately* from its listing —
 * synopsis, cast, director, runtime, original title, genres, countries, poster,
 * trailer. These are what gets deferred out of `fetch()` and filled in later by
 * an `EnrichDetails` task. Detail is purely enrichment: a deferred client always
 * carries its showtimes on the bare movie (showtimes are identity-bearing, so a
 * client whose showtimes live behind a separate fetch keeps that fetch inline —
 * Rialto's event pages, Helios's screens — rather than deferring them here).
 */
case class FilmDetail(
  synopsis:       Option[String] = None,
  cast:           Seq[String]    = Seq.empty,
  director:       Seq[String]    = Seq.empty,
  runtimeMinutes: Option[Int]    = None,
  releaseYear:    Option[Int]    = None,
  originalTitle:  Option[String] = None,
  countries:      Seq[String]    = Seq.empty,
  genres:         Seq[String]    = Seq.empty,
  posterUrl:      Option[String] = None,
  trailerUrl:     Option[String] = None,
  ageRating:      Option[String] = None,
  // A per-FILM language/format (NAP/DUB/LEK) some cinemas expose only on the
  // detail page (a "Wersja językowa: polski lektor" row), never in the listing
  // title. Applied to the film's showings so the language badge is preserved
  // even where the listing carries no per-screening suffix. Empty for cinemas
  // whose language is already per-screening on the listing.
  format:         List[String]   = Nil
) {
  /** Fill gaps in an existing cinema `SourceData` slot from these detail fields,
   *  preserving the slot's showtimes/title/filmUrl. Treats the listing/bare
   *  values as authoritative — a present listing value is never replaced by a
   *  detail one; for poster it preserves the cinema's own listing poster (which
   *  several clients prefer over the detail-page poster). */
  def mergeInto(slot: SourceData): SourceData = slot.copy(
    synopsis       = slot.synopsis.orElse(synopsis),
    cast           = if (slot.cast.nonEmpty) slot.cast else cast,
    director       = if (slot.director.nonEmpty) slot.director else director,
    runtimeMinutes = slot.runtimeMinutes.orElse(runtimeMinutes),
    releaseYear    = slot.releaseYear.orElse(releaseYear),
    originalTitle  = slot.originalTitle.orElse(originalTitle),
    countries      = if (slot.countries.nonEmpty) slot.countries else countries,
    genres         = if (slot.genres.nonEmpty) slot.genres else genres,
    posterUrl      = slot.posterUrl.orElse(posterUrl),
    trailerUrl     = slot.trailerUrl.orElse(trailerUrl),
    ageRating      = slot.ageRating.orElse(ageRating),
    // Badge the film's showings with the detail-page language, but never
    // overwrite a per-screening format the listing already set. Through
    // `ScreeningTokens` like every other badge — this is the second way a
    // source's own words reach a showtime, and a detail page words them as
    // freely as a listing does.
    showtimes      = if (format.isEmpty) slot.showtimes
                     else slot.showtimes.map(st =>
                       if (st.format.isEmpty) st.copy(format = ScreeningTokens.normalize(format)) else st)
  )
}

/**
 * A cinema (or chain) whose per-film detail can be fetched independently of its
 * listing scrape, so the detail is deferred to a queue task and deduped per
 * `(detailGroup, film)`. A `CinemaScraper` opts in by also extending this; its
 * `fetch()` then returns bare movies (showtimes + a `filmUrl` reference) and the
 * `EnrichDetails` task fills in the rest by calling `fetchFilmDetail`.
 *
 * `detailGroup` is the dedup/freshness scope: a standalone cinema uses its own
 * id, a chain uses the chain name so all its locations share one detail fetch
 * per film. `detailTarget` is the `SourceData` slot the fetched detail is
 * written to — by default the cinema's own slot, but a chain redirects it to a
 * single network-level source so the detail is stored once and shared across
 * every venue via `MovieRecord`'s film-level merged accessors.
 */
trait DetailEnricher {
  def cinema: Cinema
  def detailGroup: String
  /** The `SourceData` slot the fetched detail is merged into. Defaults to this
   *  cinema's own slot (1:1 case); a chain overrides it to a shared network
   *  source so all its venues read one detail fetch. */
  def detailTarget: Source = cinema
  /** Override the /uptime enrichment-health service name. None → the per-cinema
   *  `"<cinema>|enrichment"` sub-row (the default). A chain sets a single
   *  network-level name so it reports one global entry instead of one per
   *  venue. */
  def enrichmentServiceOverride: Option[String] = None
  /** Whether resolution should WAIT for this cinema's deferred detail. True (the
   *  default) when the detail supplies TMDB-identity hints (director / original
   *  title / production year): a scraped row with a `filmUrl` is then held
   *  `detailPending` — out of the read model and the TMDB stage — until
   *  `fetchFilmDetail` lands, at which point `EnrichDetailsHandler` triggers
   *  resolution. Set false for a cinema whose listing already carries those
   *  hints and whose detail is purely display enrichment (synopsis / poster /
   *  trailer): the row resolves immediately from the listing and the detail
   *  merges in asynchronously when its `EnrichDetails` task runs. */
  def defersTmdbResolution: Boolean = true
  /** Fetch + parse one film's detail by the reference the listing scrape left on
   *  the movie (its `filmUrl`). None on failure/absence, so the task stays
   *  stale and is retried rather than recording an empty result as fresh.
   *
   *  A client MAY let a durable `HttpStatusException` (404/410) escape instead of
   *  folding it into None — that is how it opts into [[DetailFetchOutcome.Gone]]
   *  and stops the handler retrying a dead page every tick. Swallowing it stays
   *  correct, just noisier. */
  def fetchFilmDetail(ref: String): Option[FilmDetail]

  /** [[fetchFilmDetail]] as the enrichment handler needs to hear it: a failure
   *  that describes the URL (404/410) told apart from one that describes the
   *  moment. See [[DetailFetchOutcome]] for why the difference matters — the
   *  handler stamps the first and retries the second every tick.
   *
   *  `final`, because the classification is one rule and must not fork per
   *  client: a client opts into `Gone` purely by letting the durable
   *  `HttpStatusException` escape its fetch (see
   *  [[DetailFetchOutcome.transientToNone]]), and one that still swallows it
   *  keeps the every-tick retry unchanged. */
  final def fetchDetail(ref: String): DetailFetchOutcome =
    DetailFetchOutcome.of(scala.util.Try(fetchFilmDetail(ref)))

  /** The per-film detail URL this enricher can actually fetch for `record` — the
   *  cinema's own event-page reference from its source slot — or None when there
   *  is nothing native to fetch: no slot/`filmUrl`, OR a Filmweb-FALLBACK row
   *  whose `filmUrl` points at filmweb.pl (the cinema is passing Filmweb data
   *  through, which only TMDB enrichment, not this cinema's parser, can read).
   *  Pointing `fetchFilmDetail` at a fallback URL fails on every pass, so every
   *  detail-driving site (scrape classify, the reaper, staging) gates on this
   *  rather than the raw `filmUrl`. */
  final def nativeDetailRef(record: MovieRecord): Option[String] =
    nativeDetailRefIn(record.cinemaData)

  /** [[nativeDetailRef]] against an ALREADY-COMPUTED `cinemaData`, for callers that
   *  ask several enrichers about the SAME row.
   *
   *  `cinemaData` (not `data.get(cinema)`): per-(cinema,title) slots are keyed by
   *  `CinemaShowing`, so a bare-cinema lookup misses them — `cinemaData` collapses a
   *  venue's slots to one representative, whose `filmUrl` is what detail fetches. But
   *  it is a `def` that SORTS the row's slots and rebuilds a Map on every call, and
   *  enrichers are one instance PER VENUE (Cineworld alone is 87), so asking each of
   *  them via `nativeDetailRef` recomputes the same map once per venue per row.
   *
   *  Measured against the live UK corpus 2026-07-29: 1,599 records x 185 enricher
   *  instances cost 54.6ms a pass recomputing, versus 3.6ms hoisting it — 15x. That
   *  loop runs every 60s in [[services.tasks.DetailReaper]] and was 7.93cc of the UK
   *  worker's 12.46cc, 64% of its CPU, to produce 0.3 EnrichDetails/min. */
  final def nativeDetailRefIn(cinemaData: Map[Cinema, SourceData]): Option[String] =
    cinemaData.get(cinema).flatMap(_.filmUrl).filterNot(FilmwebShowtimesClient.isFilmwebFilmUrl)
}
