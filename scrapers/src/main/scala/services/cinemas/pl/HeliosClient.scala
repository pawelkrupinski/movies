package services.cinemas.pl

import java.time.LocalDate
import java.util.Locale

import models.*
import services.cinemas.common.{CinemaScraper, ListingFeed}
import services.cinemas.pl.HeliosNuxt.{BookingBase, cleanTitle}
import services.cinemas.pl.HeliosRest.{ApiScreening, RestData}
import services.titles.TitleNormalizer
import tools.Whitespace.collapsedSpaces
import tools.{HeliosFetch, HttpFetch, HttpRead}

// `today` is injected so tests can pin it: the REST `/screening` + `/event`
// URLs bake the date window, and the recorded fixtures were captured for a
// specific day. The composition root passes the real Warsaw date; fixture
// wirings pass the fixture's capture date so the recorded URLs still match.
class HeliosClient(
  http:  HttpFetch    = HeliosFetch,
  config:   HeliosCinema = HeliosVenues.Poznan,
  today: => LocalDate,
  // Per-film detail (`/api/movie/{id}`) + screen-name fetch path. Defaults to
  // `http`; the composition root injects ONE CachingDetailFetch shared across
  // every Helios location, so a film's detail body is fetched once per chain per
  // TTL instead of once per location per pass. Live screening/page fetches that
  // carry volatile showtimes stay on `http`.
  detailHttp: Option[HttpFetch] = None,
  titles: TitleNormalizer
) extends CinemaScraper {

  override val cinema: Cinema = config.cinema
  override def listingFeed: ListingFeed = ListingFeed.Chain
  private val sourceId = config.sourceId
  private val PageUrl  = config.pageUrl
  private val restFeed = new HeliosRestFeed(http, detailHttp.getOrElse(http), sourceId)

  // ── Fetch ─────────────────────────────────────────────────────────────────

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PageUrl, HeliosRestFeed.ApiBase, BookingBase)

  // `config.baseUrl` (`helios.pl/<citySlug>/<cinemaSlug>`) is the venue's public
  // repertoire page — the same URL the NUXT scrape fetches `/repertuar` from.
  override def sourceUrl: Option[String] = Some(config.baseUrl)
  override def chainVenueId: Option[String] = Some(sourceId)

  def fetch(): Seq[CinemaMovie] = {
    // `today` is by-name: read ONCE, so the REST windows can't straddle midnight.
    val rest = restFeed.read(today)
    val nuxt = HeliosNuxt.buildMovies(HttpRead.page(http, PageUrl), config, titles)
    // The NUXT `/repertuar` page is Helios's authoritative full-repertoire source
    // (it lists every screening, well beyond REST's 6-day window). helios.pl
    // intermittently 302-redirects it to the films-less homepage — server-side,
    // per-venue, rotating — and `RealHttpFetch` follows the redirect, so
    // `buildMovies` parses nothing. If we then fell through to the REST
    // `/screening` subset, `fetch` would return a NON-EMPTY but truncated list,
    // which slips past `MovieCache.recordCinemaScrape`'s empty-scrape guard (it
    // only catches a fully-empty result) and PRUNES every NUXT-only film from the
    // venue — re-added on the next healthy tick. That round-trip is the per-city
    // served-films "flap" (Grafana panel-34). A film-less NUXT page means the
    // scrape is degraded, so return nothing and let the empty-scrape guard retain
    // the prior repertoire until NUXT recovers (a retry can't help — the redirect
    // persists for the whole degraded window).
    if (nuxt.isEmpty) return Seq.empty
    val enriched = enrichFromRest(nuxt, rest)
    mergeDuplicateFilms(removeLessSpecificOverlaps(enriched ++ restOnlyMovies(enriched, rest))).sortBy(_.movie.title)
  }

  // ── REST enrichment of NUXT movies ────────────────────────────────────────
  //
  // NUXT is the primary source — it lists every screening on the repertoire page,
  // including events and screenings outside the 6-day REST window. REST adds
  // room name, format (2D/NAP/ATMOS…), and richer movie metadata for the subset
  // of screenings it covers (regular screenings via `/screening`, event screenings
  // via `/event`). We match REST screenings to NUXT showtimes by the shared
  // screening UUID (NUXT sourceId == REST screening id), then look up the REST
  // movie body via screeningsById to enrich each NUXT movie.

  private def enrichFromRest(movies: Seq[CinemaMovie], rest: RestData): Seq[CinemaMovie] =
    movies.map { cm =>
      // Match REST screening → REST movie body via the shared screening UUID. Only
      // accept a film's enrichment when the REST title matches the NUXT title (after
      // cleaning) — Cyrillic vs Latin variants of the same Ukrainian film share a
      // booking UUID but are listed separately in the output. An event's is the film
      // the event itself names, whatever either is titled ("… w Helios Anime" screens
      // a body titled "All You Need Is Killl").
      val restInfo = cm.showtimes
        .flatMap(_.bookingUrl).flatMap(extractScreeningId)
        .flatMap(rest.screeningsById.get).headOption
        .flatMap(s => rest.movieDetails.get(s.movieId).filter(info => s.event || info.title.map(cleanTitle(_, titles)).contains(cm.movie.title)))

      cm.copy(
        movie     = cm.movie.copy(
          runtimeMinutes = cm.movie.runtimeMinutes.orElse(restInfo.flatMap(_.duration)),
          releaseYear    = restInfo.flatMap(_.year),
          originalTitle  = restInfo.flatMap(_.originalTitle).orElse(cm.movie.originalTitle),
          countries      = restInfo.map(_.countries).getOrElse(cm.movie.countries),
          genres         = restInfo.map(_.genres).getOrElse(cm.movie.genres)
        ),
        posterUrl  = restInfo.flatMap(_.posterUrl).orElse(cm.posterUrl),
        synopsis   = restInfo.flatMap(_.description),
        cast       = restInfo.map(_.cast).getOrElse(Seq.empty),
        director   = restInfo.map(_.director).getOrElse(Seq.empty),
        trailerUrl = restInfo.flatMap(_.trailerUrl).orElse(cm.trailerUrl),
        ageRating  = restInfo.flatMap(_.ageRating).orElse(cm.ageRating),
        showtimes  = cm.showtimes.map(enrichShowtime(_, rest))
      )
    }

  // REST sometimes lists movies NUXT does not — e.g. Ukrainian-titled UA screenings
  // that share their underlying screening UUID with the Cyrillic NUXT entry but
  // appear in the REST API under a Latin-transliterated title. Include any REST
  // movie whose (cleaned) title is not represented in the NUXT-derived list.
  private def restOnlyMovies(nuxtCovered: Seq[CinemaMovie], rest: RestData): Seq[CinemaMovie] = {
    val nuxtTitles = nuxtCovered.map(_.movie.title).toSet
    // an event's film is listed as the event, from the repertoire page — never as a film of its own
    rest.screeningsById.values.toSeq.filterNot(_.event)
      .groupBy(_.movieId).toSeq
      .flatMap { case (movieId, screenings) =>
        rest.movieDetails.get(movieId)
          .filterNot(_.title.map(cleanTitle(_, titles)).exists(nuxtTitles))
          .map(info => CinemaMovie(
            movie = Movie(
              title          = cleanTitle(info.title.getOrElse(movieId), titles),
              runtimeMinutes = info.duration,
              releaseYear    = info.year,
              originalTitle  = info.originalTitle,
              countries      = info.countries,
              genres         = info.genres,
              rawTitle       = Some(info.title.getOrElse(movieId))
            ),
            cinema    = config.cinema,
            posterUrl = info.posterUrl,
            filmUrl   = info.slug.map(s => s"${config.baseUrl}/filmy/$s"),
            synopsis   = info.description,
            cast       = info.cast,
            director   = info.director,
            trailerUrl = info.trailerUrl,
            ageRating  = info.ageRating,
            showtimes  = screenings.flatMap(restShowtime(_, rest)).distinct.sortBy(_.dateTime)
          ))
      }
      .filter(_.showtimes.nonEmpty)
  }

  private def restShowtime(s: ApiScreening, rest: RestData): Option[Showtime] =
    s.dateTime.map(dt => Showtime(
      dateTime   = dt,
      bookingUrl = Some(s"$BookingBase/${s.id}?cinemaId=$sourceId"),
      room       = rest.screenNames.get(s.screenId),
      format     = s.format
    ))

  private def enrichShowtime(st: Showtime, rest: RestData): Showtime =
    st.bookingUrl.flatMap(extractScreeningId).flatMap(rest.screeningsById.get) match {
      case Some(rs) => st.copy(room = rest.screenNames.get(rs.screenId), format = rs.format)
      case None     => st
    }

  private def extractScreeningId(bookingUrl: String): Option[String] =
    if (bookingUrl.startsWith(s"$BookingBase/"))
      Some(bookingUrl.stripPrefix(s"$BookingBase/").takeWhile(_ != '?'))
    else None

  // The NUXT page sometimes lists the same physical screening under both a generic
  // film title and a more-specific event/promo title (e.g. "Tom i Jerry" and
  // "Tom i Jerry - seanse z konkursami HDD"). Drop the less-specific row when its
  // showtimes overlap in time with a more-specific row.
  /** Collapse entries that are the same film listed twice in one scrape.
   *
   *  Helios surfaces a film under more than one raw title that the cache's row
   *  identity (`TitleNormalizer.sanitize`) treats as one:
   *    - a `/wydarzenie` event page and a `/filmy` film page differing only in
   *      case ("Drzewo Magii" vs "Drzewo magii"), and
   *    - a REST-only entry whose title carries a stray trailing space
   *      ("Straszny film " vs "Straszny film") — the same untrimmed title also
   *      defeats `enrichFromRest`'s exact-title match, so the film page never
   *      gets its year and the REST row leaks through as a separate entry.
   *
   *  Both spellings hash to the same cache key, so the cache writes a single
   *  slot but every raw entry overwrites it, and `recordCinemaScrape` flags each
   *  one `isNew` on every scrape — the recurring "(N new)" on Helios. Group by
   *  the cache's normalised title and merge each group into one entry: keep the
   *  canonical `/filmy` film as the base, fill any field it lacks from the
   *  siblings, and union their showtimes. */
  private[cinemas] def mergeDuplicateFilms(movies: Seq[CinemaMovie]): Seq[CinemaMovie] =
    movies.groupBy(m => titles.sanitize(m.movie.title)).values.toSeq.map(_.reduce { (a, b) =>
      if (HeliosClient.mergeRank(a) >= HeliosClient.mergeRank(b)) HeliosClient.mergeInto(a, b) else HeliosClient.mergeInto(b, a)
    })

  private def removeLessSpecificOverlaps(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val timesByTitle = movies.map(m => m.movie.title -> m.showtimes.map(_.dateTime).toSet).toMap
    val norm         = (s: String) => s.toLowerCase(Locale.ROOT).collapsedSpaces
    movies.filterNot { m =>
      val times = timesByTitle(m.movie.title)
      movies.exists(other =>
        other.movie.title != m.movie.title &&
          norm(other.movie.title).startsWith(norm(m.movie.title) + " ") &&
          timesByTitle(other.movie.title).intersect(times).nonEmpty)
    }
  }
}

object HeliosClient {
  /** Which of two listings of one film is the base of their merge: the canonical `/filmy` film page first, then any
   *  page, then a known year. */
  private def mergeRank(m: CinemaMovie): Int =
    Seq(m.filmUrl.exists(_.contains("/filmy/")) -> 4, m.filmUrl.isDefined -> 2, m.movie.releaseYear.isDefined -> 1)
      .collect { case (true, weight) => weight }.sum

  /** `primary` with every field it lacks taken from `other`, and both's showtimes. */
  private def mergeInto(primary: CinemaMovie, other: CinemaMovie): CinemaMovie =
    primary.copy(
      movie = primary.movie.copy(
        title          = primary.movie.title.trim,
        releaseYear    = primary.movie.releaseYear.orElse(other.movie.releaseYear),
        runtimeMinutes = primary.movie.runtimeMinutes.orElse(other.movie.runtimeMinutes),
        countries      = orOther(primary.movie.countries, other.movie.countries),
        genres         = orOther(primary.movie.genres, other.movie.genres)
      ),
      posterUrl  = primary.posterUrl.orElse(other.posterUrl),
      filmUrl    = primary.filmUrl.orElse(other.filmUrl),
      synopsis   = primary.synopsis.orElse(other.synopsis),
      cast       = orOther(primary.cast, other.cast),
      director   = orOther(primary.director, other.director),
      trailerUrl = primary.trailerUrl.orElse(other.trailerUrl),
      ageRating  = primary.ageRating.orElse(other.ageRating),
      showtimes  = (primary.showtimes ++ other.showtimes).distinct.sortBy(_.dateTime)
    )

  private def orOther(values: Seq[String], other: Seq[String]): Seq[String] = if (values.nonEmpty) values else other
}
