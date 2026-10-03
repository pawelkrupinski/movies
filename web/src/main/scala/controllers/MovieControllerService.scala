package controllers

import models._
import play.api.Logging
import services.readmodel.WebReadModel

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime}

// What the listing IS, apart from how a request asks for it: the schedules the web serves,
// read from the read model. A file of its own because the convergence legs read their
// verdicts through it (e2e dependsOn web), so `.github/convergence-paths.txt` names it and
// what it reaches — not the controller, its views, or anything else of the web.

case class CinemaShowtimes(cinema: Cinema, showtimes: Seq[Showtime])

case class FilmSchedule(
                         movie: Movie,
                         posterUrl: Option[String],
                         synopsis: Option[String],
                         cast: Seq[String],
                         director: Seq[String],
                         cinemaFilmUrls: Seq[(Cinema, String)],
                         showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
                         // The fully-resolved metadata document this schedule was built from —
                         // ratings, poster fallbacks, original title, trailers. Replaces the
                         // old `Option[MovieRecord]`: the web no longer holds MovieRecords.
                         resolved: ResolvedMovie,
                         // This film's `/{city}/movie/{slug}` address, assigned over the whole
                         // corpus by `FilmSlugs` so two same-titled films get one each. `None`
                         // only for a title that folds to no usable slug — `FilmHref` answers
                         // those with the legacy query form. Carried on the schedule rather
                         // than re-derived per call site so the card link, the canonical
                         // og:url, the sitemap and the JSON-LD can't disagree.
                         slug: Option[String],
                         // The day, in the city's zone, this schedule was cut against: what
                         // "upcoming" meant, and the year its date labels are read from
                         // (`CardFormat.date`). From the service's clock, never the system's.
                         asOf: LocalDate
                       ) {
  /** [[cinemaFilmUrls]] a page may link — the http(s) ones ([[WebHref]]); every
   *  real listing's whole list. */
  lazy val linkableCinemaFilmUrls: Seq[(Cinema, WebHref)] =
    cinemaFilmUrls.flatMap { case (cinema, url) => WebHref.of(url).map(cinema -> _) }

  /** [[posterUrl]] when a page may load it ([[WebHref]]). */
  lazy val posterHref: Option[WebHref] = posterUrl.flatMap(WebHref.of)

  /** The resolved fallback posters a page may load ([[WebHref]]), in their order. */
  lazy val fallbackPosterHrefs: Seq[WebHref] = resolved.fallbackPosterUrls.flatMap(WebHref.of)

  // Hashed once per schedule, not per lookup: it keys the card cache (`FilmCardFragments`)
  // on every listing render, a deep hash of every showtime, and a reused schedule
  // (`MovieControllerService.Built`) is the same immutable object render after render.
  override lazy val hashCode: Int = scala.util.hashing.MurmurHash3.caseClassHash(this)
}

/**
 * Builds the per-city [[FilmSchedule]] view from the denormalised read model:
 * this city's [[CityScreening]] documents joined to their [[ResolvedMovie]]. The web
 * never touches the `movies` collection or a MovieRecord — the merge already
 * happened at projection time.
 */
class MovieControllerService(
  readModel: WebReadModel,
  // What "now" is when a caller does not say: which showtimes are still upcoming.
  clock: java.time.Clock
) extends Logging {

  /** The current instant, on [[clock]]. */
  def now(): java.time.Instant = clock.instant()

  /** The current wall-clock time in `city`'s own zone, on [[clock]]. */
  def nowIn(city: City): LocalDateTime = venueClock.nowIn(city)
  private val venueClock = new models.VenueClock(clock)

  def toSchedules(city: City): Seq[FilmSchedule] =
    toSchedules(city, nowIn(city))

  /** Every OTHER city in `country` with an upcoming showing of `filmId` right
   *  now — what a film page needs to link directly to its sibling-city
   *  near-duplicates (`/{city}/movie/{slug}`), which used to be reachable only
   *  through the sitemap and nothing else. Same `isUpcoming` predicate
   *  [[schedulesFor]] applies, checked per city instead of building each
   *  city's whole schedule. */
  def citiesShowing(filmId: String, excluding: City, country: Country, now: LocalDateTime): Seq[City] = {
    val film = Set(filmId)
    country.allSorted.filter { c =>
      c != excluding && readModel.screeningsOfFilms(c.slug, film).exists(_.showtimes.exists(_.isUpcoming(now)))
    }
  }

  /** Overload with an injectable `now` so tests can pin the clock to a fixture's
   * capture date. Scoped to `city`: `readModel.screeningsForCity` already
   * returns only this city's cinemas' screenings, so a film playing only
   * elsewhere drops out here.
   *
   * Ordering-tolerant join: a screening document whose `ResolvedMovie` hasn't landed
   * yet (the movie-before-screenings write order can still be observed in the
   * reverse order over two independent change streams) simply contributes
   * nothing until the movie document arrives — no half-rendered card. */
  def toSchedules(city: City, now: LocalDateTime): Seq[FilmSchedule] =
    schedulesFor(city, readModel.screeningsForCity(city.slug), now)

  /** The schedules of ONLY the films with these ids, in [[toSchedules]]' order — the
   *  same join over the same rows, minus every film the caller did not ask for. A
   *  request for one film used to build the whole city's list and pick its own out
   *  of it: 2,000 joins for one card, ~4 ms a request on a 2,000-card city. */
  private def schedulesFor(city: City, filmIds: Set[String]): Seq[FilmSchedule] =
    schedulesFor(city, readModel.screeningsOfFilms(city.slug, filmIds), nowIn(city))

  private def schedulesFor(city: City, cityScreenings: Seq[CityScreening], now: LocalDateTime): Seq[FilmSchedule] = {
    // `Showtime.isUpcoming(now)` is `dateTime.isAfter(now.minus(Grace))`: the cutoff once,
    // not a fresh LocalDateTime per showtime of the city (54k on New York).
    val cutoff = now.minus(Showtime.Grace)
    val asOf   = now.toLocalDate
    // Grouped in a pre-sized mutable map rather than `groupBy`, whose persistent HashMap
    // copied nodes on every insert: ~1.7 MB a New York render, for 2,600 rows.
    val byFilm = new java.util.HashMap[String, Vector[CityScreening]](cityScreenings.size * 2)
    cityScreenings.foreach(sc => byFilm.merge(sc.filmId, Vector(sc), _ ++ _))
    val films = scala.collection.mutable.ArrayBuffer.empty[(LocalDateTime, FilmSchedule)]
    byFilm.forEach { (filmId, screenings) =>
      readModel.movie(filmId).flatMap { resolved =>
        val key   = (city.slug, filmId)
        val prior = built.getIfPresent(key)
        if (prior != null && prior.stillHolds(screenings, resolved, asOf, cutoff, readModel.filmSlugs.slugFor(resolved._id)))
          Some((prior.earliest, prior.schedule))
        else {
          val byDate = showingsByDate(screenings, cutoff)
          if (byDate.isEmpty) None
          else {
            // A film with no future showing in this city drops out of its list view (its
            // documents stay in the store); one with any is ordered by its earliest.
            val earliest = byDate.head._2.iterator.map(_.showtimes.head.dateTime).min
            val cinemaFilmUrls: Seq[(Cinema, String)] =
              screenings
                .flatMap(sc => MovieControllerService.cinemaByName(sc.cinema).flatMap(c => sc.filmUrl.map(c -> _)))
                .sortBy(_._1.displayName)
            val schedule = filmSchedule(resolved, cinemaFilmUrls, byDate, city, asOf)
            built.put(key, Built(screenings, resolved, asOf, earliest, schedule))
            Some((earliest, schedule))
          }
        }
      }
      .foreach(films += _)
    }
    // Earliest showtime, then title, then film id — a total order, so two same-titled films
    // tied on their earliest showing never take the grouping map's iteration order, which moves
    // with the city's row count. Compared field by field, not as a tuple built per comparison.
    films.sortInPlaceWith { case ((e1, f1), (e2, f2)) =>
      val byTime = e1.compareTo(e2)
      if (byTime != 0) byTime < 0
      else {
        val byTitle = f1.movie.title.compareTo(f2.movie.title)
        if (byTitle != 0) byTitle < 0 else f1.resolved._id.compareTo(f2.resolved._id) < 0
      }
    }
    films.iterator.map(_._2).toList
  }

  /** A film's schedule as last built for a city, and what it was built from.
   *
   *  WHY. Every render of a listing rebuilt every film's schedule from the read model —
   *  8 MB a render on New York — while between two renders almost every film is
   *  unchanged. A schedule is a pure function of its film's rows, its resolved
   *  metadata, the city, the day, its corpus-wide slug and which showtimes are still
   *  upcoming; [[stillHolds]] checks every one of them, so a reused schedule is the one
   *  a rebuild would produce. The read model replaces a row or a movie document on any
   *  write rather than mutating it, so "the same object" is "unchanged". */
  private final case class Built(rows: Vector[CityScreening], resolved: ResolvedMovie, asOf: LocalDate,
                                 earliest: LocalDateTime, schedule: FilmSchedule) {
    def stillHolds(current: Vector[CityScreening], currentResolved: ResolvedMovie, today: LocalDate,
                   cutoff: LocalDateTime, slug: Option[String]): Boolean =
      (resolved eq currentResolved) && asOf == today &&
        // No showtime has lapsed since: the earliest one is still upcoming, so all are —
        // and the cutoff only moves forward, so none has become upcoming either.
        cutoff.isBefore(earliest) &&
        schedule.slug == slug &&
        rows.size == current.size && sameRows(current)

    // Every row still one of `current`'s, by identity — indexed loops rather than
    // `forall`/`exists`, whose closures were a fifth of what a sitemap allocated (it
    // checks every film of every city).
    private def sameRows(current: Vector[CityScreening]): Boolean = {
      var i = 0
      while (i < rows.length) {
        val row = rows(i)
        var j = 0
        while (j < current.length && !(current(j) eq row)) j += 1
        if (j == current.length) return false
        i += 1
      }
      true
    }
  }

  private val built: com.github.benmanes.caffeine.cache.Cache[(String, String), Built] =
    com.github.benmanes.caffeine.cache.Caffeine.newBuilder()
      .maximumSize(MovieControllerService.BuiltSchedules)
      .build[(String, String), Built]()

  /** One film's upcoming showtimes in this city, by date and then by cinema: dates
   *  ascending, a date's cinemas by their earliest showtime that day with
   *  `displayName` breaking ties (the "Kino Malta vs Kino Meduza" snapshot-flake fix),
   *  each cinema's showtimes by time.
   *
   *  Built by sorting each cinema's showtimes ONCE and cutting them into per-date
   *  runs, rather than regrouping (cinema, showtime) tuples by date and again by
   *  cinema: on a New-York-sized city that was 25 MB of tuples, maps and orderings per
   *  render, where this is an array per cinema and a `CinemaShowtimes` per run. */
  private def showingsByDate(screenings: Seq[CityScreening], cutoff: LocalDateTime): Seq[(LocalDate, Seq[CinemaShowtimes])] = {
    // A cinema can carry the film on more than one row; its showtimes merge, in row order.
    val perCinema = new java.util.LinkedHashMap[Cinema, scala.collection.mutable.ArrayBuffer[Showtime]]()
    for (sc <- screenings; cinema <- MovieControllerService.cinemaByName(sc.cinema); st <- sc.showtimes)
      if (st.dateTime.isAfter(cutoff)) perCinema.computeIfAbsent(cinema, _ => scala.collection.mutable.ArrayBuffer.empty).addOne(st)
    if (perCinema.isEmpty) return Nil
    val runs = scala.collection.mutable.ArrayBuffer.empty[(LocalDate, CinemaShowtimes)]
    perCinema.forEach { (cinema, showtimes) =>
      val sorted = showtimes.sortInPlaceBy(_.dateTime)      // stable, as `sortBy` was
      var from = 0
      while (from < sorted.length) {
        val date = sorted(from).dateTime.toLocalDate
        var to = from + 1
        while (to < sorted.length && sorted(to).dateTime.toLocalDate == date) to += 1
        runs += date -> CinemaShowtimes(cinema, sorted.slice(from, to).toList)
        from = to
      }
    }
    runs.sortInPlaceWith { case ((d1, a), (d2, b)) =>
      val byDate = d1.compareTo(d2)
      if (byDate != 0) byDate < 0
      else {
        val byFirst = a.showtimes.head.dateTime.compareTo(b.showtimes.head.dateTime)
        if (byFirst != 0) byFirst < 0 else a.cinema.displayName < b.cinema.displayName
      }
    }
    val days = List.newBuilder[(LocalDate, Seq[CinemaShowtimes])]
    var i = 0
    while (i < runs.length) {
      val date = runs(i)._1
      val day  = List.newBuilder[CinemaShowtimes]
      while (i < runs.length && runs(i)._1 == date) { day += runs(i)._2; i += 1 }
      days += date -> day.result()
    }
    days.result()
  }

  /** Assemble a [[FilmSchedule]] from a resolved movie + its (possibly empty)
   *  showings. Shared by the live `toSchedules` join and the deep-link
   *  resilience fallback below, so both materialise the schedule identically. */
  private def filmSchedule(resolved: ResolvedMovie,
                           cinemaFilmUrls: Seq[(Cinema, String)],
                           showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
                           city: City,
                           asOf: LocalDate): FilmSchedule =
    FilmSchedule(
      movie = Movie(resolved.title, resolved.runtimeMinutes, resolved.releaseYear, countries = resolved.countries, genres = resolved.genres),
      posterUrl = resolved.posterUrl,
      synopsis = resolved.synopsisFor(city),
      cast = resolved.cast,
      director = resolved.directors,
      cinemaFilmUrls = cinemaFilmUrls,
      showings = showings,
      resolved = resolved,
      slug = readModel.filmSlugs.slugFor(resolved._id),
      asOf = asOf
    )

  def film(city: City, title: String): Option[FilmSchedule] = {
    // Matched through the read model's title index rather than by folding every
    // schedule's title per request; among the films the index names, the first
    // in schedule order (earliest showtime) wins, as it always has — and only
    // those films are joined.
    def lookup(t: String): Option[FilmSchedule] = {
      val ids = readModel.filmTitles.idsFor(t).toSet
      if (ids.isEmpty) None else schedulesFor(city, ids).headOption
    }
    // Telegram (and some other chat apps) re-percent-encode a pasted URL whose
    // query already carries %XX escapes: our `%20` becomes `%2520`, `%C5%BC`
    // becomes `%25C5%25BC`. Play decodes that once, so `title` arrives with a
    // literal `%20` / `%C5%BC` still in it and the direct match misses. On a
    // miss, decode the residual escapes once more and retry.
    val decoded: Option[String] =
      Option(title)
        .filter(MovieControllerService.looksPercentEncoded)
        .map(t => URLDecoder.decode(t, StandardCharsets.UTF_8))
    lookup(title).orElse(decoded.flatMap(lookup))
      .orElse(knownMovieFallback(city, title, decoded))
  }

  /** Resolve the canonical `/{city}/movie/{slug}` address.
   *
   *  `FilmSlugs` assigned the address, so it is also what reverses it — one
   *  film per slug, whether or not another film shares its title. The re-slug
   *  scan behind it is the fallback for a slug the map doesn't know: a link
   *  minted before a re-key, or the sub-second window while the read model
   *  reloads. Re-slugging alone is what USED to resolve every address, and on a
   *  same-title pair it could only ever reach one of the two films — it stays
   *  as a safety net, not as the rule.
   *
   *  The fallback tie-breaks on the title rather than taking the head, because
   *  `toSchedules` orders by earliest showtime and that shifts through the day. */
  def filmBySlug(city: City, slug: String): Option[FilmSchedule] = {
    val addressed = readModel.filmSlugs.idFor(slug)
    def reslugged(title: String): Boolean = tools.Slugify(title) == slug

    // An address the index knows joins ONE film; only an unknown one walks the
    // city's schedules — and then the corpus — re-slugging titles, the safety
    // net for a stale link.
    addressed.fold(toSchedules(city).filter(s => reslugged(s.movie.title)).minByOption(_.movie.title))(id => schedulesFor(city, Set(id)).headOption)
      .orElse {
        readModelFallback(
          city,
          addressed.fold(readModel.allMovies().filter(m => reslugged(m.title)).minByOption(_.title))(readModel.movie),
          reference = s"slug='$slug'"
        )
      }
  }

  /** Resilience for film deep-links: a title the read model KNOWS but that has no
   *  live schedule in this city right now must not 404 a shared/bookmarked link.
   *  The common cause is a sub-second window while the worker re-projects or
   *  re-keys the film — its `web_movies` and `web_screenings` documents arrive
   *  over two independent change streams, so the `toSchedules` join momentarily
   *  drops it (see [[services.readmodel.ReadModelProjectionMetrics]] for the
   *  worker-side `films_pruned` / reprojection signal). Render the movie with an
   *  empty showings list instead; it self-heals on the next load once both
   *  documents land. A genuinely-ended run resolves the same way (better than a
   *  404 for an old link); a title the read model has never seen still returns
   *  None. Each hit is logged so the rate of "a link would have broken" is
   *  visible alongside the worker metrics. */
  private def knownMovieFallback(city: City, title: String, decoded: Option[String]): Option[FilmSchedule] = {
    // Newest first, per `FilmTitles`: a same-title pair with no live schedule
    // here resolves to the film that holds the bare slug.
    def byTitle(t: String): Option[ResolvedMovie] =
      readModel.filmTitles.idsFor(t).iterator.flatMap(readModel.movie).nextOption()
    readModelFallback(city, byTitle(title).orElse(decoded.flatMap(byTitle)), reference = s"title='$title'")
  }

  /** Shared tail of both deep-link resolvers (by title and by slug): render the
   *  read model's copy of the movie with no showings, and log that a link would
   *  otherwise have broken. `reference` names whichever key the caller looked up,
   *  so the log line stays actionable. */
  private def readModelFallback(city: City, resolved: Option[ResolvedMovie], reference: String): Option[FilmSchedule] =
    resolved.map { movie =>
      logger.warn(s"film deep-link served from the read model without a live ${city.slug} schedule " +
        s"(reprojection/rekey gap or ended run): $reference filmId=${movie._id}")
      filmSchedule(movie, cinemaFilmUrls = Seq.empty, showings = Seq.empty, city, nowIn(city).toLocalDate)
    }
}

object MovieControllerService {

  /** How many (city, film) schedules [[MovieControllerService]] keeps for reuse: every
   *  film of the busiest cities several times over. A New York schedule set is ~4 MB. */
  val BuiltSchedules: Long = 10_000
  /** Above this many showtimes on one page, the day carousel's clone-and-slide
   *  preview (`shared.js` `buildDayColumn`, cloning the whole `#film-grid` to
   *  build a sliding neighbour-day preview) costs enough DOM-cloning that a
   *  swipe visibly stutters on a phone — Salt Lake City (28k showtimes, 40
   *  cinemas) measured ~17.5k DOM nodes per clone, two clones per swipe.
   *  `renderIndexHtml` stamps `isLargeCity` onto `#view-root` as
   *  `data-large-city`, and `shared.js` skips straight to an in-place
   *  re-filter instead of cloning + animating when it's set. Desktop is
   *  unaffected — the carousel's slide/clone already only runs behind
   *  `pointer: coarse`. */
  val LargeCityShowtimeThreshold = 10000

  /** Total individual showtimes across every film/date/cinema in `schedules` —
   *  the size the day carousel actually has to clone per swipe. */
  def totalShowtimes(schedules: Seq[FilmSchedule]): Int = {
    // Counted in a plain Int rather than `.map(...).sum`, which boxed every count.
    var total = 0
    for (film <- schedules; (_, cinemas) <- film.showings; cinema <- cinemas) total += cinema.showtimes.size
    total
  }

  /** Does any showtime on any day of `schedules` screen in IMAX? The Filtry
   *  panel offers its "IMAX only" checkbox only then -- in a city without an
   *  IMAX screen the filter could only ever blank the listing. */
  def hasImaxShowtime(schedules: Seq[FilmSchedule]): Boolean =
    // Nested `exists` rather than an iterator chain, which allocated an iterator per
    // list it crossed: 1.6 MB a New York render, for one boolean.
    schedules.exists(_.showings.exists(_._2.exists(_.showtimes.exists(_.format.contains("IMAX")))))

  /** displayName → Cinema (cinemas are `Source`s, so reuse the shared map). */
  private def cinemaByName(name: String): Option[Cinema] =
    Source.byDisplayName.get(name).collect { case c: Cinema => c }

  private val PercentEscape = "%[0-9A-Fa-f]{2}".r

  /** Does the string still contain an unresolved `%XX` escape? Used to spot a
   *  doubly-encoded title (see [[MovieControllerService.film]]) without
   *  touching the normal, already-decoded path. */
  private def looksPercentEncoded(s: String): Boolean =
    PercentEscape.findFirstIn(s).isDefined
}
