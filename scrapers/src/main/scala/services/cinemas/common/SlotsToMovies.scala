package services.cinemas.common

import java.time.LocalDateTime

import models.*

/** One screening as a plain listing scraper reads it: a cleaned film title, its time, and
  * the few links/tokens the page carries. A client whose films carry nothing more builds these
  * and hands them to [[SlotsToMovies.basic]] instead of declaring its own slot type and
  * [[CinemaMovie]] assembly. A client whose page says more about the film (year, runtime,
  * cast, a detail record) keeps its own slot type and [[SlotsToMovies.fold]]. */
private[cinemas] final case class ScrapedSlot(
  title:      String,
  dateTime:   LocalDateTime,
  bookingUrl: Option[String] = None,
  filmUrl:    Option[String] = None,
  posterUrl:  Option[String] = None,
  room:       Option[String] = None,
  format:     List[String]   = Nil
)

/** One screening card of a page that lists a card per screening, each card linking its film's own page by slug and
  * none selling a ticket (the venue books by phone or a form): folded by [[SlotsToMovies.byFilmPage]]. */
private[cinemas] final case class FilmPageCard(
  title:     String,
  filmSlug:  String,
  dateTime:  LocalDateTime,
  posterUrl: Option[String] = None,
  year:      Option[Int]    = None
)

/** The film facts a listing row prints beside its screening (a `reż.` credit line): directors, production countries,
  * year, runtime -- each optional. */
private[cinemas] final case class ListingFacts(
  directors: Seq[String] = Seq.empty,
  countries: Seq[String] = Seq.empty,
  year:      Option[Int] = None,
  runtime:   Option[Int] = None
)

/** One screening row of a listing that links its film's page, may sell a ticket, and prints the film's
  * [[ListingFacts]]: folded by [[SlotsToMovies.byTitleAndFilmPage]]. */
private[cinemas] final case class CreditedScreening(
  title:      String,
  filmUrl:    Option[String],
  dateTime:   LocalDateTime,
  bookingUrl: Option[String],
  facts:      ListingFacts
)

/** The "fold raw screening slots into [[CinemaMovie]]s" shape every cinema
  * scraper repeats: group the per-screening slots by one film's key (its cleaned
  * title, or a feed's film id / slug), turn each group's slots into
  * deduplicated, time-ordered [[Showtime]]s, drop a film whose slots all
  * collapsed to nothing, build one [[CinemaMovie]] per surviving key, and order
  * the result (title-sorted unless the scraper asks otherwise).
  *
  * Only the grouping key, the slot → [[Showtime]] mapping, the dedup key, the
  * per-film [[CinemaMovie]] assembly and the output order differ between
  * scrapers; this captures the invariant skeleton (group → map → distinct →
  * sort → empty-drop → order) so a fix to the merge/dedup/ordering rule lands
  * in every scraper at once.
  *
  * @tparam S the scraper's own raw-slot type (one per screening).
  * @tparam K the grouping key: one film per distinct key. */
private[cinemas] object SlotsToMovies {

  /** The dedup key most scrapers use: a screening is the same showing iff it
    * has the same wall-clock time AND the same booking link. */
  def byTimeAndBooking(showtime: Showtime): Any = (showtime.dateTime, showtime.bookingUrl)

  /** The dedup key of a feed that sells one slot in several versions under one link
    * (an OmU and a dubbed print at the same time): time, booking link AND format. */
  def byTimeBookingAndFormat(showtime: Showtime): Any = (showtime.dateTime, showtime.bookingUrl, showtime.format)

  /** The dedup key of a venue that lists one slot per hall: time and hall. */
  def byTimeAndRoom(showtime: Showtime): Any = (showtime.dateTime, showtime.room)

  /** Dedup on the whole showtime: two are one only when every field agrees. */
  def byWholeShowtime(showtime: Showtime): Any = showtime

  /** No dedup: every slot's showtime is kept, a repeated one too — each gets a
    * key of its own that equals no other. */
  val keepEvery: Showtime => Any = _ => new Object

  /** How the folded films are ordered: each film with the key it was grouped by. */
  final class MovieOrder[K] private[SlotsToMovies] (private[SlotsToMovies] val arrange: Seq[(K, CinemaMovie)] => Seq[(K, CinemaMovie)])

  object MovieOrder {
    /** The default: by the film's title. */
    def byTitle[K]: MovieOrder[K] = new MovieOrder(_.sortBy(_._2.movie.title))

    /** The films in the order `groupBy` yields their keys — for a scraper whose
      * output was never sorted, so that moving it onto this fold moves nothing. */
    def asGrouped[K]: MovieOrder[K] = new MovieOrder(identity)

    /** By the grouping key (a feed's film id, a film page URL). */
    def byKey[K](using ordering: Ordering[K]): MovieOrder[K] = new MovieOrder(_.sortBy(_._1))
  }

  /**
   * Fold `slots` into [[CinemaMovie]]s, one per distinct `keyOf`.
   *
   * @param slots       the raw per-screening slots (titles already normalised:
   *                    grouping is on the verbatim `keyOf` value).
   * @param keyOf       the group key — each slot's (cleaned) film title, or the
   *                    feed's own film id when the feed has one.
   * @param showtimeOf  build the [[Showtime]] for one slot.
   * @param distinctBy  the dedup key for showtimes within a film (default
   *                    `(dateTime, bookingUrl)`); a film's showtimes are deduped
   *                    by it (first kept) then sorted by time.
   * @param order       the output order (default [[MovieOrder.byTitle]]).
   * @param buildMovie  assemble one [[CinemaMovie]] from a film's key, its
   *                    slots (in encounter order) and its prepared showtimes.
   *                    Called only for films with at least one showtime.
   */
  def fold[S, K](
    slots:      Seq[S],
    keyOf:      S => K,
    showtimeOf: S => Showtime,
    distinctBy: Showtime => Any = byTimeAndBooking,
    order:      MovieOrder[K] = MovieOrder.byTitle[K]
  )(buildMovie: (K, Seq[S], Seq[Showtime]) => CinemaMovie): Seq[CinemaMovie] =
    foldBlocks(slots, keyOf, (slot: S) => Seq(showtimeOf(slot)), distinctBy, order)((key, group, showtimes) =>
      Some(buildMovie(key, group, showtimes)))

  /** [[fold]] for a scraper whose unit carries any number of showtimes — a
    * film block with its slots, a detail page's schedule, one day's
    * [[CinemaMovie]] being merged across days: a film's showtimes are every
    * unit's, deduped by `distinctBy` and time-sorted as [[fold]]'s are — or that
    * can still refuse a film once its units are in hand (its detail record is
    * missing, it has no title): `buildMovie` returns `None` for a film to drop. */
  def foldBlocks[S, K](
    blocks:      Seq[S],
    keyOf:       S => K,
    showtimesOf: S => Seq[Showtime],
    distinctBy:  Showtime => Any = byTimeAndBooking,
    order:       MovieOrder[K] = MovieOrder.byTitle[K]
  )(buildMovie: (K, Seq[S], Seq[Showtime]) => Option[CinemaMovie]): Seq[CinemaMovie] =
    order.arrange(blocks.groupBy(keyOf).toSeq.flatMap { case (key, group) =>
      val showtimes = group
        .flatMap(showtimesOf)
        .distinctBy(distinctBy)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else buildMovie(key, group, showtimes).map(key -> _)
    }).map(_._2)

  /** Merge the same film's [[CinemaMovie]]s — one per day, chunk or listing section — into one per
    * `keyOf`: the first one's film metadata, every one's showtimes deduped by `distinctBy` and
    * time-sorted as [[fold]]'s are; a film left with no showtimes drops out. */
  def mergeMovies[K](
    movies:     Seq[CinemaMovie],
    keyOf:      CinemaMovie => K,
    distinctBy: Showtime => Any = byTimeAndBooking,
    order:      MovieOrder[K] = MovieOrder.byTitle[K]
  ): Seq[CinemaMovie] =
    foldBlocks(movies, keyOf, _.showtimes, distinctBy, order)((_, group, showtimes) => Some(group.head.copy(showtimes = showtimes)))

  /** The whole fold for a plain listing: one film per title, each [[ScrapedSlot]] a
    * [[Showtime]], the film's poster and page the first its slots carry, nothing else known
    * about it (TMDB enriches the rest downstream). */
  def basic(slots: Seq[ScrapedSlot], cinema: Cinema, distinctBy: Showtime => Any = byTimeAndBooking): Seq[CinemaMovie] =
    fold(slots, _.title, slot => Showtime(slot.dateTime, slot.bookingUrl, slot.room, slot.format), distinctBy) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.posterUrl).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        showtimes = showtimes
      )
    }

  /** The whole fold for a page of [[FilmPageCard]]s: one film per slug (its first card's title, year and poster, its
    * page `filmUrlOf(slug)`), one showtime per time, no booking link. */
  def byFilmPage(cards: Seq[FilmPageCard], cinema: Cinema, filmUrlOf: String => String): Seq[CinemaMovie] =
    fold(cards, _.filmSlug, card => Showtime(card.dateTime, bookingUrl = None), distinctBy = _.dateTime) { (slug, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(head.title, releaseYear = head.year),
        cinema    = cinema,
        posterUrl = head.posterUrl,
        filmUrl   = Some(filmUrlOf(slug)),
        showtimes = showtimes
      )
    }

  /** The whole fold for a listing of [[CreditedScreening]]s: one film per title and film page, one showtime per time
    * and booking link, each of the film's facts the first its rows state. */
  def byTitleAndFilmPage(screenings: Seq[CreditedScreening], cinema: Cinema): Seq[CinemaMovie] =
    fold(screenings, s => (s.title, s.filmUrl), s => Showtime(s.dateTime, s.bookingUrl)) { case ((title, filmUrl), group, showtimes) =>
      val facts = group.map(_.facts)
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = facts.flatMap(_.runtime).headOption, releaseYear = facts.flatMap(_.year).headOption,
                          countries = facts.map(_.countries).find(_.nonEmpty).getOrElse(Seq.empty)),
        cinema    = cinema,
        filmUrl   = filmUrl,
        director  = facts.map(_.directors).find(_.nonEmpty).getOrElse(Seq.empty),
        showtimes = showtimes
      )
    }
}
