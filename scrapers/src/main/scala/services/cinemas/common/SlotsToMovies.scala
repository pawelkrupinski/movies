package services.cinemas.common

import models._

import java.time.LocalDateTime

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
    foldSome(slots, keyOf, showtimeOf, distinctBy, order)((key, group, showtimes) => Some(buildMovie(key, group, showtimes)))

  /** [[fold]] for a scraper that can still refuse a film once its slots are in
    * hand (its detail record is missing, it has no title): `buildMovie` returns
    * `None` for a film to drop. */
  def foldSome[S, K](
    slots:      Seq[S],
    keyOf:      S => K,
    showtimeOf: S => Showtime,
    distinctBy: Showtime => Any = byTimeAndBooking,
    order:      MovieOrder[K] = MovieOrder.byTitle[K]
  )(buildMovie: (K, Seq[S], Seq[Showtime]) => Option[CinemaMovie]): Seq[CinemaMovie] =
    foldBlocks(slots, keyOf, (slot: S) => Seq(showtimeOf(slot)), distinctBy, order)(buildMovie)

  /** [[foldSome]] for a scraper whose unit carries any number of showtimes — a
    * film block with its slots, a detail page's schedule, one day's
    * [[CinemaMovie]] being merged across days: a film's showtimes are every
    * unit's, deduped by `distinctBy` and time-sorted as [[fold]]'s are. */
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
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
}
