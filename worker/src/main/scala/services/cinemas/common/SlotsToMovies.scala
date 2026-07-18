package services.cinemas.common

import models._

/** The "fold raw screening slots into [[CinemaMovie]]s" shape every hand-rolled
  * cinema scraper repeats: group the per-screening slots by their (already
  * cleaned) title, turn each group's slots into deduplicated, time-ordered
  * [[Showtime]]s, drop a film whose slots all collapsed to nothing, build one
  * [[CinemaMovie]] per surviving title, and return them title-sorted.
  *
  * Only the slot → [[Showtime]] mapping, the dedup key and the per-film
  * [[CinemaMovie]] assembly differ between scrapers; this captures the invariant
  * skeleton (group → map → distinct → sort → empty-drop → outer sort) so a fix
  * to the merge/dedup/ordering rule lands in every scraper at once.
  *
  * @tparam S the scraper's own raw-slot type (one per screening). */
private[cinemas] object SlotsToMovies {

  /** The dedup key most scrapers use: a screening is the same showing iff it
    * has the same wall-clock time AND the same booking link. */
  def byTimeAndBooking(showtime: Showtime): Any = (showtime.dateTime, showtime.bookingUrl)

  /**
   * Fold `slots` into title-sorted [[CinemaMovie]]s.
   *
   * @param slots       the raw per-screening slots (titles already normalised:
   *                    grouping is on the verbatim `titleOf` value).
   * @param titleOf     the group key — each slot's (cleaned) film title.
   * @param showtimeOf  build the [[Showtime]] for one slot.
   * @param distinctBy  the dedup key for showtimes within a film (default
   *                    `(dateTime, bookingUrl)`); a film's showtimes are deduped
   *                    by it then sorted by time.
   * @param buildMovie  assemble one [[CinemaMovie]] from a film's title, its
   *                    slots (in encounter order) and its prepared showtimes.
   *                    Called only for films with at least one showtime.
   */
  def fold[S](
    slots:      Seq[S],
    titleOf:    S => String,
    showtimeOf: S => Showtime,
    distinctBy: Showtime => Any = byTimeAndBooking
  )(buildMovie: (String, Seq[S], Seq[Showtime]) => CinemaMovie): Seq[CinemaMovie] =
    slots.groupBy(titleOf).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(showtimeOf)
        .distinctBy(distinctBy)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(buildMovie(title, group, showtimes))
    }.sortBy(_.movie.title)
}
