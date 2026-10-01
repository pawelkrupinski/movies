package services.movies

import models.{CinemaShowing, SourceData}

/**
 * Some of a film's venues as they stand now: for each venue, EVERY slot the film has there, each
 * with its showtimes stitched exactly as a whole-film read stitches them (a slot's showtimes are its
 * `screenings` row's; a slot with no row has none). What a change confined to a few venues' showtimes
 * hands the change stream's listeners instead of the whole film — see `MovieChangeStream`'s
 * venue re-read. A listener that cannot take it whole answers false and is sent the whole film.
 */
final case class VenueSlots(filmId: FilmId, atCinemas: Map[models.Cinema, Seq[(CinemaShowing, SourceData)]])

/** What a listener did with a [[VenueSlots]]: applied it, or declined it — for `reason` — and is
 *  owed the whole film. */
enum VenueVerdict {
  case Applied
  case Declined(reason: String)
}
