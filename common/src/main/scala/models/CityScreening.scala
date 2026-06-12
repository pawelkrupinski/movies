package models

/**
 * Read-only projection of one cinema's screenings of one film in one city —
 * one document per (film, city, cinema) in the `web_screenings` collection,
 * indexed by `city`.
 *
 * Holds ALL of that cinema's showtimes for the film, not just the future ones:
 * the web filters "future" at read time against the request clock, so the
 * document doesn't churn as the clock advances — it changes only when that
 * cinema's showtime set or deep-link actually changes. That's what keeps the
 * web's change-stream delta minimal: a single cinema's repertoire edit moves
 * exactly this one document.
 *
 * `_id` is `${filmId}|${city}|${cinema}`; `filmId` joins to
 * [[ResolvedMovie]]`._id`.
 */
case class CityScreening(
  _id:       String,
  filmId:    String,
  // City.slug — the indexed query key ("show every screening in this city").
  city:      String,
  // Cinema.displayName — stable identity of the venue.
  cinema:    String,
  // The cinema's deep-link to this film, when it reports one.
  filmUrl:   Option[String],
  showtimes: Seq[Showtime]
) {
  /** Readable alias for the Mongo `_id`. */
  def id: String = _id
}
