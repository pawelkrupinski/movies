package services.identity

import models.{CinemaMovie, Movie, Showtime}

import java.time.LocalDateTime

/** One listing read's repeated values, held once: a cut-over projection holds every venue's listing at
 *  once while it resolves and writes, and a film at a hundred venues of one feed arrived as a hundred
 *  equal copies of its title, cast, synopsis and poster, every showtime with its own instant. Each value
 *  is replaced by the first equal one this pool saw, so the listings are equal to what was read, only
 *  sharing. Scoped to ONE read and dropped with it — plain maps, no eviction, nothing outlives the
 *  listings that hold the values; not thread-safe, as a read is one thread's. */
private[identity] final class ListingValuePool {
  private def table[A <: AnyRef] = new java.util.HashMap[A, A]()
  // One table per type, so a value is only ever replaced by one of its own class (an equal `Seq` of
  // another class, a `Vector` for a `List`, would be no substitute for a `List` field).
  private val movies   = table[Movie]
  private val texts    = table[Option[String]]
  private val lists    = table[Seq[String]]
  private val formats  = table[List[String]]
  private val ids      = table[Map[String, String]]
  private val instants = table[LocalDateTime]

  private def held[A <: AnyRef](in: java.util.HashMap[A, A], value: A): A = {
    val prior = in.putIfAbsent(value, value)
    if (prior == null) value else prior
  }
  private def text(value: Option[String]): Option[String] = if (value.isEmpty) None else held(texts, value)
  private def list(value: Seq[String]): Seq[String] = if (value.isEmpty) value else held(lists, value)

  def film(cm: CinemaMovie): CinemaMovie = cm.copy(
    movie       = held(movies, cm.movie),
    posterUrl   = text(cm.posterUrl),
    filmUrl     = text(cm.filmUrl),
    synopsis    = text(cm.synopsis),
    cast        = list(cm.cast),
    director    = list(cm.director),
    showtimes   = cm.showtimes.map(showtime),
    externalIds = if (cm.externalIds.isEmpty) cm.externalIds else held(ids, cm.externalIds),
    trailerUrl  = text(cm.trailerUrl),
    ageRating   = text(cm.ageRating))

  private def showtime(s: Showtime): Showtime = {
    val at     = held(instants, s.dateTime)
    val room   = text(s.room)
    val format = if (s.format.isEmpty) s.format else held(formats, s.format)
    if ((at eq s.dateTime) && (room eq s.room) && (format eq s.format)) s else s.copy(dateTime = at, room = room, format = format)
  }
}
