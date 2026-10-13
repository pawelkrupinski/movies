package services.movies

import controllers.FilmSchedule
import models.Showtime

/** A film's id is minted from whichever key the row was FIRST created under, so it
  * depends on arrival order by design (`FilmId`) — opaque, and compared by nothing.
  * Two replays of one corpus are therefore compared on the film's stored KEY, which
  * every arrival order agrees on: the records carry it as their id, the screenings
  * are re-keyed by it, and a rendered row's card id (`<id>` or `<id>~<variant>`) is
  * rewritten onto it. Everything else about a film must not move between replays.
  *
  * The records come back in the order of that key too. Ordered by title and year alone, two
  * films sharing both — a relay billed under one banner at venues the model keeps apart, two
  * same-titled secret screenings — tied, kept the store's order, which is the id's, which is
  * arrival order: every pass then held the same films in a different sequence, and the
  * whole-corpus comparison failed on a difference no film had (run 38251516862). */
object OrderIndependentIds {
  final class Keyed(records: Seq[StoredMovieRecord], normalizer: TitleNormalizer) {
    private val keyById: Map[String, String] = records.map(r => r.id.value -> r.key(normalizer)).toMap

    val stableRecords: Seq[StoredMovieRecord] = records.map(r => r.copy(id = FilmId(r.key(normalizer))))
      .sortBy(r => (r.title, r.year.fold("")(_.toString), r.id.value))

    def cardId(id: String): String = id.split("~", 2) match {
      case Array(film, variant) => s"${keyById.getOrElse(film, film)}~$variant"
      case _                    => keyById.getOrElse(id, id)
    }

    def row(schedule: FilmSchedule): FilmSchedule =
      schedule.copy(resolved = schedule.resolved.copy(_id = cardId(schedule.resolved._id)))

    def screenings(byFilm: Map[String, Map[String, Seq[Showtime]]]): Map[String, Map[String, Seq[Showtime]]] =
      byFilm.map { case (id, slots) => cardId(id) -> slots }
  }

  def apply(records: Seq[StoredMovieRecord], normalizer: TitleNormalizer): Keyed = new Keyed(records, normalizer)
}
