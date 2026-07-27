package services.movies

import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters

/**
 * The addressing shape shared by every side collection split out of `movies` and
 * keyed per cinema slot — [[ScreeningsRepository]] (`screenings`, showtimes) and
 * [[SlotsRepository]] (`movie_slots`, the SourceData metadata).
 *
 * Both store one row per `(filmId, slotKey)` under a composite `_id`, index `filmId`
 * for per-film reads, and prune stale slots with the same `$nin` predicate. Keeping
 * that in ONE place is what stops the two drifting: a change to how a composite id is
 * formed or parsed has to stay consistent across them, and the `$nin: []` edge case
 * below is load-bearing for both.
 */
object SlotKeyed {

  /** Non-printable separator, so a composite `_id` can never collide with a slot key
   *  (which itself uses `␟` between cinema and title). */
  val IdSep: Char = '\u001f'

  def idOf(filmId: String, slotKey: String): String = s"$filmId$IdSep$slotKey"

  /** The `filmId` prefix of a composite `_id` — how a DELETE change event, which
   *  carries no post-image, recovers which film changed. */
  def filmIdOf(compositeId: String): String = compositeId.takeWhile(_ != IdSep)

  /** Every stored row of one film, in either side collection — the per-film read/delete
   *  predicate. Shared so a caller that reaches a side collection directly (the staging
   *  fold, which deletes `movies` rows inside its own transaction and must take their
   *  slots + screenings with them) keys on the same field as the repositories do. */
  def filmFilter(filmId: String): Bson = Filters.eq("filmId", filmId)

  /** The stored rows of `filmId` that `keep` no longer names — the DELETE half of a
   *  `replaceFilm`, as ONE server-side predicate rather than a read plus a delete per
   *  stale slot.
   *
   *  Keys on the `filmId` + `slotKey` FIELDS rather than re-deriving the composite
   *  `_id`, so it stays unambiguous even for a `filmId` that itself contains [[IdSep]].
   *
   *  `keep` EMPTY yields `$nin: []` — nothing is a member of the empty set, so it
   *  matches EVERY row of the film. An empty slot map therefore clears the film
   *  exactly as a "delete every key the read returned" loop did. This predicate is the
   *  only thing standing between a whole-record write and a film's stored rows, so it
   *  is unit-tested directly. */
  def staleSlotsFilter(filmId: String, keep: Set[String]): Bson =
    Filters.and(Filters.eq("filmId", filmId), Filters.nin[String]("slotKey", keep.toSeq*))
}
