package services.movies

import models.CityScreening

/**
 * A CITY RENAME's read-model rows, refiled under each renamed city's former slug — as an overnight
 * rename leaves them (`CityScreening._id` is `film|city|cinema`) — worked out in memory and handed
 * back as its NET effect on the store: the rows to upsert and the ids to delete.
 *
 * The convergence suite's next day used to apply it a row at a time, a delete and an upsert per
 * row: two round trips for each of the renamed cities' rows (1,368 US films' worth across 21 cities,
 * run 37150307201). As a net effect it is one bulk upsert plus the deletes, leaving the store
 * exactly as the row-by-row writes did:
 *   - renames apply in order, so a row an earlier rename moved is what a later one partitions;
 *   - a moved row that takes an id already present replaces that row (last write wins);
 *   - an id that is moved on again by a later rename is never written at all.
 */
final case class CityRenameRefile(upserts: Seq[CityScreening], deletes: Seq[String])

object CityRenameRefile {
  /** `renames` are (current slug, former slug) pairs, applied in order. */
  def of(screenings: Seq[CityScreening], renames: Seq[(String, String)]): CityRenameRefile = {
    val moved = renames.foldLeft(screenings) { case (rows, (current, former)) =>
      val (moving, staying) = rows.partition(_.city == current)
      staying ++ moving.map(sc => sc.copy(_id = sc._id.replace(s"|$current|", s"|$former|"), city = former))
    }
    val originalById = screenings.map(sc => sc._id -> sc).toMap
    val finalById    = moved.map(sc => sc._id -> sc).toMap
    CityRenameRefile(
      upserts = moved.map(_._id).distinct.map(finalById).filterNot(sc => originalById.get(sc._id).contains(sc)),
      deletes = screenings.map(_._id).distinct.filterNot(finalById.contains))
  }
}
