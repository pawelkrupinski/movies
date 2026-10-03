package services.movies

import play.api.Logging
import services.retention.StampedRows

import java.time.Clock
import scala.concurrent.duration._

/**
 * Deletes the per-film enrichment state of films the corpus no longer holds.
 *
 * `freshness`, `enrichment_attempts` and `rating_cadence` keep one row per (source, film), keyed
 * `<source>|tmdb:<id>` — and nothing deleted one when its film left the corpus: every film the country
 * ever screened kept its rating stamps, last attempts and cadence for good.
 *
 * A sweep deletes a row when ALL of these hold:
 *  - its key is a film's TMDB key (`<source>|tmdb:<id>`): the title-keyed rows (a film with no tmdbId,
 *    the venue detail stamps) carry no identity this sweep can check, and are left;
 *  - no film of the corpus carries that tmdbId — read from a COMPLETE corpus scan (`liveTmdbIds`
 *    answers `None` for an incomplete one, and nothing is deleted);
 *  - it was last written longer ago than `keepFor`, so a film that drops out for a week and comes back
 *    keeps its cadence, and a film resolved during the sweep is not raced;
 *  - it still carries the stamp the scan read when it is deleted.
 *
 * Every store is scanned before the corpus is read, and the corpus before anything is deleted: a film
 * present at any moment of the corpus read protects its rows, and a scan that fails deletes nothing.
 */
final class OrphanFilmStateSweep(
  stores:      Seq[(String, StampedRows)],
  liveTmdbIds: () => Option[Set[Int]],
  clock:       Clock,
  keepFor:     FiniteDuration = OrphanFilmStateSweep.KeepFor
) extends Logging {
  import OrphanFilmStateSweep._

  /** Rows deleted per store; empty when the corpus read was incomplete. */
  def sweep(): Map[String, Int] = {
    val cutoff  = clock.instant().minusMillis(keepFor.toMillis)
    val scanned = stores.map { case (name, rows) => (name, rows, rows.stampedBefore(cutoff)) }
    liveTmdbIds() match {
      case None =>
        logger.warn("orphan film-state sweep: the corpus read was incomplete — nothing deleted")
        Map.empty
      case Some(live) =>
        val deleted = scanned.map { case (name, rows, stamped) =>
          name -> rows.deleteIfStill(stamped.filter { case (key, _) => tmdbIdOf(key).exists(id => !live(id)) })
        }.toMap
        logger.info(s"orphan film-state sweep: ${deleted.toSeq.sortBy(_._1).map { case (n, c) => s"$n $c" }.mkString(", ")} " +
          s"deleted (TMDB-keyed rows of films gone from the corpus, unwritten for ${keepFor.toDays}d)")
        deleted
    }
  }
}

object OrphanFilmStateSweep {
  /** How long a gone film's rows are kept since they were last written. */
  val KeepFor: FiniteDuration = 30.days
  /** How often the sweep runs. */
  val Interval: FiniteDuration = 24.hours

  private val TmdbKey = """^[a-z]+\|tmdb:(\d+)$""".r
  /** The film a row's key names by its TMDB id, if that is how it is keyed. */
  def tmdbIdOf(key: String): Option[Int] = key match {
    case TmdbKey(id) => id.toIntOption
    case _           => None
  }

  /** The tmdbIds of every film the repository holds, or `None` when it could not be read whole — read
   *  from `movies` alone, the cheapest scan there is: a tmdbId is the row's own field, not a side row's. */
  def liveTmdbIds(repository: MovieRepository): () => Option[Set[Int]] = () => {
    val ids      = Set.newBuilder[Int]
    val complete = repository.foreachRecordWithoutShowtimes(_.record.tmdbId.foreach(ids += _))
    Option.when(complete.isComplete)(ids.result())
  }
}
