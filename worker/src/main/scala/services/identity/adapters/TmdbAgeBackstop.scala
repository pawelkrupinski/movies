package services.identity
package adapters

import play.api.Logging

import java.time.Clock
import scala.concurrent.duration._

/**
 * The film records and people the model reads that TMDB last gave longer ago than [[MaxAge]]: fetched again by the
 * lookup fill, after its gaps and refreshes and within what its round has left, at most [[PerRound]] a round, oldest
 * first. Records and people are otherwise renewed only when TMDB's changes name them (`TmdbChangesSweep`), and an edit
 * the feed never showed — older than its 14 days of history when the sweep was behind longer, or one it read as
 * irrelevant — left the record as it was for good (docs/design/identity-questions.md, "an age backstop").
 *
 * The store is scanned at most every [[ScanEvery]], and only while the model is taken up (`liveKeys`): an answer it no
 * longer reads is the store sweep's, not asked again. One fetched again is re-stamped by the store, so the next scan
 * does not name it; one that failed is named again by the next.
 */
final class TmdbAgeBackstop(documents: TmdbDocumentRetention, liveKeys: () => Option[Set[String]], clock: Clock,
                            maxAge: FiniteDuration = TmdbAgeBackstop.MaxAge, perRound: Int = TmdbAgeBackstop.PerRound)
    extends Logging {
  import TmdbAgeBackstop._

  private var pending   = Vector.empty[Aged]
  private var scannedAt = Option.empty[Long]

  /** What this round should fetch again, taken off what is pending: the fill asks for it once a round. A scan that
   *  throws is tried again the next round. */
  def due(): Seq[Aged] = synchronized {
    val now = clock.millis()
    if (pending.isEmpty && scannedAt.forall(now - _ >= ScanEvery.toMillis)) {
      pending = scan(now)
      scannedAt = Some(now)
      if (pending.nonEmpty) logger.info(s"TMDB age backstop: ${pending.size} records and people the model reads unfetched for ${maxAge.toDays}d")
    }
    val (taken, left) = pending.splitAt(perRound)
    pending = left
    taken
  }

  private def scan(now: Long): Vector[Aged] = liveKeys().fold(Vector.empty[Aged]) { live =>
    def aged(kind: TmdbKind)(of: Int => Aged) =
      documents.fetchedBefore(kind, now - maxAge.toMillis).collect {
        case (id, at) if live.contains(TmdbStore.keyOf(kind, id)) && id.toIntOption.isDefined => at -> of(id.toInt)
      }
    (aged(TmdbKind.Film)(Aged.Film(_)) ++ aged(TmdbKind.Person)(Aged.Person(_))).sortBy(_._1).map(_._2).toVector
  }
}

object TmdbAgeBackstop {
  /** How long TMDB's record of a film or person the model reads is kept without asking again: as long as an unread one
   *  is kept (`TmdbStoreSweep.KeepUnread`). */
  val MaxAge: FiniteDuration = 30.days
  /** The most fetched again in one round: a first scan's backlog drains over the day's rounds (48 at the fill's 30 min),
   *  not in one burst. */
  val PerRound: Int = 50
  /** How often the store is scanned for what has aged, at most. */
  val ScanEvery: FiniteDuration = 6.hours

  enum Aged {
    case Film(tmdbId: Int)
    case Person(tmdbId: Int)
  }

  /** None ever: a fill without a store to scan. */
  val Never: TmdbAgeBackstop = new TmdbAgeBackstop(
    new TmdbDocumentRetention {
      def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = Nil
      def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int = 0
    },
    () => None,
    Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)
  )
}
