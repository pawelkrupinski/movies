package services.identity

import models.Source
import services.movies.{ShowtimesDigest, StoredMovieRecord}

import java.time.LocalDateTime

/**
 * The projection's own "trust what we have" (docs/design/identity-resolver.md §11): a projection
 * that would take more than [[MaxVanishedShare]] of the stored cards, or more than
 * [[MaxShowtimeLossShare]] of their upcoming showtimes, off the site is refused, and what is stored
 * keeps serving. The first projection of a cutover is diffed against the old path's films this way,
 * and so is every later one — a projection fed a broken listing set must not empty the site.
 *
 * The scrape guards (`ListingIntake`) already hold a single venue's thin scrape; this is the
 * corpus-level check. Like them it has a grace: refused for [[Grace]] projections running, the
 * shrink is taken to be real and lands (the caller counts the refusals).
 */
object ProjectionGuard {

  /** The shares §11 names: a card is a film, a showtime an upcoming screening. */
  val MaxVanishedShare: Double     = 0.02
  val MaxShowtimeLossShare: Double = 0.005
  /** Consecutive refusals before a shrink is accepted — the scrape guards' own grace. */
  val Grace: Int = services.movies.ScrapeHealth.MaxConsecutiveDepthRejections

  /** Why `draft` must not be written over `stored`, if it must not. */
  def refusal(draft: ProjectionDraft, stored: Seq[StoredMovieRecord], now: LocalDateTime): Option[String] = {
    def upcoming(records: Iterable[models.MovieRecord]): Long = records.iterator.flatMap(_.data.iterator)
      .collect { case (source, sd) if Source.cinemaOf(source).isDefined => ShowtimesDigest.upcomingShowtimeCount(sd, now).toLong }.sum
    val before = upcoming(stored.map(_.record))
    val after  = upcoming(draft.drafts.map(_.record))
    val cards  = draft.vanished.size
    if (stored.nonEmpty && cards > stored.size * MaxVanishedShare)
      Some(s"$cards of ${stored.size} films would vanish (more than ${percent(MaxVanishedShare)})")
    else if (before > 0 && after < before * (1 - MaxShowtimeLossShare))
      Some(s"upcoming showtimes would fall from $before to $after (more than ${percent(MaxShowtimeLossShare)})")
    else None
  }

  private def percent(share: Double): String = f"${share * 100}%.1f%%"
}
