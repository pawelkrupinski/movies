package services.identity

import java.time.LocalDateTime

import models.StoredMovieRecord
import models.{Source, ShowtimesDigest}

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
  /** How many of the films a refusal names in its reason. */
  val NamedFilms: Int = 20
  /** Consecutive refusals before a shrink is accepted — the scrape guards' own grace. */
  val Grace: Int = services.movies.ScrapeHealth.MaxConsecutiveDepthRejections

  /** Why `draft` must not be written over `stored`, if it must not. `stored` are the films the draft stands in for —
   *  the whole corpus, or a projection's scope ([[ProjectionScope]]) — and `corpus` every stored film: the films outside
   *  the scope stay as they are, so the shares are of the whole corpus either way. */
  def refusal(draft: ProjectionDraft, stored: Seq[StoredMovieRecord], now: LocalDateTime, corpus: Seq[StoredMovieRecord]): Option[String] = {
    def upcoming(records: Iterable[models.MovieRecord]): Long = records.iterator.flatMap(_.data.iterator)
      .collect { case (source, sd) if Source.cinemaOf(source).isDefined => ShowtimesDigest.upcomingShowtimeCount(sd, now).toLong }.sum
    val before = upcoming(corpus.map(_.record))
    val after  = before - upcoming(stored.map(_.record)) + upcoming(draft.drafts.map(_.record))
    // A card is a film with a showtime still to come: one whose screenings are all past is on no
    // page, so its leaving takes nothing off the site (the old path held such films until its daily
    // cleanup, so a first projection would otherwise count a day of finished films as lost).
    val vanished = draft.vanished.toSet
    val leaving  = stored.filter(s => vanished(s.id) && upcoming(Seq(s.record)) > 0)
    tooManyVanish(leaving, corpus.size).orElse(tooManyShowtimesLost(before, after))
  }

  /** More than [[MaxVanishedShare]] of the corpus's `films` `leaving` the site, the first [[NamedFilms]] of them named. */
  private def tooManyVanish(leaving: Seq[StoredMovieRecord], films: Int): Option[String] = {
    val cards = leaving.size
    Option.when(films > 0 && cards > films * MaxVanishedShare)(
      s"$cards of $films films would vanish (more than ${percent(MaxVanishedShare)}): " +
        leaving.map(_.title).sorted.take(NamedFilms).mkString(", ") + (if (cards > NamedFilms) ", …" else ""))
  }

  /** The upcoming showtimes falling by more than [[MaxShowtimeLossShare]], `before` to `after`. */
  private def tooManyShowtimesLost(before: Long, after: Long): Option[String] =
    Option.when(before > 0 && after < before * (1 - MaxShowtimeLossShare))(
      s"upcoming showtimes would fall from $before to $after (more than ${percent(MaxShowtimeLossShare)})")

  private def percent(share: Double): String = f"${share * 100}%.1f%%"
}
