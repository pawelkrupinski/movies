package services.tasks

import models.Country
import play.api.Logging
import services.attempts.RatingKeys
import services.cadence.{RatingCadence, RatingCadenceStore}
import services.movies.{LatestTitleYear, MovieCacheReader}

import java.time.{Clock, Instant, LocalDate}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Each film's release date in the deployment's country, as far as the worker knows it. */
trait ReleaseDates {
  /** The release dates of those of `tmdbIds` that have one. */
  def releasedOn(tmdbIds: Seq[Int]): Map[Int, LocalDate]
}

/**
 * Which rating keys (`<site>|tmdb:<id>`) belong to a film around its release whose site has
 * no rating for it yet — the ones [[RatingSchedule]] holds to [[RatingCadence.PremiereInterval]].
 *
 * Without it a film's rating cadence backs off before release exactly as a settled film's
 * does: nothing changes, so by its premiere it sat at 16–32h, and the first rating landed up
 * to ~1.5 days after the site had one ("Jak żyć, żeby nie zwariować", fw|tmdb:1489938, 32h on
 * the eve of its 2026-10-23 release). A source counts only where this country refreshes it and
 * the row is eligible for it, and only while the row holds no rating from it: once one lands,
 * the key drops out and the normal backoff resumes. A film already rated is never touched.
 *
 * The due schedule asks this for every (film, source) on every reaper tick and handler pickup,
 * so the answer is a set rebuilt at most every [[PremiereRatingWatch.RebuildEvery]] from the
 * cache, with each film's release date read once a day. A rebuild that fails keeps the last
 * set: the cap only ever makes a film refresh sooner.
 */
final class PremiereRatingWatch(
  cache:        MovieCacheReader,
  releases:     ReleaseDates,
  country:      Country,
  clock:        Clock,
  rebuildEvery: FiniteDuration = PremiereRatingWatch.RebuildEvery
)(using LatestTitleYear) extends Logging {
  private val sources = RatingSources.forCountry(country)

  @volatile private var held: Option[(Instant, Set[String])] = None
  // Release dates by film, `None` for a film with none; dropped whole once a day so a corrected date is taken up.
  private var dated: (LocalDate, Map[Int, Option[LocalDate]]) = (LocalDate.MIN, Map.empty)

  /** Is `dedupKey`'s film around its release with no rating from that key's site yet? */
  def awaitingFirstRating(dedupKey: String): Boolean = current().contains(dedupKey)

  private def current(): Set[String] = {
    def fresh(now: Instant) = held.collect { case (at, keys) if now.isBefore(at.plusMillis(rebuildEvery.toMillis)) => keys }
    fresh(clock.instant()).getOrElse(synchronized {
      val now = clock.instant()
      fresh(now).getOrElse {
        val keys = Try(build(now)) match {
          case Success(built)     => built
          case Failure(exception) =>
            logger.warn(s"[${country.code}] premiere rating watch rebuild failed: ${exception.getMessage}")
            held.map(_._2).getOrElse(Set.empty)
        }
        held = Some(now -> keys)
        keys
      }
    })
  }

  private def build(now: Instant): Set[String] = {
    val today   = now.atZone(country.zone).toLocalDate
    val unrated = cache.entries.flatMap { case (_, record) =>
      record.tmdbId.map(_ -> sources.filter(s => s.eligible(record) && !s.rated(record)))
    }.filter(_._2.nonEmpty)
    val releaseOf = datesFor(unrated.map(_._1).distinct, today)
    unrated.iterator.collect {
      case (id, unratedSources) if releaseOf.get(id).flatten.exists(RatingCadence.aroundPremiere(_, today)) =>
        unratedSources.map(s => RatingKeys.tmdbKey(s.kind, id))
    }.flatten.toSet
  }

  private def datesFor(ids: Seq[Int], today: LocalDate): Map[Int, Option[LocalDate]] = {
    val known   = if (dated._1 == today) dated._2 else Map.empty[Int, Option[LocalDate]]
    val missing = ids.filterNot(known.contains)
    val read    = if (missing.isEmpty) Map.empty else releases.releasedOn(missing)
    val all     = known ++ missing.map(id => id -> read.get(id))
    dated = (today, all)
    all
  }
}

object PremiereRatingWatch {
  /** How stale the set may be: a rating that lands keeps its key capped this long at most. */
  val RebuildEvery: FiniteDuration = 10.minutes
}

/** The ONE due schedule every rating refresh shares — the reaper's enqueue gate and each
 *  [[RatingHandler]]'s pickup gate (see [[DueWindow]]): each key's adaptive cadence
 *  ([[RatingCadence]]), capped at [[RatingCadence.PremiereInterval]] while
 *  `awaitingFirstRating` says so ([[PremiereRatingWatch]]). `cadence` is by-name: the
 *  worker builds this window eagerly, ahead of the store it reads at each call. */
object RatingSchedule {
  def dueWindow(cadence: => RatingCadenceStore, awaitingFirstRating: String => Boolean): DueWindow =
    new DueWindow(key => RatingCadence.intervalFor(cadence.statsFor(key), awaitingFirstRating(key)), RatingCadence.BaseInterval)
}
