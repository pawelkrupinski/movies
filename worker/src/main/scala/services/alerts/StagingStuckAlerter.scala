package services.alerts

import play.api.Logging
import services.Stoppable
import services.staging.{StagingRecord, StagingRepo}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically scans `pending_movies` for newcomer films that have been sitting
 * unresolved — no TMDB conclusion, i.e. neither a hit nor a definitive no-match
 * ([[models.MovieRecord.tmdbConcluded]] is false) — for longer than
 * `stuckThreshold`, and fires ONE Telegram alert naming them. These are films the
 * staging promoter re-tries every pass but never concludes: a persistent TMDB
 * miss, a detail page that won't yield director/originalTitle hints, a title the
 * resolver can't disambiguate. An unconcluded row never folds into `movies`, so
 * it never reaches the app — a silent data hole worth a nudge.
 *
 * Why an in-memory "first seen unresolved" clock instead of the row's stored
 * `updatedAt`: a still-showing newcomer is re-scraped — and re-upserted, bumping
 * `updatedAt` (`MovieCache` line ~721) — every scrape tick, so `updatedAt` tracks
 * "last seen", never "first seen". It would almost never cross a 1h threshold for
 * exactly the actively-showing films we care about, which are the only ones left
 * in staging (a film a cinema stops listing is pruned). So we time each
 * unresolved row from when THIS worker first observed it unresolved.
 *
 * Like [[FilmwebDropAlerter]] the clock resets on restart — which also stops a
 * redeploy from re-paging every already-stuck film; the trade-off is that a film
 * stuck across a restart waits up to one more threshold before alerting. The
 * worker runs as a single machine, so one in-memory map is the whole truth (no
 * cluster dedup needed).
 *
 * A row that concludes or stops being listed (pruned from staging, or folded into
 * `movies`) drops from tracking, so a future re-occurrence can alert again. Each
 * stuck film alerts once until it clears.
 */
class StagingStuckAlerter(
  stagingRepo:    StagingRepo,
  notify:         String => Unit,  // deliver one alert message (e.g. TelegramNotifier.send)
  stuckThreshold: FiniteDuration = 1.hour,
  interval:       FiniteDuration = 10.minutes,
  clock:          Clock          = Clock.systemUTC()
) extends Stoppable with Logging {

  // staging `_id` -> instant this worker first saw the row unresolved.
  private val firstSeenUnresolved = mutable.Map.empty[String, Instant]
  // staging `_id`s already alerted, so each stuck film pages once until it clears.
  private val alerted             = mutable.Set.empty[String]

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("staging-stuck-alerter")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(runOnce()), interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"StagingStuckAlerter started — alerting on staging rows unresolved > ${stuckThreshold.toMinutes}m, scanning every ${interval.toMinutes}m.")
  }

  override def stop(): Unit = { scheduler.shutdown(); () }

  /** One scan pass: reconcile the tracking maps against the current unresolved
   *  staging rows, then alert (once) on any that have now been unresolved past the
   *  threshold. Returns the alert text sent this pass (`None` when nothing crossed),
   *  exposed for tests. `synchronized` so a test's manual `runOnce` can't race a
   *  scheduled tick. */
  private[alerts] def runOnce(): Option[String] = synchronized {
    val now        = clock.instant()
    val unresolved = stagingRepo.findAll().filterNot(_.record.tmdbConcluded)
    val currentIds = unresolved.map(idOf).toSet

    // Drop rows that resolved or vanished since last pass — frees them to re-alert.
    firstSeenUnresolved.keys.filterNot(currentIds).toList.foreach { id =>
      firstSeenUnresolved.remove(id)
      alerted.remove(id)
    }

    val cutoff = now.minusMillis(stuckThreshold.toMillis)
    val newlyStuck = unresolved.filter { r =>
      val since = firstSeenUnresolved.getOrElseUpdate(idOf(r), now)
      // Crossed the threshold (first-seen at or before the cutoff) AND not yet
      // alerted. `alerted.add` is only reached when the row is past the cutoff
      // (&& short-circuits), so a still-young row is never prematurely marked.
      !since.isAfter(cutoff) && alerted.add(idOf(r))
    }

    if (newlyStuck.isEmpty) None
    else {
      val msg = alertText(newlyStuck)
      notify(msg)
      logger.warn(s"StagingStuckAlerter: ${newlyStuck.size} film(s) unresolved in staging > ${stuckThreshold.toMinutes}m.")
      Some(msg)
    }
  }

  private def idOf(r: StagingRecord): String = StagingRecord.idFor(r.cinema, r.title, r.year)

  /** One bullet per film (`title (year)` + the cinemas reporting it), titles
   *  sorted so the message is stable. Rows are per-cinema, so a film showing at
   *  several cinemas collapses to one bullet. */
  private def alertText(rows: Seq[StagingRecord]): String = {
    val bullets = rows
      .groupBy(r => (r.title, r.year))
      .toSeq
      .sortBy { case ((title, _), _) => title }
      .map { case ((title, year), group) =>
        val cinemas = group.map(_.cinema.displayName).distinct.sorted.mkString(", ")
        s"• $title${year.fold("")(y => s" ($y)")} — $cinemas"
      }
    s"🎬⏳ Stuck in staging > ${stuckThreshold.toMinutes}m (unenriched / TMDB-unresolved):\n" + bullets.mkString("\n")
  }
}
