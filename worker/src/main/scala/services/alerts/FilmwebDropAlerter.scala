package services.alerts

import models.Cinema
import services.cinemas.{ScrapeOutcome, ScrapeOutcomeListener}

import scala.collection.concurrent.TrieMap

/**
 * Watches the Filmweb-sourced venues for the "Filmweb dropped this venue"
 * failure mode: a venue whose seances feed silently empties or starts 404ing
 * because Filmweb removed it. (`FilmwebShowtimesClient` looks 7 days ahead, so an
 * empty result means NO showings all week — a strong dropped signal, not a quiet
 * night.) After `threshold` consecutive empty-or-failing scrapes a single
 * Telegram alert fires so we migrate the venue to an own-site / platform
 * scraper; a later successful scrape clears it so a fresh drop can re-alert.
 *
 * Only venues that have served data at least once SINCE THIS WORKER STARTED can
 * alert. That single rule does double duty: it excludes the genuinely
 * film-dormant Filmweb venues (Tatry, artKino, … — never any data, so never an
 * alert) without a hand-kept exclusion list, AND it stops a redeploy from
 * re-paging every already-dropped venue. The trade-off: a venue that drops
 * across a restart (no success observed yet this lifetime) is missed until it
 * briefly returns — acceptable for a migrate-me nudge, and the /uptime page
 * still shows it white.
 *
 * Non-Filmweb cinemas are ignored — their health is the general `UptimeMonitor`'s
 * (and `FilmwebFallbackScraper`'s) job, not this Filmweb-specific watcher.
 *
 * Thread-safety: each cinema's outcomes arrive sequentially (one fetch per
 * tick) while different cinemas run concurrently on different keys, so the
 * `TrieMap`s suffice without coarse locking.
 */
class FilmwebDropAlerter(
  filmwebCinemas: Set[String],     // displayNames of the Filmweb-backed venues
  notify:         String => Unit,  // deliver one alert line (e.g. TelegramNotifier.send)
  threshold:      Int             = 3
) extends ScrapeOutcomeListener {

  private val everServed = TrieMap.empty[String, Unit]   // venue has served data this lifetime
  private val downStreak = TrieMap.empty[String, Int]    // consecutive empty/fail count
  private val alerted    = TrieMap.empty[String, Unit]   // currently alerted (dedup until recovery)

  def onOutcome(cinema: Cinema, outcome: ScrapeOutcome): Unit = {
    val name = cinema.displayName
    if (!filmwebCinemas.contains(name)) return
    outcome match {
      case ScrapeOutcome.Success =>
        everServed.putIfAbsent(name, ())
        downStreak.remove(name)
        alerted.remove(name)            // recovered — a future drop can alert again
      case ScrapeOutcome.Empty | ScrapeOutcome.Failure =>
        val streak = downStreak.getOrElse(name, 0) + 1
        downStreak.update(name, streak)
        // putIfAbsent returns None the first crossing → alert once until recovery.
        if (everServed.contains(name) && streak >= threshold && alerted.putIfAbsent(name, ()).isEmpty)
          notify(alertText(cinema, outcome, streak))
    }
  }

  private def alertText(cinema: Cinema, outcome: ScrapeOutcome, streak: Int): String = {
    val how = outcome match {
      case ScrapeOutcome.Failure => "failing"
      case _                     => "empty"
    }
    s"🎬⚠️ Filmweb likely dropped: ${cinema.displayName} — " +
      s"$how for $streak consecutive scrapes. Move it to an own-site / platform scraper."
  }
}
