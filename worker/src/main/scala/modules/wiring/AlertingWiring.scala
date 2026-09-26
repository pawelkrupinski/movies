package modules.wiring

import settings.{AlertRoute, TelegramRoute, FilmwebDropThreshold, ProcessConfiguration, StagingStuckScanInterval, StagingStuckThreshold}

import modules.WorkerWiring
import services.alerts.{AlertBurst, BurstLimitedNotifier, FilmwebDropAlerter, StagingStuckAlerter, TelegramNotifier}
import services.cinemas.common.ScrapeOutcomeListener
import services.metrics.EnvGatedFeature

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** The Telegram alerters, each wired only when its bot token + chat id are
 *  configured (absent in CI / local without secrets → no alerts). An alerter left
 *  off is NOT silent any more: [[alerterFeatures]] feeds the
 *  `kinowo_worker_alerter_enabled` gauge and the boot WARN (see [[EnvGatedFeature]]).
 *  See reference_fallback_telegram_channel. */
trait AlertingWiring { self: WorkerWiring =>

  private def notifierFor(route: TelegramRoute): TelegramNotifier = new TelegramNotifier(httoFetch, route)

  // Telegram alerter for fallback ENTER / RECOVERED events. Posts to the dedicated
  // "Fallback to Filmweb" topic when a topic id is set.
  protected lazy val fallbackTelegramNotifier: Option[TelegramNotifier] =
    configuration.telegramRoute(AlertRoute.FilmwebFallback).toOption.map(notifierFor)

  // Every per-venue fallback page (ENTER / RECOVERED / UNCOVERED, and the gone-venue
  // page) shares ONE burst limit: an aggregator outage hands every venue on it over at
  // once, and a thousand pages bury the one that means something.
  protected lazy val fallbackPager: String => Unit = {
    val limited = fallbackTelegramNotifier.map(notifier =>
      new BurstLimitedNotifier(notifier.send, AlertBurst(10, FiniteDuration(1L, TimeUnit.HOURS)), clock))
    message => limited.foreach(_.send(message))
  }

  // Telegram alerter for the OTHER half of the Filmweb story: a venue whose sole
  // source IS Filmweb (no own-site fallback possible) going empty/404 because
  // Filmweb dropped it — a nudge to migrate it to an own-site scraper. Posts to
  // the dedicated "Filmweb Drops Cinemas" channel; off unless its chat id is set,
  // so CI / local without secrets raise no alerts.
  protected lazy val filmwebDropAlerter: Option[FilmwebDropAlerter] =
    configuration.telegramRoute(AlertRoute.FilmwebDrop).toOption.filter(_ => filmwebEnabled).map { route =>
      new FilmwebDropAlerter(filmwebOnlyCinemas, notifierFor(route).send,
        configuration.filmwebDropThreshold(FilmwebDropThreshold(3)))
    }

  // The single drop-watcher shared across every UptimeRecordingScraper wrap (it
  // self-filters to the Filmweb-only venues), or a no-op when unconfigured.
  protected lazy val scrapeOutcomeListener: ScrapeOutcomeListener =
    filmwebDropAlerter.getOrElse(ScrapeOutcomeListener.NoOp)

  // Telegram alerter for newcomers the promoter can't conclude: a row sitting in
  // `pending_movies` TMDB-unresolved for over an hour never folds into `movies`, so
  // it never reaches the app — a silent data hole. Routes to its own chat if set,
  // else the shared "Kinowo Monitoring" group (KINOWO_FALLBACK_TG_CHAT_ID), so it
  // works on prod without a new secret; off in CI / local without any chat id.
  protected lazy val stagingStuckAlerter: Option[StagingStuckAlerter] =
    configuration.telegramRoute(AlertRoute.StagingStuck).toOption.map { route =>
      new StagingStuckAlerter(stagingRepository, notifierFor(route).send,
        stuckThreshold = configuration.stagingStuckThreshold(StagingStuckThreshold(FiniteDuration(60L, TimeUnit.MINUTES))),
        interval       = configuration.stagingStuckScanInterval(StagingStuckScanInterval(FiniteDuration(10L, TimeUnit.MINUTES))))
    }

  /** Which of this country's alerters are wired, read from the same routes as the
   *  alerters above so the gauge and the wiring cannot disagree. */
  lazy val alerterFeatures: Seq[EnvGatedFeature] = AlertingWiring.alerters(configuration, filmwebEnabled)

  /** Publish [[alerterFeatures]] and WARN, naming the missing keys, for any that is off. */
  def reportAlerters(): Unit = {
    workerMetrics.envGatedFeatures.recordAlerters(country.code, alerterFeatures)
    EnvGatedFeature.disabledWarning("alerter", alerterFeatures).foreach(w => logger.warn(s"[${country.code}] $w"))
  }
}

object AlertingWiring {
  /** Every alerter this country runs, on or off. The Filmweb two are left out where
   *  Filmweb is not wired at all: off by design there, and a 0 would page for nothing. */
  def alerters(configuration: ProcessConfiguration, filmwebEnabled: Boolean): Seq[EnvGatedFeature] = {
    def feature(name: String, route: AlertRoute) = EnvGatedFeature.from(name, configuration.telegramRoute(route))
    val filmweb =
      if (filmwebEnabled) Seq(feature("filmweb_fallback", AlertRoute.FilmwebFallback), feature("filmweb_drop", AlertRoute.FilmwebDrop))
      else Nil
    filmweb :+ feature("staging_stuck", AlertRoute.StagingStuck)
  }
}
