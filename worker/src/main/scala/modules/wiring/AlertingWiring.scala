package modules.wiring

import modules.WorkerWiring
import services.alerts.{FilmwebDropAlerter, StagingStuckAlerter, TelegramNotifier, TelegramRoute}
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

  private def notifierFor(route: TelegramRoute): TelegramNotifier =
    new TelegramNotifier(httoFetch, route.token, route.chatId, route.topicId)

  // Telegram alerter for fallback ENTER / RECOVERED events. Posts to the dedicated
  // "Fallback to Filmweb" topic when a topic id is set.
  protected lazy val fallbackTelegramNotifier: Option[TelegramNotifier] =
    AlertingWiring.fallbackRoute(env.get).toOption.map(notifierFor)

  // Telegram alerter for the OTHER half of the Filmweb story: a venue whose sole
  // source IS Filmweb (no own-site fallback possible) going empty/404 because
  // Filmweb dropped it — a nudge to migrate it to an own-site scraper. Posts to
  // the dedicated "Filmweb Drops Cinemas" channel; off unless its chat id is set,
  // so CI / local without secrets raise no alerts.
  protected lazy val filmwebDropAlerter: Option[FilmwebDropAlerter] =
    AlertingWiring.filmwebDropRoute(env.get).toOption.filter(_ => filmwebEnabled).map { route =>
      new FilmwebDropAlerter(filmwebOnlyCinemas, notifierFor(route).send,
        env.positiveInt("KINOWO_FILMWEB_DROP_THRESHOLD", 3))
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
    AlertingWiring.stagingStuckRoute(env.get).toOption.map { route =>
      new StagingStuckAlerter(stagingRepository, notifierFor(route).send,
        stuckThreshold = FiniteDuration(env.positiveLong("KINOWO_STAGING_STUCK_MINUTES", 60L), TimeUnit.MINUTES),
        interval       = FiniteDuration(env.positiveLong("KINOWO_STAGING_STUCK_SCAN_MINUTES", 10L), TimeUnit.MINUTES))
    }

  /** Which of this country's alerters are wired, read from the same routes as the
   *  alerters above so the gauge and the wiring cannot disagree. */
  lazy val alerterFeatures: Seq[EnvGatedFeature] = AlertingWiring.alerters(env.get, filmwebEnabled)

  /** Publish [[alerterFeatures]] and WARN, naming the missing keys, for any that is off. */
  def reportAlerters(): Unit = {
    workerMetrics.envGatedFeatures.recordAlerters(country.code, alerterFeatures)
    EnvGatedFeature.disabledWarning("alerter", alerterFeatures).foreach(w => logger.warn(s"[${country.code}] $w"))
  }
}

object AlertingWiring {
  def fallbackRoute(read: String => Option[String]): Either[Seq[String], TelegramRoute] =
    TelegramRoute.resolve(read, Seq("KINOWO_FALLBACK_TG_CHAT_ID"), "KINOWO_FALLBACK_TG_TOPIC_ID")

  def filmwebDropRoute(read: String => Option[String]): Either[Seq[String], TelegramRoute] =
    TelegramRoute.resolve(read, Seq("KINOWO_FILMWEB_DROP_TG_CHAT_ID"), "KINOWO_FILMWEB_DROP_TG_TOPIC_ID")

  def stagingStuckRoute(read: String => Option[String]): Either[Seq[String], TelegramRoute] =
    TelegramRoute.resolve(read, Seq("KINOWO_STAGING_STUCK_TG_CHAT_ID", "KINOWO_FALLBACK_TG_CHAT_ID"),
      "KINOWO_STAGING_STUCK_TG_TOPIC_ID")

  /** Every alerter this country runs, on or off. The Filmweb two are left out where
   *  Filmweb is not wired at all: off by design there, and a 0 would page for nothing. */
  def alerters(read: String => Option[String], filmwebEnabled: Boolean): Seq[EnvGatedFeature] = {
    val filmweb =
      if (filmwebEnabled) Seq(
        EnvGatedFeature.from("filmweb_fallback", fallbackRoute(read)),
        EnvGatedFeature.from("filmweb_drop", filmwebDropRoute(read)))
      else Nil
    filmweb :+ EnvGatedFeature.from("staging_stuck", stagingStuckRoute(read))
  }
}
