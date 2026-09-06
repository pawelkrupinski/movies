package modules.wiring

import modules.WorkerWiring
import services.alerts.{FilmwebDropAlerter, StagingStuckAlerter, TelegramNotifier}
import services.cinemas.common.ScrapeOutcomeListener
import tools.Env

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** The Telegram alerters, each wired only when its bot token + chat id are
 *  configured (absent in CI / local without secrets → no alerts). See
 *  reference_fallback_telegram_channel. */
trait AlertingWiring { self: WorkerWiring =>

  // Telegram alerter for fallback ENTER / RECOVERED events. Posts to the dedicated
  // "Fallback to Filmweb" topic when a topic id is set.
  protected lazy val fallbackTelegramNotifier: Option[TelegramNotifier] = for {
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_FALLBACK_TG_CHAT_ID").flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield new TelegramNotifier(httoFetch, token, chatId,
    Env.get("KINOWO_FALLBACK_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))

  // Telegram alerter for the OTHER half of the Filmweb story: a venue whose sole
  // source IS Filmweb (no own-site fallback possible) going empty/404 because
  // Filmweb dropped it — a nudge to migrate it to an own-site scraper. Posts to
  // the dedicated "Filmweb Drops Cinemas" channel; off unless its chat id is set,
  // so CI / local without secrets raise no alerts.
  protected lazy val filmwebDropAlerter: Option[FilmwebDropAlerter] = for {
    _      <- Option.when(filmwebEnabled)(())
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_FILMWEB_DROP_TG_CHAT_ID").flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield {
    val notifier = new TelegramNotifier(httoFetch, token, chatId,
      Env.get("KINOWO_FILMWEB_DROP_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))
    new FilmwebDropAlerter(filmwebOnlyCinemas, notifier.send,
      Env.positiveInt("KINOWO_FILMWEB_DROP_THRESHOLD", 3))
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
  protected lazy val stagingStuckAlerter: Option[StagingStuckAlerter] = for {
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_STAGING_STUCK_TG_CHAT_ID")
                .orElse(Env.get("KINOWO_FALLBACK_TG_CHAT_ID"))
                .flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield {
    val notifier = new TelegramNotifier(httoFetch, token, chatId,
      Env.get("KINOWO_STAGING_STUCK_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))
    new StagingStuckAlerter(stagingRepository, notifier.send,
      stuckThreshold = FiniteDuration(Env.positiveLong("KINOWO_STAGING_STUCK_MINUTES", 60L), TimeUnit.MINUTES),
      interval       = FiniteDuration(Env.positiveLong("KINOWO_STAGING_STUCK_SCAN_MINUTES", 10L), TimeUnit.MINUTES))
  }
}
