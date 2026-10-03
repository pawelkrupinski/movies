package services.alerts

import play.api.Logging
import services.fallback.FallbackEvent
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.Locale
import scala.util.{Failure, Success, Try}

/** Which in-app alert a Telegram message is: the `kind` label on
 *  `kinowo_worker_telegram_notifications_total` and the `kind=` field of the
 *  notifier's log lines. Named by the caller, so a count says WHAT was paged. */
final case class TelegramAlertKind(value: String)

object TelegramAlertKind {
  val GoneVenue: TelegramAlertKind    = TelegramAlertKind("gone_venue")
  val VenueClosure: TelegramAlertKind = TelegramAlertKind("venue_closure")
  val FilmwebDrop: TelegramAlertKind  = TelegramAlertKind("filmweb_drop")

  /** A fallback transition's page: `ENTER` → `fallback_enter`. */
  def fallback(event: String): TelegramAlertKind = TelegramAlertKind(s"fallback_${event.toLowerCase(Locale.ROOT)}")

  /** The kinds the worker pages, seeded at 0 so the failure alert's `increase`
   *  sees the first attempt of each. A kind outside it still counts, unseeded. */
  val all: Seq[TelegramAlertKind] =
    Seq(FallbackEvent.Enter, FallbackEvent.Recovered, FallbackEvent.Uncovered).map(fallback) ++
      Seq(GoneVenue, VenueClosure, FilmwebDrop)
}

/** The `outcome` label values of `kinowo_worker_telegram_notifications_total`. */
object TelegramOutcome {
  val Sent   = "sent"
  val Failed = "failed"
  val all: Seq[String] = Seq(Sent, Failed)
}

/** Counts one delivery attempt; the wiring binds it to a country. */
trait TelegramNotificationRecorder {
  def record(kind: TelegramAlertKind, outcome: String): Unit
}

/**
 * Posts a message to a Telegram chat (optionally a forum topic) via the Bot API
 * `sendMessage` endpoint. Uses GET with URL-encoded query parameters so it works
 * through any `HttpFetch` (incl. GET-only test fakes). Best-effort: a delivery
 * failure is logged, never thrown — an alert must never break the scrape tick
 * that triggered it.
 *
 * Every attempt leaves a trace, a delivered one included: an INFO
 * `Telegram notify sent` or a WARN `Telegram notify failed`, each naming the
 * kind and the country (one JVM runs several countries' wirings), and one
 * increment of `recorder`. Only failures used to log, so nobody could count what
 * had been paged.
 */
class TelegramNotifier(http: HttpFetch, route: settings.TelegramRoute, country: String,
                       recorder: TelegramNotificationRecorder) extends Logging {
  def send(kind: TelegramAlertKind)(text: String): Unit =
    Try {
      http.get(s"https://api.telegram.org/bot${route.token.value}/sendMessage?chat_id=${route.chatId.value}" +
        route.topicId.fold("")(topic => s"&message_thread_id=${topic.value}") +
        s"&text=${URLEncoder.encode(text, StandardCharsets.UTF_8)}")
    } match {
      case Success(_) =>
        recorder.record(kind, TelegramOutcome.Sent)
        logger.info(s"Telegram notify sent: kind=${kind.value} country=$country")
      case Failure(exception) =>
        recorder.record(kind, TelegramOutcome.Failed)
        logger.warn(s"Telegram notify failed: kind=${kind.value} country=$country: ${exception.getMessage}")
    }
}
