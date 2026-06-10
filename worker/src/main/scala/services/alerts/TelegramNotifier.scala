package services.alerts

import play.api.Logging
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Posts a message to a Telegram chat (optionally a forum topic) via the Bot API
 * `sendMessage` endpoint. Uses GET with URL-encoded query params so it works
 * through any `HttpFetch` (incl. GET-only test fakes). Best-effort: a delivery
 * failure is logged, never thrown — an alert must never break the scrape tick
 * that triggered it.
 */
class TelegramNotifier(http: HttpFetch, botToken: String, chatId: Long, topicId: Option[Long]) extends Logging {
  def send(text: String): Unit =
    Try {
      val url = s"https://api.telegram.org/bot$botToken/sendMessage?chat_id=$chatId" +
        topicId.fold("")(t => s"&message_thread_id=$t") +
        s"&text=${URLEncoder.encode(text, StandardCharsets.UTF_8)}"
      http.get(url)
      ()
    }.recover { case ex => logger.warn(s"Telegram notify failed: ${ex.getMessage}") }.getOrElse(())
}
