package services.alerts

import settings.{TelegramBotToken, TelegramChatId, TelegramRoute, TelegramTopicId}

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{PrometheusExposition, TelegramNotificationMetrics}
import tools.{GetOnlyHttpFetch, HttpFetch, LogCapture}

class TelegramNotifierSpec extends AnyFlatSpec with Matchers {

  /** Captures the URL the notifier fetches (and returns empty). */
  private class CapturingFetch extends GetOnlyHttpFetch {
    var url: String = ""
    def get(u: String): String = { url = u; "" }
  }

  private val failing: HttpFetch = new GetOnlyHttpFetch { def get(u: String): String = throw new RuntimeException("network down") }

  private def route(chat: Long = 1L) = TelegramRoute(TelegramBotToken("T"), TelegramChatId(chat), None)

  /** Sends one `kind` page through `http` for `country`, returning the rendered
   *  metrics and the notifier's log events from this thread. */
  private def sendOnce(http: HttpFetch, country: String, kind: TelegramAlertKind): (String, Seq[ILoggingEvent]) = {
    val registry = new PrometheusRegistry()
    val metrics  = new TelegramNotificationMetrics(Seq("pl", "us"), registry)
    val events   = LogCapture.thisThread(classOf[TelegramNotifier].getName, Some(Level.INFO)) {
      new TelegramNotifier(http, route(), country, metrics.recorderFor(country)).send(kind)("x")
    }
    (PrometheusExposition.render(registry), events)
  }

  "TelegramNotifier" should "build a sendMessage URL with chat id, topic and URL-encoded text" in {
    val http = new CapturingFetch
    new TelegramNotifier(http, TelegramRoute(TelegramBotToken("BOT:TOKEN"), TelegramChatId(-1003950886618L), Some(TelegramTopicId(2))),
      "pl", (_, _) => ()).send(TelegramAlertKind.GoneVenue)("Kino Praha down → Filmweb")

    http.url should startWith ("https://api.telegram.org/botBOT:TOKEN/sendMessage?")
    http.url should include ("chat_id=-1003950886618")
    http.url should include ("message_thread_id=2")
    http.url should include ("text=Kino+Praha+down+%E2%86%92+Filmweb")   // space→+, arrow + spaces encoded
  }

  it should "omit message_thread_id when no topic is set" in {
    val http = new CapturingFetch
    new TelegramNotifier(http, route(123L), "pl", (_, _) => ()).send(TelegramAlertKind.GoneVenue)("hi")
    http.url should include ("chat_id=123")
    http.url should not include "message_thread_id"
  }

  it should "swallow a delivery failure (never throw into the scrape tick)" in {
    noException should be thrownBy new TelegramNotifier(failing, route(), "pl", (_, _) => ()).send(TelegramAlertKind.GoneVenue)("x")
  }

  // A delivered page used to leave no trace at all, so nobody could count what was sent.
  it should "log a delivered page at INFO and count it as sent, under its kind and country" in {
    val (metrics, events) = sendOnce(new CapturingFetch, "us", TelegramAlertKind.fallback("ENTER"))

    events.map(e => (e.getLevel, e.getFormattedMessage)) shouldBe
      Seq((Level.INFO, "Telegram notify sent: kind=fallback_enter country=us"))
    metrics should include ("""kinowo_worker_telegram_notifications_total{country="us",kind="fallback_enter",outcome="sent"} 1""")
    metrics should include ("""kinowo_worker_telegram_notifications_total{country="us",kind="fallback_enter",outcome="failed"} 0""")
  }

  it should "log a failed page at WARN and count it as failed, under its kind and country" in {
    val (metrics, events) = sendOnce(failing, "pl", TelegramAlertKind.GoneVenue)

    events.map(e => (e.getLevel, e.getFormattedMessage)) shouldBe
      Seq((Level.WARN, "Telegram notify failed: kind=gone_venue country=pl: network down"))
    metrics should include ("""kinowo_worker_telegram_notifications_total{country="pl",kind="gone_venue",outcome="failed"} 1""")
    metrics should include ("""kinowo_worker_telegram_notifications_total{country="pl",kind="gone_venue",outcome="sent"} 0""")
  }

  "TelegramNotificationMetrics" should "seed every country × known kind × outcome at 0, so the failure alert sees a first failure" in {
    val registry = new PrometheusRegistry()
    val _        = new TelegramNotificationMetrics(Seq("pl", "us"), registry)
    val text     = PrometheusExposition.render(registry)
    for (c <- Seq("pl", "us"); k <- TelegramAlertKind.all; o <- TelegramOutcome.all)
      text should include (s"""kinowo_worker_telegram_notifications_total{country="$c",kind="${k.value}",outcome="$o"} 0""")
  }
}
