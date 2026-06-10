package services.alerts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

class TelegramNotifierSpec extends AnyFlatSpec with Matchers {

  /** Captures the URL the notifier fetches (and returns empty). */
  private class CapturingFetch extends GetOnlyHttpFetch {
    var url: String = ""
    def get(u: String): String = { url = u; "" }
  }

  "TelegramNotifier" should "build a sendMessage URL with chat id, topic and URL-encoded text" in {
    val http = new CapturingFetch
    new TelegramNotifier(http, "BOT:TOKEN", chatId = -1003950886618L, topicId = Some(2)).send("Kino Praha down → Filmweb")

    http.url should startWith ("https://api.telegram.org/botBOT:TOKEN/sendMessage?")
    http.url should include ("chat_id=-1003950886618")
    http.url should include ("message_thread_id=2")
    http.url should include ("text=Kino+Praha+down+%E2%86%92+Filmweb")   // space→+, arrow + spaces encoded
  }

  it should "omit message_thread_id when no topic is set" in {
    val http = new CapturingFetch
    new TelegramNotifier(http, "T", chatId = 123L, topicId = None).send("hi")
    http.url should include ("chat_id=123")
    http.url should not include "message_thread_id"
  }

  it should "swallow a delivery failure (never throw into the scrape tick)" in {
    val boom = new GetOnlyHttpFetch { def get(u: String): String = throw new RuntimeException("network down") }
    noException should be thrownBy new TelegramNotifier(boom, "T", 1L, None).send("x")
  }
}
