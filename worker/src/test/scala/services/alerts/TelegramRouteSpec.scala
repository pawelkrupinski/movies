package services.alerts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TelegramRouteSpec extends AnyFlatSpec with Matchers {

  private def resolve(env: (String, String)*)(chatKeys: String*) =
    TelegramRoute.resolve(env.toMap.get, chatKeys, "TOPIC")

  "TelegramRoute.resolve" should "read the token, the chat id and the optional topic" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "CHAT" -> "-100", "TOPIC" -> "5")("CHAT") shouldBe
      Right(TelegramRoute("T", -100L, Some(5L)))
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "CHAT" -> "-100")("CHAT") shouldBe Right(TelegramRoute("T", -100L, None))
  }

  it should "name every key whose absence leaves it unrouted" in {
    resolve()("CHAT") shouldBe Left(Seq("TELEGRAM_BOT_TOKEN", "CHAT"))
    resolve("TELEGRAM_BOT_TOKEN" -> "T")("CHAT") shouldBe Left(Seq("CHAT"))
  }

  it should "take the first chat key that is set, and name the alternatives together when none is" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "SHARED" -> "7")("OWN", "SHARED") shouldBe Right(TelegramRoute("T", 7L, None))
    resolve("TELEGRAM_BOT_TOKEN" -> "T")("OWN", "SHARED") shouldBe Left(Seq("OWN or SHARED"))
  }

  it should "count a chat id that is not a number as missing, rather than silently routing nowhere" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "CHAT" -> "monitoring")("CHAT") shouldBe Left(Seq("CHAT (not a number)"))
  }
}
