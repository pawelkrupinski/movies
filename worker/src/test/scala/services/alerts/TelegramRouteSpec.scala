package services.alerts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.*
import tools.Env

class TelegramRouteSpec extends AnyFlatSpec with Matchers {

  private def resolve(vars: (String, String)*)(route: AlertRoute) =
    new ProcessConfiguration(Env.of(vars*)).telegramRoute(route)

  private def missing(settings: String*) = Left(settings.map(MissingSetting(_)))

  "An alert route" should "read the token, the chat id and the optional topic" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "KINOWO_FILMWEB_DROP_TG_CHAT_ID" -> "-100", "KINOWO_FILMWEB_DROP_TG_TOPIC_ID" -> "5")(AlertRoute.FilmwebDrop) shouldBe
      Right(TelegramRoute(TelegramBotToken("T"), TelegramChatId(-100L), Some(TelegramTopicId(5L))))
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "KINOWO_FILMWEB_DROP_TG_CHAT_ID" -> "-100")(AlertRoute.FilmwebDrop) shouldBe
      Right(TelegramRoute(TelegramBotToken("T"), TelegramChatId(-100L), None))
  }

  it should "name every setting whose absence leaves it unrouted" in {
    resolve()(AlertRoute.FilmwebDrop) shouldBe missing("TELEGRAM_BOT_TOKEN", "KINOWO_FILMWEB_DROP_TG_CHAT_ID")
    resolve("TELEGRAM_BOT_TOKEN" -> "T")(AlertRoute.FilmwebDrop) shouldBe missing("KINOWO_FILMWEB_DROP_TG_CHAT_ID")
  }

  it should "take the first chat setting that is set, and name the alternatives together when none is" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "KINOWO_FALLBACK_TG_CHAT_ID" -> "7")(AlertRoute.StagingStuck) shouldBe
      Right(TelegramRoute(TelegramBotToken("T"), TelegramChatId(7L), None))
    resolve("TELEGRAM_BOT_TOKEN" -> "T")(AlertRoute.StagingStuck) shouldBe
      missing("KINOWO_STAGING_STUCK_TG_CHAT_ID or KINOWO_FALLBACK_TG_CHAT_ID")
  }

  it should "count a chat id that is not a number as missing, rather than silently routing nowhere" in {
    resolve("TELEGRAM_BOT_TOKEN" -> "T", "KINOWO_FILMWEB_DROP_TG_CHAT_ID" -> "monitoring")(AlertRoute.FilmwebDrop) shouldBe
      missing("KINOWO_FILMWEB_DROP_TG_CHAT_ID (not a number)")
  }
}
