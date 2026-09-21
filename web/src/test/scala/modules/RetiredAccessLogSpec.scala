package modules

import ch.qos.logback.classic.spi.ILoggingEvent
import controllers.{RetiredAccessLog, RetiredSiteController, WellKnownController}
import models.Country
import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{Action, AnyContent, Handler, Result, Results}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import tools.LogCapture

import scala.concurrent.Future

/**
 * The behavioural half of [[RetiredAccessLog]] — [[LogbackConfigSpec]] already
 * guards that a call to it reaches a real appender under `logback.xml`; this
 * guards that [[RetiredSiteController]] actually MAKES that call, with the
 * right `outcome`, for each of the three ways a retired host can answer a
 * request, AND that the bot/human signal fields (`ip`, `ua`, `referer`,
 * `lang`) actually come off the real incoming headers rather than always
 * falling back to `-`. See [[RetiredSiteSpec]] for the page-rendering
 * behaviour these same routes produce.
 */
class RetiredAccessLogSpec extends AnyFlatSpec with Matchers {

  private val country = Country.Poland

  private val router = AppLoader.retiredRoutes(
    new RetiredSiteController(
      Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi), country),
    new WellKnownController(Helpers.stubControllerComponents()),
    file => Helpers.stubControllerComponents().actionBuilder(Results.Ok(s"asset:$file")))

  private def hit(method: String, path: String, headers: Seq[(String, String)] = Seq.empty): Seq[ILoggingEvent] =
    LogCapture.thisThread(RetiredAccessLog.LoggerName) {
      val request = FakeRequest(method, path).withHeaders(headers*)
      router.routes.lift(request) match {
        case Some(action: Action[?]) =>
          val result: Future[Result] = action.asInstanceOf[Action[AnyContent]].apply(request)
          status(result) // force evaluation before the capture block exits
        case Some(other: Handler) => fail(s"$method $path routed to a non-action handler: $other")
        case None                 => fail(s"$method $path is not routed at all")
      }
    }

  private def onlyMessage(events: Seq[ILoggingEvent]): String = {
    val messages = events.map(_.getFormattedMessage)
    messages should have size 1
    messages.head
  }

  "the landing" should "log a notice hit" in {
    onlyMessage(hit("GET", "/")) shouldBe "notice GET / ip=- ua=- referer=- lang=-"
  }

  "a known city page" should "log a notice hit" in {
    onlyMessage(hit("GET", "/poznan/")) shouldBe "notice GET /poznan/ ip=- ua=- referer=- lang=-"
  }

  "a segment that isn't one of this country's cities" should "log a redirect hit" in {
    onlyMessage(hit("GET", "/not-a-city/")) shouldBe "redirect GET /not-a-city/ ip=- ua=- referer=- lang=-"
  }

  "a deep link into a film page" should "log a notice hit" in {
    onlyMessage(hit("GET", "/poznan/movie/diuna-czesc-druga")) shouldBe
      "notice GET /poznan/movie/diuna-czesc-druga ip=- ua=- referer=- lang=-"
  }

  "an API call" should "log an upgrade hit, not a notice or redirect" in {
    onlyMessage(hit("GET", "/api/catalog")) shouldBe "upgrade GET /api/catalog ip=- ua=- referer=- lang=-"
  }

  "a write outside /api/" should "log a redirect hit" in {
    onlyMessage(hit("POST", "/auth/token")) shouldBe "redirect POST /auth/token ip=- ua=- referer=- lang=-"
  }

  "a machine file" should "log a redirect hit" in {
    onlyMessage(hit("GET", "/robots.txt")) shouldBe "redirect GET /robots.txt ip=- ua=- referer=- lang=-"
  }

  // The whole point of this file: `Fly-Client-IP`, `User-Agent`, `Referer` and
  // `Accept-Language` are the fields a person triaging traffic actually reads
  // to tell a bot from a real visitor — assert each one lands verbatim, not
  // just that the no-header case falls back to `-`.
  "a request carrying the bot/human signal headers" should "log all four verbatim" in {
    onlyMessage(hit("GET", "/poznan/", Seq(
      "Fly-Client-IP"    -> "203.0.113.42",
      "User-Agent"       -> "Mozilla/5.0 (compatible; SomeBot/1.0)",
      "Referer"          -> "https://example.com/",
      "Accept-Language"  -> "pl-PL,pl;q=0.9"))) shouldBe
      "notice GET /poznan/ ip=203.0.113.42 ua=Mozilla/5.0 (compatible; SomeBot/1.0) " +
        "referer=https://example.com/ lang=pl-PL,pl;q=0.9"
  }
}
