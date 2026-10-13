package tools

import com.sun.net.httpserver.HttpExchange
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

/**
 * A page that leaves its document by its own hand (a picker setting `location`, a
 * form posting back) passes through a window where the NEW document has committed
 * but its parser has not yet built `<html>`: `document.documentElement` is null.
 * A `waitFor` on a property of that root throws a TypeError in that window — the
 * `PrivacyLanguageSwitchSpec` CI failure, 2026-10-13, "Cannot read properties of
 * null (reading 'lang')". The window is held open here by a server that answers
 * the navigation's headers at once and its body only later.
 */
class CdpNavigateBySpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  private val BodyDelayMs = 1500L

  private def slowBody(exchange: HttpExchange): Boolean =
    exchange.getRequestURI.getPath == "/slow" && {
      exchange.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
      exchange.sendResponseHeaders(200, 0)
      val out = exchange.getResponseBody
      out.flush()
      Thread.sleep(BodyDelayMs)
      out.write("<!doctype html><html lang=\"de\"><body>new</body></html>".getBytes(StandardCharsets.UTF_8))
      out.close()
      true
    }

  private def withPage(body: CdpPage => Unit): Unit = {
    val server = new TestHttpServer(
      routes = { case _ => "<!doctype html><html lang=\"en\"><body>old</body></html>" },
      dynamicRoute = slowBody)
    try Chrome.tryStart(configuration.cdpBrowserBinary) match {
      case None         => cancel("Chrome not installed")
      case Some(chrome) =>
        try chrome.openPage(s"${server.baseUrl}/") { page => body(page) }
        finally chrome.close()
    }
    finally server.close()
  }

  "waiting on the new document's root" should "meet the window where that root does not exist yet" in withPage { page =>
    page.eval("location.href = '/slow'")
    val thrown = the [RuntimeException] thrownBy page.waitFor("document.documentElement.lang === 'de'")
    thrown.getMessage should include ("Cannot read properties of null")
  }

  "navigateBy" should "return only once the document the page navigated to has loaded" in withPage { page =>
    page.navigateBy("location.href = '/slow'")
    page.evalString("document.documentElement.lang") shouldBe "de"
    page.evalString("document.body.textContent") shouldBe "new"
  }
}
