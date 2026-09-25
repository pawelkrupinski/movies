package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}

import java.net.URI

/**
 * `waitFor`'s budget is the time the PAGE had to reach the state, not wall time
 * the operating system spent not running it.
 *
 * On a loaded laptop (several sbt builds, swap nearly full) Chrome's renderer is
 * sometimes descheduled or paged out for seconds at a stretch. Measured with the
 * keyboard day-step slide, which commits ~610 ms after the step: an evaluate sent
 * 114 ms in did not return until 2.2 s, reporting "not yet" from page time 2304 ms,
 * and the slide committed at 2311 ms — seven page-milliseconds after the budget had
 * already been spent on a freeze the page could do nothing about. Other freezes in
 * the same loop ran 3 s, 6 s and 21 s. A wall-clock deadline turns any such freeze
 * into a timeout, however generous; so the time an evaluate spends blocked beyond
 * a normal round-trip is not charged to the budget.
 *
 * The freeze here is the real mechanism: every renderer process is SIGSTOPped
 * mid-wait and SIGCONTed later, as the kernel's scheduler or pager would.
 */
class CdpWaitForSpec extends AnyFlatSpec with Matchers {

  private def rendererPids(chrome: Chrome): Seq[Long] = {
    val version = Json.parse(Chrome.httpGet(s"http://${Chrome.Loopback}:${chrome.debuggingPort}/json/version"))
    val browser = new CdpPage(URI.create((version \ "webSocketDebuggerUrl").as[String]))
    try (browser.send("SystemInfo.getProcessInfo") \ "processInfo").as[Seq[JsValue]]
      .collect { case p if (p \ "type").as[String] == "renderer" => (p \ "id").as[Long] }
    finally browser.close()
  }

  private def signal(name: String, pids: Seq[Long]): Unit =
    new ProcessBuilder(("kill" +: s"-$name" +: pids.map(_.toString))*).inheritIO().start().waitFor()

  private def withPage(body: (Chrome, CdpPage) => Unit): Unit = {
    val server = new TestHttpServer(routes = { case _ => "<!doctype html><html><body>page</body></html>" })
    try Chrome.tryStart() match {
      case None         => cancel("Chrome not installed")
      case Some(chrome) =>
        try chrome.openPage(s"${server.baseUrl}/") { page => body(chrome, page) }
        finally chrome.close()
    }
    finally server.close()
  }

  "waitFor" should "not charge a renderer freeze to the page's budget" in withPage { (chrome, page) =>
    // The state turns true 200 ms of page time after the page notices it was frozen
    // (a >1 s gap between 20 ms ticks) — so it is false for the whole freeze and
    // true shortly after, whatever order the resumed renderer runs things in.
    page.eval(
      """(() => { let last = performance.now();
        |  const tick = setInterval(() => {
        |    const now = performance.now();
        |    if (now - last > 1000) { clearInterval(tick); setTimeout(() => { window.settled = true; }, 200); }
        |    last = now;
        |  }, 20); })()""".stripMargin)
    val renderers = rendererPids(chrome)
    renderers should not be empty
    val freezer = new Thread(() => {
      Thread.sleep(100)
      signal("STOP", renderers)
      try Thread.sleep(2500) finally signal("CONT", renderers)
    })
    freezer.start()
    try page.waitFor("window.settled === true", timeoutMs = 2000)
    finally freezer.join()
  }

  it should "still time out on a state the running page never reaches" in withPage { (_, page) =>
    val started = System.nanoTime()
    val failure = intercept[RuntimeException](page.waitFor("window.never === true", timeoutMs = 500))
    failure.getMessage should include ("Timed out after 500ms")
    ((System.nanoTime() - started) / 1000000) should be < 5000L
  }
}
