package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}

import java.net.URI
import java.nio.file.Files

/**
 * A driver must drive the Chrome it launched, whatever else runs on the machine.
 *
 * It used to pick the DevTools port itself (bind 0, read, close) and hand it to Chrome.
 * When another process took the port first — several agents start PageTest Chromes on one
 * laptop — Chrome fell back to `[::1]`, and the probe at `localhost` reached the OTHER
 * browser and drove it until its owner closed it: "Output closed" mid-spec. A port held by
 * anything else read as a Chrome that never started, and the spec cancelled as "Chrome not
 * installed". Chrome now picks and binds its port itself, and the driver reads it from the
 * profile only that Chrome writes, checking the browser that answers is the one named there.
 */
class CdpLaunchSpec extends AnyFlatSpec with Matchers {

  /** The pids of the browser processes answering on `chrome`'s DevTools port. */
  private def answeringBrowsers(chrome: Chrome): Seq[Long] = {
    val version = Json.parse(Chrome.httpGet(s"http://${Chrome.Loopback}:${chrome.debuggingPort}/json/version"))
    val browser = new CdpPage(URI.create((version \ "webSocketDebuggerUrl").as[String]))
    try (browser.send("SystemInfo.getProcessInfo") \ "processInfo").as[Seq[JsValue]]
      .collect { case p if (p \ "type").as[String] == "browser" => (p \ "id").as[Long] }
    finally browser.close()
  }

  private def withChrome(body: Chrome => Unit): Unit =
    Chrome.tryStart() match {
      case None         => cancel("Chrome not installed")
      case Some(chrome) => try body(chrome) finally chrome.close()
    }

  "Chromes started side by side" should "each drive the browser they launched" in withChrome { first =>
    withChrome { second =>
      first.debuggingPort should not be second.debuggingPort
      answeringBrowsers(first) shouldBe Seq(first.browserPid)
      answeringBrowsers(second) shouldBe Seq(second.browserPid)
    }
  }

  "A profile whose DevToolsActivePort names another browser's port" should "not be taken as this Chrome's" in withChrome { other =>
    val profile = Files.createTempDirectory("chrome-cdp-test-foreign-")
    val alive   = new ProcessBuilder("sleep", "30").start()
    try {
      Files.writeString(profile.resolve("DevToolsActivePort"),
        s"${other.debuggingPort}\n/devtools/browser/00000000-0000-0000-0000-000000000000\n")
      Chrome.activePort(alive, profile) shouldBe None
    } finally { alive.destroyForcibly(); Chrome.deleteProfile(profile) }
  }
}
