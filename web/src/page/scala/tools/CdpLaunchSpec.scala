package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}
import settings.CdpBrowserBinary

import java.net.URI
import java.nio.file.Files
import java.util.concurrent.TimeUnit

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
class CdpLaunchSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  /** The pids of the browser processes answering on `chrome`'s DevTools port. */
  private def answeringBrowsers(chrome: Chrome): Seq[Long] = {
    val version = Json.parse(Chrome.httpGet(s"http://${Chrome.Loopback}:${chrome.debuggingPort}/json/version"))
    val browser = new CdpPage(URI.create((version \ "webSocketDebuggerUrl").as[String]))
    try (browser.send("SystemInfo.getProcessInfo") \ "processInfo").as[Seq[JsValue]]
      .collect { case p if (p \ "type").as[String] == "browser" => (p \ "id").as[Long] }
    finally browser.close()
  }

  private def withChrome(body: Chrome => Unit): Unit =
    Chrome.tryStart(configuration.cdpBrowserBinary) match {
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

  // Main #2146: Chrome exited with 133 at launch and the failure said only that, its own CHECK message discarded.
  "A browser that exits at launch" should "fail naming its exit code and what it printed" in {
    val dir     = Files.createTempDirectory("chrome-cdp-test-crasher-")
    val crasher = dir.resolve("chrome")
    Files.writeString(crasher, "#!/bin/sh\necho 'FATAL: Check failed: shared memory' >&2\nexit 133\n")
    crasher.toFile.setExecutable(true)
    try {
      val failure = the[IllegalStateException] thrownBy Chrome.tryStart(Some(CdpBrowserBinary(crasher)))
      failure.getMessage should include("(exited with 133)")
      failure.getMessage should include("FATAL: Check failed: shared memory")
    } finally Chrome.deleteProfile(dir)
  }

  // A browser that died mid-suite (2026-10-27, /dev/shm outgrown in a CI container) failed every later test with a
  // bare "Connection refused" from opening its tab: the failure now says the browser itself is gone.
  "A Chrome whose browser process has exited" should "fail the next tab saying so, not with a refused connection" in withChrome { chrome =>
    val browser = ProcessHandle.of(chrome.browserPid).orElseThrow()
    browser.destroyForcibly()
    browser.onExit().get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
    val failure = the[IllegalStateException] thrownBy chrome.openPage("about:blank")(_ => ())
    failure.getMessage should include(s"Chrome (pid ${chrome.browserPid}) exited")
  }
}
