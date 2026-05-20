package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import tools.Chrome

import java.util.concurrent.TimeUnit

/**
 * One-shot: drives prod through enough page transitions for the perf
 * beacon to dump a handful of records to `/debug/perf`, then prints
 * what the beacons captured so we can read the iOS-feels-slow numbers
 * locally without waiting for organic traffic. Once "headless desktop"
 * and "iPhone-emulated" baselines are recorded, delete the file.
 *
 * Two passes per run — first with the default headless desktop user
 * agent and viewport, then with the Network-domain CPU/network-
 * throttling levers plus an iPhone UA + mobile viewport so we get an
 * idea of what an iPhone session looks like. Real-iPhone numbers still
 * need a real iPhone — this is the "no Mac with Safari to spare"
 * approximation.
 */
class PerfProbeProdSpec extends AnyFlatSpec with Matchers {

  // Five page swaps per pass: /, /kina, /, /kina, /. Each transition
  // fires `pagehide` on the page being left, beaconing a record for
  // that URL. We get three samples for / and two for /kina per pass,
  // which is enough to see medians.
  private val Path = "https://kinowo.fly.dev"

  "perf-probe-prod" should "drive prod through navigations so the perf beacon dumps records" in {
    Chrome.tryStart() match {
      case None         => cancel("Chrome not installed — skipping perf probe")
      case Some(chrome) =>
        try {
          println("\n──── Pass 1: desktop headless ────")
          drive(chrome, throttleAsIphone = false)
          println("\n──── Pass 2: iPhone UA + viewport + 4x CPU throttle ────")
          drive(chrome, throttleAsIphone = true)
        } finally chrome.close()
    }
  }

  private def drive(chrome: Chrome, throttleAsIphone: Boolean): Unit = {
    val sequence = Seq("/", "/kina", "/", "/kina", "/")
    sequence.foreach { p =>
      val url = Path + p
      chrome.openPage(url) { page =>
        if (throttleAsIphone) {
          // iPhone 14 UA + 390x844 viewport are roughly Mobile Safari's
          // shape; CPU throttle 4x is a low-end iPhone heuristic. None of
          // these is "real iPhone" — the GPU + WebKit JS engine on a real
          // device behave differently — but the numbers diverge enough
          // from headless-desktop that we can spot the pattern.
          val iphoneUa = "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) " +
                         "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1"
          // `Emulation.setUserAgentOverride` rewrites both the outgoing
          // request header AND `navigator.userAgent` in JS — needed so
          // the beacon's `ua` field reads iPhone-Safari instead of the
          // host Chrome's headless UA.
          page.send("Emulation.setUserAgentOverride", Json.obj("userAgent" -> iphoneUa))
          page.send("Emulation.setDeviceMetricsOverride", Json.obj(
            "width" -> 390, "height" -> 844, "deviceScaleFactor" -> 3.0, "mobile" -> true
          ))
          page.send("Emulation.setCPUThrottlingRate", Json.obj("rate" -> 4.0))
        }
        // Click the OTHER nav-tab so `_sharedJs`'s click handler stashes
        // the click timestamp in sessionStorage — that's what the
        // destination page's beacon picks up to compute `tap2nav`.
        val nextHref = if (p == "/") "/kina" else "/"
        page.eval(s"document.querySelector('a.nav-tab[href=\"$nextHref\"]')?.click()")
        // Wait briefly for pagehide → sendBeacon → server. sendBeacon is
        // fire-and-forget; the browser usually flushes within ~200 ms,
        // but the tab is about to close so we give it more room.
        TimeUnit.MILLISECONDS.sleep(600)
      }
    }
    // Final settle so the last batch of beacons makes it to Fly logs.
    TimeUnit.SECONDS.sleep(3)
  }
}
