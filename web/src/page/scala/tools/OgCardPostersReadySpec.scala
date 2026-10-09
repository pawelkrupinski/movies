package tools

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/**
 * Regression for the OG-card generator's poster-decode wait
 * (`OgCardGenerator.PostersReadyJs`). The old generator slept a fixed
 * 900ms after `pickDay('anytime')`, which raced the lazy, proxied poster
 * `<img>` decode and left whichever cities were slow with blank posters on
 * their share card (PR #74 shipped empty cards for Łódź / Trójmiasto /
 * Radom). The fix polls this predicate until the in-viewport posters have
 * decoded (or their `onerror` chain hid them) before screenshotting.
 *
 * Drives a real headless Chrome over CDP against a controlled fixture so the
 * predicate's behaviour is deterministic and network-free. Skips gracefully
 * when Chrome isn't installed (CI images without a browser get cancelled
 * tests, same pattern as `PageJsBehaviourSpec`).
 */
class OgCardPostersReadySpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with SuiteConfiguration {

  private var chrome: Option[Chrome] = None

  override def beforeAll(): Unit = chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
  override def afterAll(): Unit  = chrome.foreach(_.close())

  // A real 1×1 PNG — decodes to naturalWidth 1, so it satisfies the
  // "decoded" half of the predicate once the browser has processed it.
  private val onePxPng =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="

  /** Four poster `<img>`s exercising every branch of the predicate:
   *   - #loaded:  in-viewport, valid src → decodes → ready
   *   - #pending: in-viewport, NO src → naturalWidth stays 0 → NOT ready
   *   - #hidden:  display:none (onerror chain exhausted) → ready (offsetParent null)
   *   - #below:   far below the fold → excluded from the viewport filter
   */
  private val fixtureHtml =
    s"""<!DOCTYPE html><html><head><meta charset="utf-8"></head>
       |<body style="margin:0">
       |<img id="loaded"  data-original-src="a" src="$onePxPng" style="width:100px;height:100px">
       |<img id="pending" data-original-src="b" style="width:100px;height:100px">
       |<img id="hidden"  data-original-src="c" style="display:none">
       |<img id="below"   data-original-src="d" style="position:absolute;top:5000px;width:100px;height:100px">
       |</body></html>""".stripMargin

  private def withFixture(body: CdpPage => Unit): Unit = withHtml(fixtureHtml)(body)

  private def withHtml(html: String)(body: CdpPage => Unit): Unit = chrome match {
    case None => cancel("Chrome not installed — skipping CDP poster-ready spec")
    case Some(c) =>
      val tmp = Files.createTempFile("og-posters-ready-", ".html")
      Files.writeString(tmp, html)
      try c.openPage(tmp.toUri.toString) { page =>
        page.setDesktopViewport(1180, 760)
        body(page)
      } finally Files.deleteIfExists(tmp)
  }

  "PostersReadyJs" should "report NOT ready while an in-viewport poster is undecoded" in withFixture { page =>
    // #pending has no src → naturalWidth 0, still visible → predicate false,
    // regardless of how fast #loaded decodes.
    page.evalBool(s"!!(${OgCardGenerator.PostersReadyJs})") shouldBe false
  }

  it should "report ready once every in-viewport poster has decoded or been hidden" in withFixture { page =>
    // Hide the only undecoded in-viewport poster, mimicking the onerror
    // fallback chain swapping in the "Brak plakatu" placeholder.
    page.eval("document.getElementById('pending').style.display='none'")
    // #loaded still has to finish decoding — exactly what the generator waits
    // on. waitFor throws on timeout, so reaching the assertion means ready.
    page.waitFor(OgCardGenerator.PostersReadyJs, pollMs = 50)
    page.evalBool(s"!!(${OgCardGenerator.PostersReadyJs})") shouldBe true
  }

  "postersPaintedJs" should "resolve true once every in-viewport poster has decoded" in withFixture { page =>
    page.eval("document.getElementById('pending').style.display='none'")
    page.evalBool(OgCardGenerator.postersPaintedJs(5000)) shouldBe true
  }

  // A repertoire stand-in for screenshotCity: `pickDay` defined (so the load
  // guard passes) and the given posters in the viewport.
  private def repertoireHtml(posters: String): String =
    s"""<!DOCTYPE html><html><head><meta charset="utf-8">
       |<script>function pickDay(){}</script></head>
       |<body style="margin:0">$posters</body></html>""".stripMargin

  private def withRepertoireUrl(posters: String)(body: (Chrome, String) => Unit): Unit = chrome match {
    case None => cancel("Chrome not installed — skipping CDP poster-ready spec")
    case Some(c) =>
      val tmp = Files.createTempFile("og-repertoire-", ".html")
      Files.writeString(tmp, repertoireHtml(posters))
      try body(c, tmp.toUri.toString) finally Files.deleteIfExists(tmp)
  }

  // og-alamosa.jpg (PR #216) shipped with blank posters: a poster that never
  // became ready used to be swallowed and the half-loaded page screenshotted
  // anyway. It must fail the attempt instead, so writeCard reopens the page.
  "screenshotCity" should "throw rather than screenshot while an in-viewport poster never loads" in withRepertoireUrl(
    """<img data-original-src="b" style="width:100px;height:100px">"""
  ) { (c, url) =>
    val error = the[RuntimeException] thrownBy OgCardGenerator.screenshotCity(c, url, postersTimeoutMs = 500)
    error.getMessage should include("posters")
  }

  // An SVG poster filling a `.poster-wrap`-coloured (#2a2a3e) box.
  private def posterInBox(fill: String): String =
    s"""<div style="background:#2a2a3e;width:200px;height:296px">""" +
      s"""<img data-original-src="a" decoding="async" style="width:200px;height:296px" """ +
      s"""src="data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10' fill='%23$fill'/></svg>"></div>"""

  it should "screenshot once every in-viewport poster has painted" in withRepertoireUrl(posterInBox("cc2222")) { (c, url) =>
    OgCardGenerator.screenshotCity(c, url, postersTimeoutMs = 5000) should not be empty
  }

  // PR #217's Tarnów / Tauberbischofsheim / Westerland / Wittenberge cards
  // passed every in-page check (loaded, decoded) in under 4s and still shipped
  // empty poster boxes: the pixels hadn't reached the screenshot. A poster
  // whose box in the screenshot is still the box's own background must fail
  // the attempt. (A poster that genuinely IS that colour stands in for one
  // that never painted — the screenshot can't tell them apart either.)
  it should "throw when a poster's box in the screenshot is still the empty background" in withRepertoireUrl(posterInBox("2a2a3e")) { (c, url) =>
    val error = the[RuntimeException] thrownBy OgCardGenerator.screenshotCity(c, url, postersTimeoutMs = 1000)
    error.getMessage should include("blank")
  }

  private def filled(width: Int, height: Int, color: java.awt.Color): java.awt.image.BufferedImage = {
    val image = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_RGB)
    val g = image.createGraphics()
    g.setColor(color); g.fillRect(0, 0, width, height); g.dispose()
    image
  }

  private val boxBackground = new java.awt.Color(0x2a, 0x2a, 0x3e)
  private val box = OgCardGenerator.PosterBox(x = 10, y = 10, width = 50, height = 74, background = boxBackground)

  // On desktop, pickDay('anytime') slides the day track: a copy of the target
  // day's grid is mounted off to the side and the track animates over to it.
  // Every poster check that ran during the slide saw the wrong posters (one
  // clipped Hoppers at the screen edge on Renfrewshire, 2026-10-09), so the
  // blank cards of PRs #216 and #217 passed them all. Under
  // prefers-reduced-motion the page commits the day in place (shared.js
  // runSlide), so screenshotCity must emulate it. This fixture's pickDay
  // stands in for the slide: a blank poster unless motion is reduced.
  it should "change the day under prefers-reduced-motion, so pickDay never slides" in withRepertoireUrl("") { (c, url) =>
    val fixture = java.nio.file.Paths.get(java.net.URI.create(url))
    Files.writeString(fixture, Files.readString(fixture).replace(
      "function pickDay(){}",
      "function pickDay(){var reduced=matchMedia('(prefers-reduced-motion: reduce)').matches;" +
        "document.body.innerHTML=" + play.api.libs.json.Json.stringify(play.api.libs.json.JsString(posterInBox("cc2222"))) +
        ".replace('cc2222',reduced?'cc2222':'2a2a3e');}"
    ))
    OgCardGenerator.screenshotCity(c, url, postersTimeoutMs = 1000) should not be empty
  }

  "unpaintedPosters" should "flag a poster box still showing its background" in {
    OgCardGenerator.unpaintedPosters(filled(200, 200, boxBackground), Seq(box), deviceScale = 2) shouldBe Seq(box)
  }

  it should "pass a fully painted poster box" in {
    OgCardGenerator.unpaintedPosters(filled(200, 200, java.awt.Color.ORANGE), Seq(box), deviceScale = 2) shouldBe empty
  }

  it should "flag a poster painted only in a top band (the half-painted shape)" in {
    val image = filled(200, 200, boxBackground)
    val g = image.createGraphics()
    g.setColor(java.awt.Color.ORANGE); g.fillRect(0, 0, 200, 2 * (10 + 15)); g.dispose()
    OgCardGenerator.unpaintedPosters(image, Seq(box), deviceScale = 2) shouldBe Seq(box)
  }

  // The load guard: a page that isn't the repertoire (Chrome's offline dino
  // page, a 5xx body) has no `pickDay`, so the generator skips it instead of
  // screenshotting an error page into a blank card.
  "RepertoireLoadedJs" should "be false on a page without the repertoire JS" in withFixture { page =>
    page.evalBool(s"!!(${OgCardGenerator.RepertoireLoadedJs})") shouldBe false
  }

  it should "be true once the repertoire JS has defined pickDay" in withFixture { page =>
    page.eval("window.pickDay = function(){}")
    page.evalBool(s"!!(${OgCardGenerator.RepertoireLoadedJs})") shouldBe true
  }

  // When RepertoireLoadedJs never becomes true, loadFailureDiagnostic is what
  // tells apart a DNS/connection failure from a real HTTP error page from a
  // same-origin-but-broken render — all three looked identical as the bare
  // "no pickDay" message this replaced.
  "loadFailureDiagnostic" should "report the page's actual title and body text" in withHtml(
    "<!DOCTYPE html><html><head><title>Access Denied</title></head><body>blocked by WAF</body></html>"
  ) { page =>
    val diagnostic = OgCardGenerator.loadFailureDiagnostic(page)
    diagnostic should include("title=Access Denied")
    diagnostic should include("blocked by WAF")
  }

  it should "not throw when the page has no body at all" in withHtml(
    "<!DOCTYPE html><html><head></head></html>"
  ) { page =>
    noException should be thrownBy OgCardGenerator.loadFailureDiagnostic(page)
  }

  // Split cities (London) open the "Choose your areas" modal over the grid on
  // first load; DismissAreaPickerJs clicks its "Show listings" button so the
  // card captures posters, not the modal. The button's real apply/close logic
  // lives in shared.js; here a stand-in onclick that removes the overlay proves
  // the generator selects and clicks the right element.
  private val areaPickerHtml =
    """<!DOCTYPE html><html><head><meta charset="utf-8"></head>
      |<body style="margin:0">
      |<div id="area-picker-overlay" style="position:fixed;inset:0">
      |  <button onclick="document.getElementById('area-picker-overlay').remove()">Show listings</button>
      |</div>
      |</body></html>""".stripMargin

  "DismissAreaPickerJs" should "click the picker's button and remove the overlay" in withHtml(areaPickerHtml) { page =>
    page.evalBool("!!document.getElementById('area-picker-overlay')") shouldBe true
    page.eval(OgCardGenerator.DismissAreaPickerJs)
    page.evalBool("!!document.getElementById('area-picker-overlay')") shouldBe false
  }

  it should "be a harmless no-op on a flat page with no picker" in withFixture { page =>
    // The default poster fixture has no #area-picker-overlay — the guarded click
    // must not throw and must leave the page's posters in place.
    page.eval(OgCardGenerator.DismissAreaPickerJs)
    page.evalBool("!!document.getElementById('loaded')") shouldBe true
  }
}
