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
class OgCardPostersReadySpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private var chrome: Option[Chrome] = None

  override def beforeAll(): Unit = chrome = Chrome.tryStart()
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
    page.waitFor(OgCardGenerator.PostersReadyJs, timeoutMs = 3000, pollMs = 50)
    page.evalBool(s"!!(${OgCardGenerator.PostersReadyJs})") shouldBe true
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
