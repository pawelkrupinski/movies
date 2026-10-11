package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.util.concurrent.atomic.AtomicInteger

/**
 * `GET /api/picker/:country` — one country's `/` picker fragment, the same markup
 * the landing embeds for its own country, rendered once per process and linked
 * by a URL naming its content version. The conditional-GET mechanics are
 * [[FixedBodySpec]]'s; this pins what is particular to the fragments.
 */
class PickerControllerSpec extends AnyFlatSpec with Matchers {

  private val fragments = PickerFragments.rendered()
  private val controller = new PickerController(stubControllerComponents(), fragments)
  private def get(country: String, v: Option[String] = None, headers: (String, String)*) =
    controller.fragment(country, v)(FakeRequest().withHeaders(headers*))

  "GET /api/picker/:country" should "serve that country's pane, the landing's own markup for it" in {
    val result = get("us")
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/html")
    charset(result) shouldBe Some("utf-8")
    val body = contentAsString(result)
    body shouldBe PickerFragments.render(Country.UnitedStates)
    body should startWith ("""<div class="picker-pane" data-country="us" data-base-url="https://showtimes.cc/us">""")
    Country.UnitedStates.cities.foreach(c => body should include (s"""data-slug="${c.slug}""""))
    body should not include "data-slug=\"poznan\""
    // A fragment, not a page.
    body should not include "<html"
    body should not include "<script"
  }

  it should "serve every deployed country, and 404 one that is not" in {
    Country.switchable.foreach(c => status(get(c.code)) shouldBe OK)
    status(get("xx")) shouldBe NOT_FOUND
    status(get("")) shouldBe NOT_FOUND
  }

  it should "answer 304 to its content ETag" in {
    val etag = header("ETag", get("de")).get
    etag shouldBe fragments.body("de").get.etag
    status(get("de", None, "If-None-Match" -> etag)) shouldBe NOT_MODIFIED
  }

  it should "let the browser keep it for good only under the URL naming its current version" in {
    val versioned = fragments.url("uk")
    versioned should startWith ("/api/picker/uk?v=")
    val v = versioned.split("v=", 2)(1)
    header("Cache-Control", get("uk", Some(v))) shouldBe Some("public, max-age=31536000, immutable")
    // No version, or another deploy's: revalidate, so a pod mid-rollout can never
    // pin its body under a URL that names different bytes.
    header("Cache-Control", get("uk")) shouldBe Some("no-cache")
    header("Cache-Control", get("uk", Some("0000000000000000"))) shouldBe Some("no-cache")
  }

  it should "serve the gzipped copy to a client that takes it" in {
    header("Content-Encoding", get("pl", None, "Accept-Encoding" -> "gzip")) shouldBe Some("gzip")
  }

  "the picker fragments" should "render each country once, however many requests read them" in {
    val renders = new AtomicInteger
    val counted = new PickerFragments(c => { renders.incrementAndGet(); s"<div>${c.code}</div>" })
    val served  = new PickerController(stubControllerComponents(), counted)
    renders.get shouldBe Country.switchable.size
    (1 to 3).foreach { _ =>
      contentAsString(served.fragment("de", None)(FakeRequest())) shouldBe "<div>de</div>"
      counted.pane(Country.Germany).body shouldBe "<div>de</div>"
      counted.url("de")
    }
    renders.get shouldBe Country.switchable.size
  }

  it should "hold no language: the landing's copy of a pane is byte-identical whatever language rendered the page" in {
    val pl = views.html.landing(Country.Poland, isApex = false, fragments)(using testsupport.TestMessages.forLang("pl")).body
    val en = views.html.landing(Country.Poland, isApex = false, fragments)(using testsupport.TestMessages.forLang("en")).body
    val pane = fragments.pane(Country.Poland).body
    pl should include (pane)
    en should include (pane)
    pane should not include "data-i18n"
  }
}
