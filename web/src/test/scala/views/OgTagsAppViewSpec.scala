package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The canonical link is built from the request's forwarded host (and, for the
 *  listing's `pageUrl`, its raw query), so it is request input and has to be
 *  escaped like every other attribute value Twirl writes. */
class OgTagsAppViewSpec extends AnyFlatSpec with Matchers {

  private def render(canonicalUrl: String = "", pageUrl: String = "") =
    views.html._ogTagsApp(models.Country.Poland)("Title", "Description", pageUrl = pageUrl, canonicalUrl = canonicalUrl).body

  "the share-tag partial" should "escape a hostile canonical URL instead of letting it close the attribute" in {
    val html = render(canonicalUrl = "https://evil.example\"><script>alert(1)</script>/warszawa/")
    html should not include "<script>alert(1)</script>"
    html should include ("""<link rel="canonical" href="https://evil.example&quot;&gt;&lt;script&gt;alert(1)&lt;/script&gt;/warszawa/">""")
  }

  it should "escape the page URL it falls back to as the canonical link" in {
    val html = render(pageUrl = "https://kinowo.net/warszawa/?a=\"><img src=x onerror=alert(1)>")
    html should not include "<img src=x"
  }

  it should "leave an ordinary canonical URL untouched" in {
    render(canonicalUrl = "https://kinowo.net/warszawa/") should include ("""<link rel="canonical" href="https://kinowo.net/warszawa/">""")
  }
}
