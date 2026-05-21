package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PosterProxySpec extends AnyFlatSpec with Matchers {

  // The whole point of this proxy is to force HTTPS on mixed-content
  // (HTTP-only) origins like kinobulgarska19.pl that browsers were
  // silently blocking on the HTTPS kinowo.fly.dev page.
  "PosterProxy.proxy" should "force the request URL to HTTPS regardless of origin scheme" in {
    val http  = PosterProxy.proxy("http://kinobulgarska19.pl/wp-content/uploads/2026/04/x.jpg")
    val https = PosterProxy.proxy("https://www.multikino.pl/-/media/multikino/images/x.jpg")
    http  should startWith ("https://images.weserv.nl/")
    https should startWith ("https://images.weserv.nl/")
  }

  it should "URL-encode the origin URL so cinema paths with query strings round-trip cleanly" in {
    val proxied = PosterProxy.proxy("https://www.multikino.pl/path/poster.jpg?rev=abc&v=1")
    // The original `?rev=abc&v=1` query string must be encoded as part
    // of the `url=` value, NOT bleed out into the weserv URL's own
    // query (which would make weserv interpret `&v=1` as one of its
    // own params and either ignore it or break the cache key).
    proxied should include ("url=www.multikino.pl%2Fpath%2Fposter.jpg%3Frev%3Dabc%26v%3D1")
  }

  it should "request a 480-px wide webp" in {
    val proxied = PosterProxy.proxy("https://example.com/poster.png")
    proxied should include ("&w=480")
    proxied should include ("&output=webp")
  }

  // Defensive corner case: SourceData.posterUrl is Some("") on a row
  // whose cinema scrape recorded the field as present-but-empty.
  // Returning the empty string here means the template still emits an
  // <img src=""> which triggers the `onerror` fallback to .no-poster
  // rather than firing a doomed proxy request.
  it should "return the input untouched when given an empty string" in {
    PosterProxy.proxy("") shouldBe ""
  }
}
