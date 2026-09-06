package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Accept-Encoding` negotiation for the cached responses.
 *
 *  This replaced `Accept-Encoding contains "gzip"`. That check could not say `br`
 *  at all — fine while Cloudflare was recompressing our gzip to brotli on the way
 *  out, and not fine once `Cache-Control: no-transform` stopped it doing that, +26%
 *  on a full fetch of `/uk/manchester/`. It was also wrong in a way a substring
 *  match cannot avoid: `gzip;q=0` is a REFUSAL and contains "gzip".
 */
class ContentEncodingSpec extends AnyFlatSpec with Matchers {

  import ContentEncoding._

  private def best(header: String) = negotiate(Some(header))

  "negotiation" should "prefer brotli when the client expresses no preference between them" in {
    best("gzip, deflate, br")       shouldBe Some(Brotli)
    best("gzip, deflate, br, zstd") shouldBe Some(Brotli) // what Chrome sends
    best("br, gzip")                shouldBe Some(Brotli)
  }

  it should "fall back to gzip for a client that cannot take brotli" in {
    best("gzip, deflate") shouldBe Some(Gzip)
    best("gzip")          shouldBe Some(Gzip)
  }

  it should "send uncompressed when nothing we can build is offered" in {
    best("deflate")      shouldBe None
    best("zstd")         shouldBe None
    best("")             shouldBe None
    negotiate(None)      shouldBe None
  }

  // The bug the old substring check had. `q=0` is the ONLY way a client can say
  // "not this one", and reading it as acceptance sends a body it cannot inflate.
  it should "treat q=0 as the refusal it is, not as the substring it contains" in {
    best("gzip;q=0")            shouldBe None
    best("br;q=0, gzip")        shouldBe Some(Gzip)
    best("gzip;q=0, br")        shouldBe Some(Brotli)
    best("br;q=0, gzip;q=0")    shouldBe None
  }

  it should "honour a client that weights gzip above brotli" in {
    best("br;q=0.1, gzip;q=0.9")  shouldBe Some(Gzip)
    best("br;q=0.9, gzip;q=0.1")  shouldBe Some(Brotli)
    best("gzip;q=1.0, br;q=0.5")  shouldBe Some(Gzip)
  }

  // `*` covers what is not named — but a named q=0 still overrides it, which is how
  // a client says "anything except brotli".
  it should "read a wildcard as covering whatever it did not name" in {
    best("*")            shouldBe Some(Brotli)
    best("br;q=0, *")    shouldBe Some(Gzip)
    best("*;q=0")        shouldBe None
    best("*;q=0, gzip")  shouldBe Some(Gzip)
  }

  it should "survive the spacing and casing real clients send" in {
    best("GZIP, BR")            shouldBe Some(Brotli)
    best("  gzip ;  q = 0.5 ")  shouldBe Some(Gzip)
    best("gzip,,br")            shouldBe Some(Brotli)
    best("identity")            shouldBe None
  }
}
