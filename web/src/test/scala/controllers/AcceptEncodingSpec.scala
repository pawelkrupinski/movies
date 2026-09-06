package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Accept-Encoding` parsing for the cached responses.
 *
 *  This replaced `Accept-Encoding contains "gzip"`, which was wrong in a way a
 *  substring match cannot avoid: `gzip;q=0` is a REFUSAL and contains "gzip".
 */
class AcceptEncodingSpec extends AnyFlatSpec with Matchers {

  import AcceptEncoding.acceptsGzip

  private def accepts(header: String) = acceptsGzip(Some(header))

  "acceptsGzip" should "say yes to every real browser and HTTP library header" in {
    accepts("gzip, deflate, br, zstd") shouldBe true   // Chrome
    accepts("gzip, deflate, br")       shouldBe true   // Safari
    accepts("gzip")                    shouldBe true   // OkHttp
    accepts("br, gzip")                shouldBe true
  }

  it should "say no when nothing we can build is offered" in {
    accepts("deflate")     shouldBe false
    accepts("br")          shouldBe false
    accepts("identity")    shouldBe false
    accepts("")            shouldBe false
    acceptsGzip(None)      shouldBe false
  }

  // The bug the old substring check had. `q=0` is the ONLY way a client can say
  // "not this one", and reading it as acceptance sends a body it cannot inflate.
  it should "treat q=0 as the refusal it is, not as the substring it contains" in {
    accepts("gzip;q=0")             shouldBe false
    accepts("gzip;q=0, br")         shouldBe false
    accepts("br;q=0, gzip")         shouldBe true
    accepts("gzip;q=0.1, br;q=0.9") shouldBe true
  }

  // `*` covers what is not named — but a named q=0 still overrides it.
  it should "read a wildcard as covering whatever it did not name" in {
    accepts("*")             shouldBe true
    accepts("gzip;q=0, *")   shouldBe false
    accepts("*;q=0")         shouldBe false
    accepts("*;q=0, gzip")   shouldBe true
  }

  it should "survive the spacing and casing real clients send" in {
    accepts("GZIP, BR")            shouldBe true
    accepts("  gzip ;  q = 0.5 ")  shouldBe true
    accepts("deflate,,gzip")       shouldBe true
  }
}
