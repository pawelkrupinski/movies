package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class AsciiUrlSpec extends AnyFlatSpec with Matchers {

  /** A poster link exactly as one Poznań cinema's WordPress serves it. */
  private val Scraped = "http://kinobulgarska19.pl/wp-content/uploads/2026/05/Milcząca-przyjaciółka_plakat-PL_LQ.jpg"

  "encode" should "percent-encode the UTF-8 bytes of every character the URL grammar forbids" in {
    AsciiUrl.encode(Scraped) shouldBe
      "http://kinobulgarska19.pl/wp-content/uploads/2026/05/Milcz%C4%85ca-przyjaci%C3%B3%C5%82ka_plakat-PL_LQ.jpg"
    AsciiUrl.encode("https://example.test/a b.jpg") shouldBe "https://example.test/a%20b.jpg"
  }

  /**
   * What "a strict parser" means here. Swift's `URL` rejects any non-ASCII
   * character; `java.net.URI` rejects a space but, unlike Swift, lets a Unicode
   * letter through — so the JVM-side stand-in is BOTH: pure ASCII, and a URI.
   * The raw scraped link fails the first, the space case fails the second.
   */
  it should "turn a URL a strict parser rejects into one it accepts" in {
    Scraped.forall(_ < 0x80) shouldBe false
    an[IllegalArgumentException] should be thrownBy URI.create("https://example.test/a b.jpg")

    val encoded = AsciiUrl.encode(Scraped)
    encoded.forall(_ < 0x80) shouldBe true
    URI.create(encoded).getPath should endWith("_plakat-PL_LQ.jpg")
    URI.create(AsciiUrl.encode("https://example.test/a b.jpg")).getPath shouldBe "/a b.jpg"
  }

  it should "leave a URL that was already valid byte-for-byte alone, escapes included" in {
    val valid = Seq(
      "https://image.tmdb.org/t/p/original/incepcja.jpg",
      "https://example.test/a%20b.jpg?x=1&y=%C4%85#frag",
      "https://www.youtube.com/embed/abc123?rel=0",
    )
    valid.foreach(url => AsciiUrl.encode(url) shouldBe url)
  }

  it should "be idempotent" in {
    AsciiUrl.encode(AsciiUrl.encode(Scraped)) shouldBe AsciiUrl.encode(Scraped)
  }
}
