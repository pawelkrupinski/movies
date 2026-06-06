package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrailerEmbedSpec extends AnyFlatSpec with Matchers {

  "TrailerEmbed.embedUrlFor" should "translate a youtube.com/watch URL to /embed/" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/watch?v=dQw4w9WgXcQ") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "translate a youtu.be short URL to /embed/" in {
    TrailerEmbed.embedUrlFor("https://youtu.be/dQw4w9WgXcQ") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "pass an /embed/ URL through canonicalised" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/embed/dQw4w9WgXcQ") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "handle /shorts/ URLs as videos" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/shorts/dQw4w9WgXcQ") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "strip query params on /watch URLs (timestamps, list)" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42s&list=PL123") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "accept m.youtube.com as a mobile alias" in {
    TrailerEmbed.embedUrlFor("https://m.youtube.com/watch?v=dQw4w9WgXcQ") shouldBe
      Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  }

  it should "convert vimeo.com/<id> to player.vimeo.com/video/<id>" in {
    TrailerEmbed.embedUrlFor("https://vimeo.com/76979871") shouldBe
      Some("https://player.vimeo.com/video/76979871")
  }

  it should "convert player.vimeo.com/video/<id> verbatim" in {
    TrailerEmbed.embedUrlFor("https://player.vimeo.com/video/76979871") shouldBe
      Some("https://player.vimeo.com/video/76979871")
  }

  // Negative cases — these MUST return None so the view falls back to no embed.
  // A YouTube channel page, a search-results URL, and a raw mp4 from an
  // unfamiliar CDN would all break the iframe; recognising "not embeddable"
  // is how we keep the page from rendering a broken frame.

  it should "reject a youtube.com channel URL" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/channel/UCIMTXiHkou3DJ7ov4SF-wcQ") shouldBe None
  }

  it should "reject a /watch URL with no v= param" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/watch") shouldBe None
  }

  it should "reject a URL with a malformed YouTube id (wrong length)" in {
    TrailerEmbed.embedUrlFor("https://www.youtube.com/watch?v=tooshort") shouldBe None
  }

  it should "reject an unknown host" in {
    TrailerEmbed.embedUrlFor("https://example.com/video/12345") shouldBe None
  }

  it should "reject vimeo.com/<non-numeric>" in {
    TrailerEmbed.embedUrlFor("https://vimeo.com/staffpicks") shouldBe None
  }

  it should "reject garbage input" in {
    TrailerEmbed.embedUrlFor("not a url at all") shouldBe None
    TrailerEmbed.embedUrlFor("") shouldBe None
  }
}
