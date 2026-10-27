package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrailerEmbedSpec extends AnyFlatSpec with Matchers {

  private val YouTube = Some("https://www.youtube.com/embed/dQw4w9WgXcQ")
  private val Vimeo   = Some("https://player.vimeo.com/video/76979871")

  /** What the view embeds for each trailer link, and why. None: the view falls back to no embed -- a YouTube channel
   *  page, a search-results URL or a raw mp4 from an unfamiliar CDN would all break the iframe, so recognising "not
   *  embeddable" is how the page keeps from rendering a broken frame. */
  private val Cases: Seq[(String, String, Option[String])] = Seq(
    ("translate a youtube.com/watch URL to /embed/", "https://www.youtube.com/watch?v=dQw4w9WgXcQ", YouTube),
    ("translate a youtu.be short URL to /embed/", "https://youtu.be/dQw4w9WgXcQ", YouTube),
    ("pass an /embed/ URL through canonicalised", "https://www.youtube.com/embed/dQw4w9WgXcQ", YouTube),
    ("handle /shorts/ URLs as videos", "https://www.youtube.com/shorts/dQw4w9WgXcQ", YouTube),
    ("strip query parameters on /watch URLs (timestamps, list)", "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42s&list=PL123", YouTube),
    ("accept m.youtube.com as a mobile alias", "https://m.youtube.com/watch?v=dQw4w9WgXcQ", YouTube),
    ("convert vimeo.com/<id> to player.vimeo.com/video/<id>", "https://vimeo.com/76979871", Vimeo),
    ("convert player.vimeo.com/video/<id> verbatim", "https://player.vimeo.com/video/76979871", Vimeo),
    ("reject a youtube.com channel URL", "https://www.youtube.com/channel/UCIMTXiHkou3DJ7ov4SF-wcQ", None),
    ("reject a /watch URL with no v= parameter", "https://www.youtube.com/watch", None),
    ("reject a URL with a malformed YouTube id (wrong length)", "https://www.youtube.com/watch?v=tooshort", None),
    ("reject an unknown host", "https://example.com/video/12345", None),
    ("reject vimeo.com/<non-numeric>", "https://vimeo.com/staffpicks", None),
    ("reject garbage input", "not a url at all", None),
    ("reject an empty link", "", None))

  Cases.foreach { case (behaviour, link, embed) =>
    "TrailerEmbed.embedUrlFor" should behaviour in {
      TrailerEmbed.embedUrlFor(link) shouldBe embed
    }
  }
}
