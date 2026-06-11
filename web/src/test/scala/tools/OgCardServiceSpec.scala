package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

class OgCardServiceSpec extends AnyFlatSpec with Matchers {

  // A real JPEG (the format posterForCard requests) so ImageIO.read decodes it
  // — a hand-rolled byte blob would exercise a different path than production.
  private val jpeg: Array[Byte] = {
    val p = new BufferedImage(400, 600, BufferedImage.TYPE_INT_RGB)
    val g = p.createGraphics(); g.setColor(Color.RED); g.fillRect(0, 0, 400, 600); g.dispose()
    val baos = new ByteArrayOutputStream(); ImageIO.write(p, "jpg", baos); baos.toByteArray
  }

  private class CountingFetch(payload: Array[Byte]) extends PosterFetch {
    val calls = new AtomicInteger(0)
    override def bytes(url: String): Option[Array[Byte]] = { calls.incrementAndGet(); Some(payload) }
  }

  // Fails (returns None for) its first `failFirst` fetches, then serves the
  // payload — models a poster origin that's cold/slow on first render but warm
  // on a retry.
  private class FlakyFetch(payload: Array[Byte], failFirst: Int) extends PosterFetch {
    val calls = new AtomicInteger(0)
    override def bytes(url: String): Option[Array[Byte]] =
      if (calls.incrementAndGet() <= failFirst) None else Some(payload)
  }

  private def dimensions(bytes: Array[Byte]): (Int, Int) = {
    val img = ImageIO.read(new ByteArrayInputStream(bytes))
    (img.getWidth, img.getHeight)
  }

  // Centre of the poster slot: the bg there is dark (~14), a loaded poster is red.
  private def posterRed(bytes: Array[Byte]): Int =
    new Color(ImageIO.read(new ByteArrayInputStream(bytes)).getRGB(120, 315)).getRed

  "OgCardService.card" should "build a 1200×630 PNG from the fetched poster" in {
    val fetch = new CountingFetch(jpeg)
    val bytes = new OgCardService(fetch).card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    dimensions(bytes) shouldBe (1200, 630)
    fetch.calls.get shouldBe 1
  }

  it should "memoise per identical inputs — second call neither re-fetches nor re-renders" in {
    val fetch = new CountingFetch(jpeg)
    val svc   = new OgCardService(fetch)
    val a = svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    val b = svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    b should be theSameInstanceAs a
    fetch.calls.get shouldBe 1
  }

  it should "re-render when an input changes (e.g. a refreshed rating)" in {
    val fetch = new CountingFetch(jpeg)
    val svc   = new OgCardService(fetch)
    svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.9), None, None, None), Some("https://cdn/x.jpg"))
    fetch.calls.get shouldBe 2
  }

  it should "degrade to a text-only card (still 1200×630) when the poster fetch fails" in {
    val fetch: PosterFetch = (_: String) => None
    dimensions(new OgCardService(fetch).card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))) shouldBe (1200, 630)
  }

  it should "fall back to the weserv proxy when the origin URL itself fails to decode" in {
    // First fetch (origin) returns None, second (weserv) succeeds -> poster loads.
    val fetch = new FlakyFetch(jpeg, failFirst = 1)
    posterRed(new OgCardService(fetch).card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))) should be > 150
    fetch.calls.get shouldBe 2
  }

  it should "not cache a poster-less card, so the next share retries the fetch" in {
    // Both attempts (origin + weserv) fail on the first card; the third fetch
    // (the second card's origin attempt) succeeds. If the failure were cached,
    // the second card would be served from the cache and never re-fetch.
    val fetch  = new FlakyFetch(jpeg, failFirst = 2)
    val svc    = new OgCardService(fetch)
    val first  = svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    val second = svc.card("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some("https://cdn/x.jpg"))
    posterRed(first)  should be < 60   // text-only fallback (dark slot)
    posterRed(second) should be > 150  // retry loaded the poster
    fetch.calls.get   should be > 2    // proves the second card re-fetched, not a cache hit
  }

  it should "not touch the network when the film has no poster" in {
    val fetch = new CountingFetch(jpeg)
    new OgCardService(fetch).card("Film", "2026 · Dramat", OgCardRenderer.ratingBadges(None, None, None, Some(7.1)), None)
    fetch.calls.get shouldBe 0
  }
}
