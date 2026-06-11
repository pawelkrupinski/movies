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

  private class CountingFetch(payload: Array[Byte]) extends GetOnlyHttpFetch {
    val calls = new AtomicInteger(0)
    override def get(url: String): String = ""
    override def getBytes(url: String): Array[Byte] = { calls.incrementAndGet(); payload }
  }

  private def dimensions(bytes: Array[Byte]): (Int, Int) = {
    val img = ImageIO.read(new ByteArrayInputStream(bytes))
    (img.getWidth, img.getHeight)
  }

  "OgCardService.card" should "build a 1200×630 PNG from the fetched poster" in {
    val fetch = new CountingFetch(jpeg)
    val bytes = new OgCardService(fetch).card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some("https://cdn/x.jpg"))
    dimensions(bytes) shouldBe (1200, 630)
    fetch.calls.get shouldBe 1
  }

  it should "memoise per identical inputs — second call neither re-fetches nor re-renders" in {
    val fetch = new CountingFetch(jpeg)
    val svc   = new OgCardService(fetch)
    val a = svc.card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some("https://cdn/x.jpg"))
    val b = svc.card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some("https://cdn/x.jpg"))
    b should be theSameInstanceAs a
    fetch.calls.get shouldBe 1
  }

  it should "re-render when an input changes (e.g. a refreshed rating)" in {
    val fetch = new CountingFetch(jpeg)
    val svc   = new OgCardService(fetch)
    svc.card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some("https://cdn/x.jpg"))
    svc.card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.9"), Some("https://cdn/x.jpg"))
    fetch.calls.get shouldBe 2
  }

  it should "degrade to a text-only card (still 1200×630) when the poster fetch throws" in {
    val fetch = new GetOnlyHttpFetch {
      override def get(url: String): String = ""
      override def getBytes(url: String): Array[Byte] = throw new RuntimeException("boom")
    }
    dimensions(new OgCardService(fetch).card("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some("https://cdn/x.jpg"))) shouldBe (1200, 630)
  }

  it should "not touch the network when the film has no poster" in {
    val fetch = new CountingFetch(jpeg)
    new OgCardService(fetch).card("Film", "2026 · Dramat", Seq("Filmweb 7.1"), None)
    fetch.calls.get shouldBe 0
  }
}
