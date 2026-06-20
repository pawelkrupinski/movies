package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

class CityOgCardServiceSpec extends AnyFlatSpec with Matchers {

  // A real JPEG (the format posterForCard requests) so ImageIO.read decodes it.
  private val jpeg: Array[Byte] = {
    val p = new BufferedImage(400, 600, BufferedImage.TYPE_INT_RGB)
    val g = p.createGraphics(); g.setColor(Color.RED); g.fillRect(0, 0, 400, 600); g.dispose()
    val baos = new ByteArrayOutputStream(); ImageIO.write(p, "jpg", baos); baos.toByteArray
  }

  private class CountingFetch(payload: Array[Byte]) extends PosterFetch {
    val calls = new AtomicInteger(0)
    override def bytes(url: String): Option[Array[Byte]] = { calls.incrementAndGet(); Some(payload) }
  }

  // Fails the first `failFirst` fetches, then serves the payload.
  private class FlakyFetch(payload: Array[Byte], failFirst: Int) extends PosterFetch {
    val calls = new AtomicInteger(0)
    override def bytes(url: String): Option[Array[Byte]] =
      if (calls.incrementAndGet() <= failFirst) None else Some(payload)
  }

  private def dimensions(bytes: Array[Byte]): (Int, Int) = {
    val img = ImageIO.read(new ByteArrayInputStream(bytes))
    (img.getWidth, img.getHeight)
  }

  // Top-right montage cell, where the left→right gradient has faded to clear, so
  // a loaded red poster shows through.
  private def montageRed(bytes: Array[Byte]): Int =
    new Color(ImageIO.read(new ByteArrayInputStream(bytes)).getRGB(1100, 100)).getRed

  "CityOgCardService.card" should "build a 1200×630 poster-montage PNG from the fetched posters" in {
    val fetch = new CountingFetch(jpeg)
    val bytes = new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu",
      Seq("https://cdn/a.jpg", "https://cdn/b.jpg"))
    dimensions(bytes) shouldBe (1200, 630)
    montageRed(bytes) should be > 150
  }

  it should "memoise per city — second call neither re-fetches nor re-renders" in {
    val fetch   = new CountingFetch(jpeg)
    val service = new CityOgCardService(fetch)
    val a = service.card("poznan", "Repertuar kin w Poznaniu", Seq("https://cdn/a.jpg", "https://cdn/b.jpg"))
    val b = service.card("poznan", "Repertuar kin w Poznaniu", Seq("https://cdn/a.jpg", "https://cdn/b.jpg"))
    b should be theSameInstanceAs a
    fetch.calls.get shouldBe 2 // the two distinct candidate posters, fetched once
  }

  it should "degrade to a gradient-only card (still 1200×630) when no poster decodes" in {
    val fetch: PosterFetch = (_: String) => None
    dimensions(new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu",
      Seq("https://cdn/a.jpg"))) shouldBe (1200, 630)
  }

  it should "not cache a poster-less card, so the next share retries the fetch" in {
    // The one candidate's origin + weserv attempts both fail on the first card
    // (gradient only); the next card's origin attempt succeeds. A cached failure
    // would stop the second card re-fetching.
    val fetch   = new FlakyFetch(jpeg, failFirst = 2)
    val service = new CityOgCardService(fetch)
    val first  = service.card("poznan", "Repertuar kin w Poznaniu", Seq("https://cdn/a.jpg"))
    val second = service.card("poznan", "Repertuar kin w Poznaniu", Seq("https://cdn/a.jpg"))
    montageRed(first)  should be < 80  // no poster loaded → dark
    montageRed(second) should be > 150 // retry loaded a poster
  }

  it should "not touch the network when the city has no posters" in {
    val fetch = new CountingFetch(jpeg)
    new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu", Seq.empty)
    fetch.calls.get shouldBe 0
  }
}
