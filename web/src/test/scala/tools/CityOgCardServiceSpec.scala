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

  // Serves the payload only for URLs whose host the Fly IP can reach — models
  // the prod block where a film's primary (Multikino) poster 403s but a cinema
  // fallback origin doesn't.
  private class HostGatedFetch(payload: Array[Byte], reachable: String) extends PosterFetch {
    override def bytes(url: String): Option[Array[Byte]] = if (url.contains(reachable)) Some(payload) else None
  }

  private def dimensions(bytes: Array[Byte]): (Int, Int) = {
    val img = ImageIO.read(new ByteArrayInputStream(bytes))
    (img.getWidth, img.getHeight)
  }

  // The 5th column's poster slot (top-right) — clear of the brand overlay's
  // opaque gradient, so a loaded red poster shows; a missing one is dark.
  private def posterRed(bytes: Array[Byte]): Int =
    new Color(ImageIO.read(new ByteArrayInputStream(bytes)).getRGB(1130, 80)).getRed

  private def film(poster: String): CityCardFilm =
    CityCardFilm("Incepcja", Seq("2h 28min", "2010"),
      OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Seq(poster),
      "Sobota 20 czerwca", Seq("Multikino" -> Seq("18:30")))

  // Five distinct-poster films — enough to fill the grid so the right column
  // (the one `posterRed` samples) has a card.
  private val fiveFilms: Seq[CityCardFilm] = Seq("a", "b", "c", "d", "e").map(s => film(s"https://cdn/$s.jpg"))

  "CityOgCardService.card" should "build a 1200×630 page-like PNG from the films' posters" in {
    val fetch = new CountingFetch(jpeg)
    val bytes = new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu", fiveFilms)
    dimensions(bytes) shouldBe (1200, 630)
    posterRed(bytes) should be > 150
  }

  it should "memoise per city — second call neither re-fetches nor re-renders" in {
    val fetch   = new CountingFetch(jpeg)
    val service = new CityOgCardService(fetch)
    val a = service.card("poznan", "Repertuar kin w Poznaniu", fiveFilms)
    val b = service.card("poznan", "Repertuar kin w Poznaniu", fiveFilms)
    b should be theSameInstanceAs a
    fetch.calls.get shouldBe 5 // one fetch per film poster, then served from cache
  }

  it should "degrade to a poster-less card (still 1200×630) when no poster decodes" in {
    val fetch: PosterFetch = (_: String) => None
    dimensions(new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu", fiveFilms)) shouldBe (1200, 630)
  }

  it should "not cache a poster-less card, so the next share retries the fetch" in {
    // Every poster attempt fails on the first card (5 films × origin+weserv = 10);
    // the next card's fetches succeed. A cached failure would stop the re-fetch.
    val fetch   = new FlakyFetch(jpeg, failFirst = 10)
    val service = new CityOgCardService(fetch)
    val first  = service.card("poznan", "Repertuar kin w Poznaniu", fiveFilms)
    val second = service.card("poznan", "Repertuar kin w Poznaniu", fiveFilms)
    posterRed(first)  should be < 80  // no poster loaded → dark slot
    posterRed(second) should be > 150 // retry loaded the posters
  }

  it should "not touch the network when the city has no films" in {
    val fetch = new CountingFetch(jpeg)
    new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu", Seq.empty)
    fetch.calls.get shouldBe 0
  }

  it should "walk past a blocked primary poster to a reachable cinema fallback" in {
    // Each film's primary is a Multikino origin the Fly IP can't reach; the
    // fallback origin decodes. Without the fallbacks the slots render empty.
    val fetch = new HostGatedFetch(jpeg, reachable = "cinema-city.pl")
    val films = Seq.fill(5)(CityCardFilm("Incepcja", Seq("2010"),
      OgCardRenderer.ratingBadges(Some(8.8), None, None, None),
      Seq("https://www.multikino.pl/x.jpg", "https://www.cinema-city.pl/p.jpg"),
      "Sobota 20 czerwca", Seq("Multikino" -> Seq("18:30"))))
    posterRed(new CityOgCardService(fetch).card("poznan", "Repertuar kin w Poznaniu", films)) should be > 150
  }
}
