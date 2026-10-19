package tools

import models.{City, Country, London, Poznan}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

class CityCardRendererSpec extends AnyFlatSpec with Matchers {

  private val Madrid: City = Country.Spain.bySlug("madrid")
  private val Berlin: City = Country.Germany.bySlug("berlin")

  private def solid(colour: Color): BufferedImage = {
    val p = new BufferedImage(210, 315, BufferedImage.TYPE_INT_RGB)
    val g = p.createGraphics()
    g.setColor(colour); g.fillRect(0, 0, p.getWidth, p.getHeight); g.dispose()
    p
  }

  private def brandOf(city: City) = CityCardRenderer.Brand(city.country.brandName, city.country.shareHost, city.country.filmwebEnabled)

  private def rgb(img: BufferedImage, x: Int, y: Int): Color = new Color(img.getRGB(x, y))

  private def near(c: Color, target: Color, tol: Int) =
    math.abs(c.getRed - target.getRed) <= tol && math.abs(c.getGreen - target.getGreen) <= tol && math.abs(c.getBlue - target.getBlue) <= tol

  /** The pixels of the text column, where the heading is drawn. */
  private def textColumn(img: BufferedImage): Array[Int] = img.getRGB(80, 0, 560, img.getHeight, null, 0, 560)

  "A city card" should "be a 1200×630 JPEG that decodes" in {
    val bytes = OgCardRenderer.encodeCard(CityCardRenderer.render(Poznan.heading, brandOf(Poznan), Seq(solid(Color.RED))))
    (bytes(0) & 0xff, bytes(1) & 0xff) shouldBe (0xff, 0xd8)
    val decoded = ImageIO.read(new ByteArrayInputStream(bytes))
    (decoded.getWidth, decoded.getHeight) shouldBe (1200, 630)
  }

  it should "draw the city's first film at the top right, clear of the gradient, and darken the left" in {
    val card = CityCardRenderer.render(Poznan.heading, brandOf(Poznan), Seq(solid(Color.RED), solid(Color.GREEN)))
    val (x0, y0) = CityCardRenderer.tileAt(0)
    val (x1, y1) = CityCardRenderer.tileAt(1)
    near(rgb(card, x0 + CityCardRenderer.TileWidth - 20, y0 + 150), Color.RED, 40) shouldBe true
    near(rgb(card, x1 + CityCardRenderer.TileWidth - 20, y1 + 150), Color.GREEN, 40) shouldBe true
    (y1 > y0, x1 == x0) shouldBe (true, true)
    // The left edge is the gradient's near-opaque end over the background.
    near(rgb(card, 5, 5), new Color(0x0d, 0x0d, 0x22), 6) shouldBe true
  }

  it should "draw at most PosterCount posters and leave the rest of the card to the background" in {
    val card = CityCardRenderer.render(London.heading, brandOf(London), Seq.fill(30)(solid(Color.BLUE)))
    CityCardRenderer.PosterCount shouldBe 12
    val (x, y) = CityCardRenderer.tileAt(CityCardRenderer.PosterCount - 1)
    x should be < 0 // the last column overflows the left edge, under the darkest gradient
    val empty = CityCardRenderer.render(London.heading, brandOf(London), Nil)
    near(rgb(empty, 1150, 300), new Color(0x0d, 0x0d, 0x22), 6) shouldBe true
    near(rgb(card, 1150, 300), Color.BLUE, 40) shouldBe true
    y should be >= 0
  }

  it should "draw the heading it is given, so each language's heading is a different picture" in {
    val cards = Seq(Poznan, London, Madrid, Berlin).map(city => textColumn(CityCardRenderer.render(city.heading, brandOf(city), Nil)).toSeq)
    cards.distinct should have size 4
  }

  it should "show the Filmweb pill only for a country that has Filmweb" in {
    CityCardRenderer.samplePills(Country.Poland.filmwebEnabled).map(_.segs.head.text) should contain ("FW")
    CityCardRenderer.samplePills(Country.UnitedKingdom.filmwebEnabled).map(_.segs.head.text) should not contain ("FW")
  }

  "City.heading" should "read in the city's own language" in {
    Poznan.heading shouldBe "Repertuar kin w Poznaniu"
    London.heading shouldBe "Cinema listings in London"
    Madrid.heading shouldBe "Cartelera de cine en Madrid"
    Berlin.heading shouldBe "Kinoprogramm in Berlin"
  }
}
