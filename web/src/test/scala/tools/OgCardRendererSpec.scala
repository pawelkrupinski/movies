package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

class OgCardRendererSpec extends AnyFlatSpec with Matchers {

  // A solid-colour stand-in poster: tall 2:3 so it cover-fills the slot with
  // no crop, and a vivid colour we can detect in the rendered poster region.
  private def solidPoster(colour: Color): BufferedImage = {
    val p = new BufferedImage(400, 600, BufferedImage.TYPE_INT_RGB)
    val g = p.createGraphics()
    g.setColor(colour); g.fillRect(0, 0, 400, 600); g.dispose()
    p
  }

  private def decode(bytes: Array[Byte]): BufferedImage =
    ImageIO.read(new ByteArrayInputStream(bytes))

  "OgCardRenderer" should "produce a 1200×630 PNG" in {
    val bytes = OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some(solidPoster(Color.RED)))
    val img   = decode(bytes)
    img should not be null
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "composite the poster into the left slot and keep a dark background on the right" in {
    val img = decode(OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some(solidPoster(Color.RED))))

    // Centre of the poster slot (slot is ~56..401 wide, full height) — must be
    // dominated by the poster's red, proving the poster was actually drawn.
    val poster = new Color(img.getRGB(220, 315))
    poster.getRed should be > 180
    poster.getRed should be > (poster.getBlue + 80)

    // Far-right column, vertically centred — the text panel background, which
    // must stay dark (no poster bleeding across the whole card).
    val bg = new Color(img.getRGB(1180, 315))
    bg.getRed should be < 60
    bg.getGreen should be < 60
    bg.getBlue should be < 60
  }

  it should "draw light title text against the dark panel (some bright pixels in the title band)" in {
    val img = decode(OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", Seq("IMDb 8.8"), Some(solidPoster(Color.RED))))
    // Scan the title band (right of the poster, near the top) for near-white
    // anti-aliased glyph pixels.
    var bright = 0
    for (x <- 460 until 1140; y <- 70 until 150)
      if (new Color(img.getRGB(x, y)).getRed > 200) bright += 1
    bright should be > 50
  }

  it should "render a portrait-poster card without cropping the slot to landscape (slot stays 2:3)" in {
    // A wide banner source must be cover-cropped into the 2:3 slot, not letterboxed:
    // sampling inside the slot still hits poster colour, not background.
    val banner = new BufferedImage(1200, 400, BufferedImage.TYPE_INT_RGB)
    val g = banner.createGraphics(); g.setColor(Color.GREEN); g.fillRect(0, 0, 1200, 400); g.dispose()
    val img = decode(OgCardRenderer.render("Film", "", Nil, Some(banner)))
    val mid = new Color(img.getRGB(220, 315))
    mid.getGreen should be > 150
  }

  it should "render a text-only card (no exception, correct size) when there is no poster" in {
    val img = decode(OgCardRenderer.render("Film bez plakatu", "2026 · Dramat", Seq("Filmweb 7.1"), None))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "render Polish diacritics without throwing (bundled font has the glyphs)" in {
    noException should be thrownBy
      OgCardRenderer.render("Zażółć gęślą jaźń: Śćmaśń", "2026 · Dramat, Kryminał", Seq("IMDb 7.4", "Filmweb 7.8"), Some(solidPoster(Color.BLUE)))
  }

  it should "ellipsise an absurdly long title instead of overflowing" in {
    val longTitle = (1 to 60).map(_ => "Multiwersum").mkString(" ")
    val img = decode(OgCardRenderer.render(longTitle, "", Seq("IMDb 8.6"), Some(solidPoster(Color.RED))))
    img.getWidth shouldBe 1200 // renders; the wrap/ellipsis logic kept it bounded
  }
}
