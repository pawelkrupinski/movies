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

  // True if any pixel is within `tol` of `target` on every channel. Used to
  // assert a rating badge's brand colour is actually painted. No poster in
  // these cases, so a match can only come from the badge itself.
  private def hasColourNear(img: BufferedImage, target: Color, tol: Int = 10): Boolean = {
    var x = 0
    while (x < img.getWidth) {
      var y = 0
      while (y < img.getHeight) {
        val c = new Color(img.getRGB(x, y))
        if (math.abs(c.getRed - target.getRed) <= tol &&
            math.abs(c.getGreen - target.getGreen) <= tol &&
            math.abs(c.getBlue - target.getBlue) <= tol) return true
        y += 2
      }
      x += 2
    }
    false
  }

  private val ImdbGold = new Color(0xf5, 0xc5, 0x18)
  private val MetaGreen = new Color(0x66, 0xcc, 0x66)
  private val RtRed = new Color(0xfa, 0x32, 0x0a)
  private val RtGreen = new Color(0x1a, 0x8f, 0x1a)
  private val FwOrange = new Color(0xff, 0x6c, 0x00)

  "OgCardRenderer" should "produce a 1200×630 PNG" in {
    val bytes = OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED)))
    val img   = decode(bytes)
    img should not be null
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "composite the poster into the left slot and keep a dark background on the right" in {
    val img = decode(OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED))))

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
    val img = decode(OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED))))
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
    val img = decode(OgCardRenderer.render("Film bez plakatu", "2026 · Dramat", OgCardRenderer.ratingBadges(None, None, None, Some(7.1)), None))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  private def col(poster: Color, title: String = "Incepcja"): (CityCardFilm, Option[BufferedImage]) =
    CityCardFilm(
      title     = title,
      meta      = Seq("2h 28min", "2010", "Sci-Fi"),
      badges    = OgCardRenderer.ratingBadges(Some(8.8), Some(74), Some(87), Some(7.5)),
      posterUrl = None,
      dayLabel  = "Sobota 20 czerwca",
      showings  = Seq("Multikino Stary Browar" -> Seq("18:30 2D", "21:00 DUB")),
    ) -> Some(solidPoster(poster))

  private def fiveCols(c: Color) = Seq.fill(5)(col(c))

  "OgCardRenderer.renderCityPageCard" should "render the page-like grid and keep the left brand panel dark" in {
    val img = decode(OgCardRenderer.renderCityPageCard("Repertuar kin w Poznaniu", fiveCols(Color.RED)))
    img.getWidth shouldBe 1200
    img.getHeight shouldBe 630
    // A right-hand poster shows through where the gradient has faded.
    val poster = new Color(img.getRGB(1130, 80))
    poster.getRed should be > 150
    poster.getRed should be > (poster.getBlue + 80)
    // Left wordmark band stays dark behind the white text (gradient opaque here).
    new Color(img.getRGB(90, 315)).getRed should be < 80
  }

  it should "draw the white 'Kinowo' wordmark and the city line on the left" in {
    val img = decode(OgCardRenderer.renderCityPageCard("Repertuar kin w Poznaniu", Seq(col(Color.RED))))
    var bright = 0
    for (x <- 80 until 560; y <- 200 until 430)
      if (new Color(img.getRGB(x, y)).getRed > 200) bright += 1
    bright should be > 50
  }

  it should "paint the per-film rating pills and showtime chips into the cards" in {
    val img = decode(OgCardRenderer.renderCityPageCard("Repertuar kin w Poznaniu", fiveCols(Color.BLUE)))
    hasColourNear(img, ImdbGold) shouldBe true                              // an in-card rating pill
    hasColourNear(img, new Color(0xaa, 0xd4, 0xff), tol = 30) shouldBe true // a showtime chip's text
  }

  it should "render a clean brand-only card (correct size) when there are no films" in {
    val img = decode(OgCardRenderer.renderCityPageCard("Repertuar kin we Wrocławiu", Nil))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "render Polish diacritics without throwing (bundled font has the glyphs)" in {
    noException should be thrownBy
      OgCardRenderer.render("Zażółć gęślą jaźń: Śćmaśń", "2026 · Dramat, Kryminał", OgCardRenderer.ratingBadges(Some(7.4), None, None, Some(7.8)), Some(solidPoster(Color.BLUE)))
  }

  it should "ellipsise an absurdly long title instead of overflowing" in {
    val longTitle = (1 to 60).map(_ => "Multiwersum").mkString(" ")
    val img = decode(OgCardRenderer.render(longTitle, "", OgCardRenderer.ratingBadges(Some(8.6), None, None, None), Some(solidPoster(Color.RED))))
    img.getWidth shouldBe 1200 // renders; the wrap/ellipsis logic kept it bounded
  }

  // Count bright (text) pixels in the body band below the ratings, right of the
  // poster — where the director + synopsis lines land.
  private def brightBodyPixels(img: BufferedImage): Int = {
    var bright = 0
    for (x <- 460 until 1140; y <- 300 until 520)
      if (new Color(img.getRGB(x, y)).getRed > 150) bright += 1
    bright
  }

  it should "render the synopsis (and director) text in the space below the ratings" in {
    val synopsis = (1 to 40).map(_ => "Bohaterka").mkString(" ")
    val withBody = decode(OgCardRenderer.render(
      "Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None),
      Some(solidPoster(Color.RED)), director = Some("Christopher Nolan"), synopsis = Some(synopsis)))
    val without  = decode(OgCardRenderer.render(
      "Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None),
      Some(solidPoster(Color.RED))))
    // The body band is essentially empty without the new copy, and full of glyph
    // pixels with it.
    brightBodyPixels(without) should be < 50
    brightBodyPixels(withBody) should be > 400
  }

  it should "keep the synopsis clear of the footer (no body text overwrites the footer line)" in {
    // An absurdly long synopsis must be capped/ellipsised, never spilling onto
    // the kinowo.fly.dev footer at the very bottom.
    val flood = (1 to 400).map(_ => "Tekst").mkString(" ")
    val img = decode(OgCardRenderer.render(
      "Film", "2026 · Dramat", OgCardRenderer.ratingBadges(Some(7.1), None, None, None),
      None, synopsis = Some(flood)))
    // Band just above the footer baseline (Height-Margin = 574) must stay dark:
    // the body copy stops short of it.
    var bright = 0
    for (x <- 60 until 1140; y <- 545 until 565)
      if (new Color(img.getRGB(x, y)).getRed > 150) bright += 1
    bright shouldBe 0
  }

  // ── Rating badge brand colours mirror the web `_ratingStyles` exactly ──────

  it should "paint the IMDb badge label in its brand gold (#f5c518)" in {
    val img = decode(OgCardRenderer.render("Film", "2026", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), None))
    hasColourNear(img, ImdbGold) shouldBe true
  }

  it should "render Metacritic as a solid green pill (#66cc66)" in {
    val img = decode(OgCardRenderer.render("Film", "2026", OgCardRenderer.ratingBadges(None, Some(77), None, None), None))
    hasColourNear(img, MetaGreen) shouldBe true
  }

  it should "use a red RT label when fresh (≥60%) and a green one when rotten" in {
    val fresh  = decode(OgCardRenderer.render("Film", "2026", OgCardRenderer.ratingBadges(None, None, Some(90), None), None))
    val rotten = decode(OgCardRenderer.render("Film", "2026", OgCardRenderer.ratingBadges(None, None, Some(30), None), None))
    hasColourNear(fresh,  RtRed)   shouldBe true
    hasColourNear(fresh,  RtGreen) shouldBe false
    hasColourNear(rotten, RtGreen) shouldBe true
    hasColourNear(rotten, RtRed)   shouldBe false
  }

  it should "paint the Filmweb badge label in its brand orange (#ff6c00)" in {
    val img = decode(OgCardRenderer.render("Film", "2026", OgCardRenderer.ratingBadges(None, None, None, Some(7.2)), None))
    hasColourNear(img, FwOrange) shouldBe true
  }
}
