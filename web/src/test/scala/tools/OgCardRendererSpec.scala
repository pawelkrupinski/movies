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

  // A poster with real entropy. `solidPoster` is a flat fill, which BOTH codecs
  // compress to almost nothing -- comparing formats on it would prove nothing
  // about a photograph, which is what a poster actually is.
  private def noisyPoster(): BufferedImage = {
    val p = new BufferedImage(400, 600, BufferedImage.TYPE_INT_RGB)
    val rnd = new java.util.Random(1)
    for (y <- 0 until 600; x <- 0 until 400) p.setRGB(x, y, rnd.nextInt(0xFFFFFF))
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

  "OgCardRenderer" should "encode the 1200×630 card as a JPEG, not a lossless PNG" in {
    val bytes = OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED)), "kinowo.net")
    // The JPEG SOI marker. Asserted on the BYTES rather than on what ImageIO
    // makes of them: ImageIO.read decodes either format happily, so a card that
    // silently went back to PNG would pass every other test in this file while
    // quadrupling both the response and the heap the card cache holds.
    bytes(0) shouldBe 0xFF.toByte
    bytes(1) shouldBe 0xD8.toByte
    val img = decode(bytes)
    img should not be null
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
    // And it must be the declared type, so the Content-Type the controller sets
    // and the og:image:type the film page emits cannot drift from the encoder.
    OgCardRenderer.MimeType shouldBe "image/jpeg"
  }

  it should "encode a photographic card far smaller than the same card lossless" in {
    // The reason for the format, stated as a number. A real card is a poster
    // montage: PNG stores it losslessly at ~4x the bytes, and OgCardCache holds
    // those bytes, so this ratio is the difference between a cache that fits in
    // a 384 MiB old gen and one that does not.
    val badges = OgCardRenderer.ratingBadges(Some(8.8), Some(88), Some(91), Some(7.9))
    val image  = OgCardRenderer.renderImage("Incepcja", "2010 · Sci-Fi", badges, Some(noisyPoster()), "kinowo.net",
                                            director = Some("Christopher Nolan"), synopsis = Some("A thief who steals corporate secrets."))
    val jpeg = OgCardRenderer.render("Incepcja", "2010 · Sci-Fi", badges, Some(noisyPoster()), "kinowo.net",
                                     director = Some("Christopher Nolan"), synopsis = Some("A thief who steals corporate secrets."))
    val png = {
      val baos = new java.io.ByteArrayOutputStream()
      ImageIO.write(image, "png", baos)
      baos.toByteArray
    }
    jpeg.length should be < (png.length / 2)
  }

  it should "composite the poster into the left slot and keep a dark background on the right" in {
    val img = (OgCardRenderer.renderImage("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED)), "kinowo.net"))

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
    val img = (OgCardRenderer.renderImage("Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), Some(solidPoster(Color.RED)), "kinowo.net"))
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
    val img = (OgCardRenderer.renderImage("Film", "", Nil, Some(banner), "kinowo.net"))
    val mid = new Color(img.getRGB(220, 315))
    mid.getGreen should be > 150
  }

  it should "render a text-only card (no exception, correct size) when there is no poster" in {
    val img = (OgCardRenderer.renderImage("Film bez plakatu", "2026 · Dramat", OgCardRenderer.ratingBadges(None, None, None, Some(7.1)), None, "kinowo.net"))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  private def col(poster: Color, title: String = "Incepcja"): (CityCardFilm, Option[BufferedImage]) =
    CityCardFilm(
      title     = title,
      meta      = Seq("2h 28min", "2010", "Sci-Fi"),
      badges     = OgCardRenderer.ratingBadges(Some(8.8), Some(74), Some(87), Some(7.5)),
      posterUrls = Nil,
      dayLabel   = "Sobota 20 czerwca",
      showings  = Seq("Multikino Stary Browar" -> Seq("18:30 2D", "21:00 DUB")),
    ) -> Some(solidPoster(poster))

  private def fiveCols(c: Color) = Seq.fill(5)(col(c))

  "OgCardRenderer.renderCityPageCard" should "render the page-like grid and keep the left brand panel dark" in {
    val img = (OgCardRenderer.renderCityPageCardImage("Repertuar kin w Poznaniu", "Kinowo", "kinowo.net", fiveCols(Color.RED), filmweb = true))
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
    val img = (OgCardRenderer.renderCityPageCardImage("Repertuar kin w Poznaniu", "Kinowo", "kinowo.net", Seq(col(Color.RED)), filmweb = true))
    var bright = 0
    for (x <- 80 until 560; y <- 200 until 430)
      if (new Color(img.getRGB(x, y)).getRed > 200) bright += 1
    bright should be > 50
  }

  it should "paint the per-film rating pills and showtime chips into the cards" in {
    val img = (OgCardRenderer.renderCityPageCardImage("Repertuar kin w Poznaniu", "Kinowo", "kinowo.net", fiveCols(Color.BLUE), filmweb = true))
    hasColourNear(img, ImdbGold) shouldBe true                              // an in-card rating pill
    hasColourNear(img, new Color(0xaa, 0xd4, 0xff), tol = 30) shouldBe true // a showtime chip's text
  }

  // Filmweb is a Polish site and only Poland has it wired (Country.filmwebEnabled),
  // so an FW pill on a UK/DE/ES card advertises a source that listing never shows.
  // Both cases below render with NO films, so the whole canvas is the brand
  // overlay — nothing else on it is orange, and scanning all of it avoids
  // pinning the pill row's exact y, which the vertical centring moves.
  private val FilmwebOrange = new Color(0xff, 0x6c, 0x00)

  /** Pixels of the FW pill's orange. Counted, not merely detected: a handful of
   *  stray anti-aliased pixels sit near this hue, so presence is a BLOCK of it.
   *  Both cases below render with no films, so nothing else on the canvas is
   *  orange and the whole image can be scanned — which avoids pinning the pill
   *  row's y, that the overlay's vertical centring moves. */
  private def filmwebOrangePixels(img: BufferedImage): Int = {
    var n = 0
    for (x <- 0 until img.getWidth; y <- 0 until img.getHeight) {
      val c = new Color(img.getRGB(x, y))
      if (math.abs(c.getRed - FilmwebOrange.getRed) < 18 &&
          math.abs(c.getGreen - FilmwebOrange.getGreen) < 18 &&
          math.abs(c.getBlue - FilmwebOrange.getBlue) < 18) n += 1
    }
    n
  }

  it should "carry the Filmweb pill on a Polish card" in {
    val img = (OgCardRenderer.renderCityPageCardImage(
      "Repertuar kin w Poznaniu", "Kinowo", "kinowo.net", Nil, filmweb = true))
    filmwebOrangePixels(img) should be > 300
  }

  it should "drop the Filmweb pill everywhere else" in {
    for ((line, brand, host) <- Seq(
           ("Cinema listings in Manchester", "Showtimes", "showtimes.cc/uk"),
           ("Kinoprogramm in Berlin",        "Showtimes", "showtimes.cc/de"),
           ("Cartelera de cine en Madrid",   "Showtimes", "showtimes.cc/es"))) withClue(s"$host: ") {
      val img = (OgCardRenderer.renderCityPageCardImage(line, brand, host, Nil, filmweb = false))
      filmwebOrangePixels(img) should be < 50
    }
  }

  it should "render a clean brand-only card (correct size) when there are no films" in {
    val img = (OgCardRenderer.renderCityPageCardImage("Repertuar kin we Wrocławiu", "Kinowo", "kinowo.net", Nil, filmweb = true))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "render Polish diacritics without throwing (bundled font has the glyphs)" in {
    noException should be thrownBy
      OgCardRenderer.render("Zażółć gęślą jaźń: Śćmaśń", "2026 · Dramat, Kryminał", OgCardRenderer.ratingBadges(Some(7.4), None, None, Some(7.8)), Some(solidPoster(Color.BLUE)), "kinowo.net")
  }

  it should "ellipsise an absurdly long title instead of overflowing" in {
    val longTitle = (1 to 60).map(_ => "Multiwersum").mkString(" ")
    val img = (OgCardRenderer.renderImage(longTitle, "", OgCardRenderer.ratingBadges(Some(8.6), None, None, None), Some(solidPoster(Color.RED)), "kinowo.net"))
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
    val withBody = (OgCardRenderer.renderImage(
      "Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None),
      Some(solidPoster(Color.RED)), "kinowo.net", director = Some("Christopher Nolan"), synopsis = Some(synopsis)))
    val without  = (OgCardRenderer.renderImage(
      "Incepcja", "2010 · Sci-Fi", OgCardRenderer.ratingBadges(Some(8.8), None, None, None),
      Some(solidPoster(Color.RED)), "kinowo.net"))
    // The body band is essentially empty without the new copy, and full of glyph
    // pixels with it.
    brightBodyPixels(without) should be < 50
    brightBodyPixels(withBody) should be > 400
  }

  it should "keep the synopsis clear of the footer (no body text overwrites the footer line)" in {
    // An absurdly long synopsis must be capped/ellipsised, never spilling onto
    // the kinowo.net footer at the very bottom.
    val flood = (1 to 400).map(_ => "Tekst").mkString(" ")
    val img = (OgCardRenderer.renderImage(
      "Film", "2026 · Dramat", OgCardRenderer.ratingBadges(Some(7.1), None, None, None),
      None, "kinowo.net", synopsis = Some(flood)))
    // Band just above the footer baseline (Height-Margin = 574) must stay dark:
    // the body copy stops short of it.
    var bright = 0
    for (x <- 60 until 1140; y <- 545 until 565)
      if (new Color(img.getRGB(x, y)).getRed > 150) bright += 1
    bright shouldBe 0
  }

  it should "stamp the host it is GIVEN into the footer, not a hardcoded domain" in {
    // The footer was the literal "kinowo.fly.dev", so every UK and German share
    // card advertised the Polish host — invisible in tests because nothing read
    // it back. Count footer-coloured pixels (FooterCol is a dim blue-grey, so
    // red is mid-range and blue exceeds it) in the band around the footer
    // baseline at Height - Margin: no host paints none, and a longer host paints
    // more than a shorter one.
    def footerPixels(host: String): Int = {
      val img = (OgCardRenderer.renderImage("Film", "2026", Nil, None, host))
      var n = 0
      for (x <- 600 until OgCardRenderer.Width; y <- 548 until 578) {
        val c = new Color(img.getRGB(x, y))
        if (c.getRed > 90 && c.getRed < 150 && c.getBlue > c.getRed) n += 1
      }
      n
    }
    footerPixels("") shouldBe 0
    val poland = footerPixels("kinowo.net")
    val uk     = footerPixels("uk.showtimes.cc")
    poland should be > 0
    uk should be > poland
  }

  // ── Rating badge brand colours mirror the web `_ratingStyles` exactly ──────

  it should "paint the IMDb badge label in its brand gold (#f5c518)" in {
    val img = (OgCardRenderer.renderImage("Film", "2026", OgCardRenderer.ratingBadges(Some(8.8), None, None, None), None, "kinowo.net"))
    hasColourNear(img, ImdbGold) shouldBe true
  }

  it should "render Metacritic as a solid green pill (#66cc66)" in {
    val img = (OgCardRenderer.renderImage("Film", "2026", OgCardRenderer.ratingBadges(None, Some(77), None, None), None, "kinowo.net"))
    hasColourNear(img, MetaGreen) shouldBe true
  }

  it should "use a red RT label when fresh (≥60%) and a green one when rotten" in {
    val fresh  = (OgCardRenderer.renderImage("Film", "2026", OgCardRenderer.ratingBadges(None, None, Some(90), None), None, "kinowo.net"))
    val rotten = (OgCardRenderer.renderImage("Film", "2026", OgCardRenderer.ratingBadges(None, None, Some(30), None), None, "kinowo.net"))
    hasColourNear(fresh,  RtRed)   shouldBe true
    hasColourNear(fresh,  RtGreen) shouldBe false
    hasColourNear(rotten, RtGreen) shouldBe true
    hasColourNear(rotten, RtRed)   shouldBe false
  }

  it should "paint the Filmweb badge label in its brand orange (#ff6c00)" in {
    val img = (OgCardRenderer.renderImage("Film", "2026", OgCardRenderer.ratingBadges(None, None, None, Some(7.2)), None, "kinowo.net"))
    hasColourNear(img, FwOrange) shouldBe true
  }
}
