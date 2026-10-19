package tools

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{Color, LinearGradientPaint}

/**
 * Renders a CITY's 1200×630 Open Graph card — the image a shared `/{city}/` link previews with.
 *
 * It keeps the look of the card it replaces (a CDP screenshot of the live page under an overlay,
 * `tools.OgCardGenerator`, until 2026-10), drawn from data instead of a browser: the posters of
 * the films the city is showing, in a grid of rounded tiles filling the card from the right, under
 * the same left-dark gradient, with the brand wordmark, the city's heading
 * ([[models.City.heading]], in the country's language), the rating pills as the site draws them
 * (sample values: they say "with ratings", not any one film's) and the country's host. A city with
 * no posters yet is the gradient and the text alone.
 *
 * Pure, like [[OgCardRenderer]], whose fonts, pills and encoder it shares: decoded posters in, a
 * raster out. The worker picks the films and reads their posters from its poster cache.
 */
object CityCardRenderer {
  import OgCardRenderer.{Width, Height}

  /** What a country contributes to its cities' cards: the wordmark, the host drawn under the pills,
   *  and whether the Filmweb pill shows (Poland only). */
  final case class Brand(name: String, host: String, filmweb: Boolean)

  /** The poster tiles: two rows of 2:3 tiles, from the right edge leftwards, the first film at the
   *  top right — the corner the gradient leaves clearest. Six columns overflow the card's left edge,
   *  under the darkest part of the gradient. */
  val TileWidth: Int  = 200
  val TileHeight: Int = 300
  private val Gap     = 10
  private val Rows    = 2
  private val Columns = 6
  /** The most posters a card draws. */
  val PosterCount: Int = Rows * Columns

  private val Background = new Color(0x0d, 0x0d, 0x22)
  private def shade(alpha: Double) = new Color(0x0d, 0x0d, 0x22, math.round(alpha * 255).toInt)
  // The overlay's `linear-gradient(90deg, .97 0%, .92 30%, .3 64%, 0 100%)`.
  private val Gradient = new LinearGradientPaint(0f, 0f, Width.toFloat, 0f,
    Array(0f, .30f, .64f, 1f), Array(shade(.97), shade(.92), shade(.30), shade(0)))

  private val TextLeft  = 84
  private val TextWidth = 560
  private val PillsWidth = 640
  private val PillScale  = 25f / 30f

  /** The pills drawn on every city card: the site's rating sources, at sample values. */
  def samplePills(filmweb: Boolean): Seq[OgCardRenderer.Badge] =
    OgCardRenderer.ratingBadges(imdb = Some(7.8), metascore = Some(81), rottenTomatoes = Some(91), filmweb = Option.when(filmweb)(7.4))

  /** Where poster `index` (0 = the city's first film) is drawn: its tile's top-left corner. */
  def tileAt(index: Int): (Int, Int) = {
    val column = index / Rows
    val row    = index % Rows
    val top    = (Height - Rows * TileHeight - (Rows - 1) * Gap) / 2
    (Width - Gap - (column + 1) * TileWidth - column * Gap, top + row * (TileHeight + Gap))
  }

  def render(heading: String, brand: Brand, posters: Seq[BufferedImage]): BufferedImage = {
    val img = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_RGB)
    val g   = img.createGraphics()
    try {
      OgCardRenderer.applyHints(g)
      g.setColor(Background)
      g.fillRect(0, 0, Width, Height)

      posters.take(PosterCount).zipWithIndex.foreach { case (poster, i) =>
        val (x, y) = tileAt(i)
        val saved  = g.getClip
        g.setClip(new RoundRectangle2D.Float(x.toFloat, y.toFloat, TileWidth.toFloat, TileHeight.toFloat, 16f, 16f))
        OgCardRenderer.drawCover(g, poster, x, y, TileWidth, TileHeight)
        g.setClip(saved)
      }

      g.setPaint(Gradient)
      g.fillRect(0, 0, Width, Height)

      // The text block is centred vertically, as the overlay's `top:50%; translateY(-50%)` did:
      // measured first, then drawn.
      val brandFont   = OgCardRenderer.bold.deriveFont(86f)
      val headingFont = OgCardRenderer.regular.deriveFont(36f)
      val hostFont    = OgCardRenderer.bold.deriveFont(25f)
      val brandFm     = g.getFontMetrics(brandFont)
      val headingFm   = g.getFontMetrics(headingFont)
      val hostFm      = g.getFontMetrics(hostFont)
      g.setFont(headingFont)
      val headingLines = OgCardRenderer.wrap(g, heading, TextWidth - 20, maxLines = 3)
      val headingLineH = headingFm.getAscent + headingFm.getDescent + 6
      val pills        = samplePills(brand.filmweb)
      val pillsHeight  = OgCardRenderer.drawBadges(g, pills, TextLeft, 0, TextLeft + PillsWidth, paint = false, scale = PillScale)
      val blockHeight  = brandFm.getAscent + brandFm.getDescent + 18 + headingLines.size * headingLineH +
        30 + pillsHeight + 28 + hostFm.getAscent + hostFm.getDescent
      var y = (Height - blockHeight) / 2

      g.setColor(Color.WHITE)
      g.setFont(brandFont)
      y += brandFm.getAscent
      g.drawString(brand.name, TextLeft, y)
      y += brandFm.getDescent + 18

      g.setFont(headingFont)
      headingLines.foreach { line =>
        y += headingFm.getAscent
        g.drawString(line, TextLeft, y)
        y += headingFm.getDescent + 6
      }

      y += 30
      y = OgCardRenderer.drawBadges(g, pills, TextLeft, y, TextLeft + PillsWidth, scale = PillScale)

      y += 28 + hostFm.getAscent
      g.setFont(hostFont)
      g.setColor(Color.WHITE)
      g.drawString(brand.host, TextLeft, y)
    } finally g.dispose()
    img
  }
}
