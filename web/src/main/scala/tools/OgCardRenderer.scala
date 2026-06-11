package tools

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font, GradientPaint, Graphics2D, RenderingHints}
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

/**
 * Renders the 1200×630 Open Graph "share card" for a film page — the image
 * WhatsApp / Messenger / Slack / Telegram / X embed when a `/film` link is
 * shared.
 *
 * Why a server-rendered composite instead of just handing the raw poster to
 * `og:image`: every large-card preview (Messenger / Facebook especially)
 * crops a portrait 2:3 poster to its own ~1.91:1 frame and never surfaces the
 * rating text that lives in `og:description`. Baking the poster + title +
 * rating badges into one 1200×630 image (which is already 1.91:1) makes the
 * crop a no-op and puts the ratings *inside* the picture, so they always show.
 *
 * Pure: card data + an optional already-decoded poster in, PNG bytes out. No
 * HTTP and no Mongo — the poster fetch + memoisation live in [[OgCardService]]
 * so this stays trivially unit-testable (render to a BufferedImage, sample
 * pixels).
 *
 * Fonts are bundled (DejaVu Sans, full Polish coverage) and loaded via
 * `Font.createFont`, because the prod base image (`eclipse-temurin:25-jre`)
 * ships no system fonts — a logical `SANS_SERIF` would rasterise tofu for
 * `ł ę ó ż ś ć ń ą`.
 */
object OgCardRenderer {
  val Width  = 1200
  val Height = 630

  private val Margin  = 56
  private val PosterH = Height - 2 * Margin          // 518
  private val PosterW = (PosterH * 2) / 3            // 345 — a 2:3 poster slot
  private val Gutter  = 56
  private val PosterTextX = Margin + PosterW + Gutter // text column when a poster is shown

  private val Bg        = new Color(0x14, 0x17, 0x1f)
  private val BgBottom  = new Color(0x09, 0x0a, 0x0f)
  private val TitleCol  = Color.WHITE
  private val SubCol    = new Color(0x9a, 0xa3, 0xb2)
  private val PillBg    = new Color(0x23, 0x28, 0x33)
  private val PillText  = new Color(0xf0, 0xd9, 0x8a) // warm gold, matches the app's accent
  private val FooterCol = new Color(0x70, 0x78, 0x86)
  private val PosterBorder = new Color(0xff, 0xff, 0xff, 36)

  private def loadFont(resource: String): Font = {
    val is = getClass.getResourceAsStream(resource)
    if (is == null) throw new IllegalStateException(s"Bundled font not found on classpath: $resource")
    try Font.createFont(Font.TRUETYPE_FONT, is)
    catch { case e: Exception => throw new IllegalStateException(s"Failed to load bundled font $resource", e) }
    finally is.close()
  }

  // Loaded once; `deriveFont` is cheap and thread-safe per call.
  private val regular = loadFont("/fonts/DejaVuSans.ttf")
  private val bold    = loadFont("/fonts/DejaVuSans-Bold.ttf")

  /** Compose the card. `subtitle` is the year · genres line; `ratings` are
   *  pre-formatted tokens ("IMDb 8.8", "RT 87%"); `poster` is the decoded
   *  poster image or None (text-only card for films with no poster). */
  def render(title: String, subtitle: String, ratings: Seq[String], poster: Option[BufferedImage]): Array[Byte] = {
    val img = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_RGB)
    val g   = img.createGraphics()
    try {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)

      g.setPaint(new GradientPaint(0f, 0f, Bg, 0f, Height.toFloat, BgBottom))
      g.fillRect(0, 0, Width, Height)

      val textLeft = poster match {
        case Some(p) => drawPoster(g, p); PosterTextX
        case None    => Margin
      }
      val textRight = Width - Margin
      val textW     = textRight - textLeft

      var y = Margin + 12

      g.setFont(bold.deriveFont(60f))
      g.setColor(TitleCol)
      val titleFm = g.getFontMetrics
      for (line <- wrap(g, title, textW, maxLines = 3)) {
        y += titleFm.getAscent
        g.drawString(line, textLeft, y)
        y += titleFm.getDescent + 4
      }

      if (subtitle.nonEmpty) {
        y += 14
        g.setFont(regular.deriveFont(32f))
        g.setColor(SubCol)
        val sfm = g.getFontMetrics
        y += sfm.getAscent
        g.drawString(ellipsize(g, subtitle, textW), textLeft, y)
        y += sfm.getDescent
      }

      if (ratings.nonEmpty) {
        y += 36
        drawPills(g, ratings, textLeft, y, textRight)
      }

      g.setFont(regular.deriveFont(28f))
      g.setColor(FooterCol)
      val ffm    = g.getFontMetrics
      val footer = "kinowo.fly.dev"
      g.drawString(footer, textRight - ffm.stringWidth(footer), Height - Margin)
    } finally g.dispose()

    val baos = new ByteArrayOutputStream()
    ImageIO.write(img, "png", baos)
    baos.toByteArray
  }

  /** Cover-scale the poster into the 2:3 slot, clipped to a rounded rect with
   *  a faint border — so off-2:3 sources fill the slot instead of letterboxing,
   *  same intent as the on-page card's `object-fit: cover`. */
  private def drawPoster(g: Graphics2D, p: BufferedImage): Unit = {
    val arc   = 22f
    val scale = math.max(PosterW.toDouble / p.getWidth, PosterH.toDouble / p.getHeight)
    val sw    = math.round(p.getWidth * scale).toInt
    val sh    = math.round(p.getHeight * scale).toInt
    val dx    = Margin - (sw - PosterW) / 2
    val dy    = Margin - (sh - PosterH) / 2
    val slot  = new RoundRectangle2D.Float(Margin.toFloat, Margin.toFloat, PosterW.toFloat, PosterH.toFloat, arc, arc)
    val prev  = g.getClip
    g.setClip(slot)
    g.drawImage(p, dx, dy, sw, sh, null)
    g.setClip(prev)
    g.setColor(PosterBorder)
    g.setStroke(new BasicStroke(2f))
    g.draw(slot)
  }

  /** A row (wrapping to a second row if needed) of rounded rating pills. */
  private def drawPills(g: Graphics2D, pills: Seq[String], x0: Int, top: Int, xMax: Int): Unit = {
    g.setFont(bold.deriveFont(30f))
    val fm   = g.getFontMetrics
    val padX = 22
    val padY = 12
    val gap  = 16
    val arc  = 16f
    val h    = fm.getHeight + padY * 2
    var x    = x0
    var y    = top
    for (pill <- pills) {
      val w = fm.stringWidth(pill) + padX * 2
      if (x + w > xMax && x > x0) { x = x0; y += h + 14 }
      g.setColor(PillBg)
      g.fill(new RoundRectangle2D.Float(x.toFloat, y.toFloat, w.toFloat, h.toFloat, arc, arc))
      g.setColor(PillText)
      g.drawString(pill, x + padX, y + padY + fm.getAscent)
      x += w + gap
    }
  }

  /** Greedy word-wrap to `maxLines`; if more lines remain, the last kept line
   *  is ellipsised so an over-long title never spills past the card. */
  private def wrap(g: Graphics2D, text: String, maxW: Int, maxLines: Int): Seq[String] = {
    val all = wrapAll(g, text, maxW)
    if (all.length <= maxLines) all
    else all.take(maxLines - 1) :+ ellipsize(g, all.drop(maxLines - 1).mkString(" "), maxW)
  }

  private def wrapAll(g: Graphics2D, text: String, maxW: Int): Seq[String] = {
    val fm    = g.getFontMetrics
    val words = text.trim.split("\\s+").filter(_.nonEmpty)
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    var cur   = ""
    for (w <- words) {
      val cand = if (cur.isEmpty) w else s"$cur $w"
      if (cur.isEmpty || fm.stringWidth(cand) <= maxW) cur = cand
      else { lines += cur; cur = w }
    }
    if (cur.nonEmpty) lines += cur
    if (lines.isEmpty) Seq("") else lines.toSeq
  }

  /** Trim `s` with a trailing ellipsis until it fits `maxW`. */
  private def ellipsize(g: Graphics2D, s: String, maxW: Int): String = {
    val fm = g.getFontMetrics
    if (fm.stringWidth(s) <= maxW) return s
    val ell = "…"
    var end = s.length
    while (end > 0 && fm.stringWidth(s.substring(0, end) + ell) > maxW) end -= 1
    s.substring(0, end).trim + ell
  }
}
