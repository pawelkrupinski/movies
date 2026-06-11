package tools

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{Color, Font, GradientPaint, Graphics2D, RenderingHints}
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

  private val Margin  = 56                  // text inset from the top/right/bottom edges
  // Full-bleed poster: flush to the left/top/bottom edges (no padding), spanning
  // the whole card height, so it's as large as a 2:3 poster can be here.
  private val PosterH = Height               // 630
  private val PosterW = (Height * 2) / 3     // 420 — 2:3 at full height
  private val Gutter  = 48
  private val PosterTextX = PosterW + Gutter // text column when a poster is shown

  private val Bg        = new Color(0x14, 0x17, 0x1f)
  private val BgBottom  = new Color(0x09, 0x0a, 0x0f)
  private val TitleCol  = Color.WHITE
  private val SubCol    = new Color(0x9a, 0xa3, 0xb2)
  private val FooterCol = new Color(0x70, 0x78, 0x86)

  // ── Rating badges — mirror the web `_ratingStyles` two-segment pills (a
  //    coloured brand label + a dark value segment), so a shared card looks
  //    like the on-site/iOS/Android ratings. Hex values copied verbatim. ──
  private def rgb(hex: Int) = new Color((hex >> 16) & 0xff, (hex >> 8) & 0xff, hex & 0xff)
  private val ValueBg = rgb(0x2a2a3e) // the dark value-segment background shared by IMDb/RT/FW

  /** One coloured segment of a rating badge. `bold` picks the label weight (700)
   *  vs the value weight; `padX` mirrors the web label (.3rem) vs value (.35rem)
   *  horizontal padding, scaled to the card's font. */
  case class Seg(text: String, bg: Color, fg: Color, bold: Boolean, padX: Int)

  /** A rating badge = one or two adjoining [[Seg]]s (Metacritic is value-only),
   *  drawn as a single rounded pill with a square seam between segments. */
  case class Badge(segs: Seq[Seg])

  /** Build the rating badges in the web's order (IMDb, Metacritic, RT, FW) with
   *  the exact `_ratingStyles` colours, skipping sources that aren't set. RT is
   *  red-label "fresh" (≥60%) vs green-label "rotten", matching the site. */
  def ratingBadges(imdb: Option[Double], metascore: Option[Int],
                   rottenTomatoes: Option[Int], filmweb: Option[Double]): Seq[Badge] = {
    val lp = 18 // label padX  (~.3rem at the card font)
    val vp = 22 // value padX  (~.35rem)
    Seq(
      imdb.map(r => Badge(Seq(
        Seg("IMDb", rgb(0xf5c518), Color.BLACK, bold = true, lp),
        Seg(f"$r%.1f", ValueBg, rgb(0xf5c518), bold = false, vp)))),
      metascore.map(m => Badge(Seq(
        Seg(m.toString, rgb(0x66cc66), rgb(0x002200), bold = true, vp)))),
      rottenTomatoes.map { rt =>
        val fresh = rt >= 60
        Badge(Seq(
          Seg("RT", rgb(if (fresh) 0xfa320a else 0x1a8f1a), Color.WHITE, bold = true, lp),
          Seg(s"$rt%", ValueBg, rgb(if (fresh) 0xff7c5a else 0x6cd06c), bold = false, vp)))
      },
      filmweb.map(r => Badge(Seq(
        Seg("FW", rgb(0xff6c00), Color.WHITE, bold = true, lp),
        Seg(f"$r%.1f", ValueBg, rgb(0xff9c4a), bold = false, vp))))
    ).flatten
  }

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

  /** Compose the card. `subtitle` is the year · genres line; `badges` are the
   *  rating pills from [[ratingBadges]]; `poster` is the decoded poster image or
   *  None (text-only card for films with no poster). */
  def render(title: String, subtitle: String, badges: Seq[Badge], poster: Option[BufferedImage]): Array[Byte] = {
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

      if (badges.nonEmpty) {
        y += 36
        drawBadges(g, badges, textLeft, y, textRight)
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

  /** Cover-scale the poster to fill the full-bleed left column (flush to the
   *  top/left/bottom edges, no padding or border), cropping the overflow —
   *  same intent as the on-page card's `object-fit: cover`. */
  private def drawPoster(g: Graphics2D, p: BufferedImage): Unit = {
    val scale = math.max(PosterW.toDouble / p.getWidth, PosterH.toDouble / p.getHeight)
    val sw    = math.round(p.getWidth * scale).toInt
    val sh    = math.round(p.getHeight * scale).toInt
    val dx    = -(sw - PosterW) / 2
    val dy    = -(sh - PosterH) / 2
    val prev  = g.getClip
    g.setClip(0, 0, PosterW, PosterH)
    g.drawImage(p, dx, dy, sw, sh, null)
    g.setClip(prev)
  }

  /** A row (wrapping to a second row if needed) of two-segment rating badges.
   *  Each badge is filled segment-by-segment while clipped to its rounded outer
   *  rect, so the outer corners are rounded (radius like the web's 3px, scaled)
   *  and the label/value seam stays square — exactly the web pill. */
  private def drawBadges(g: Graphics2D, badges: Seq[Badge], x0: Int, top: Int, xMax: Int): Unit = {
    val fontSize = 30f
    val labelFont = bold.deriveFont(fontSize)
    val valueFont = regular.deriveFont(fontSize)
    def fontFor(s: Seg)  = if (s.bold) labelFont else valueFont
    val padY = 11
    val gap  = 14
    val arcD = 16f // corner diameter (radius ~8, ~web 3px scaled to this font)
    // Uniform height across every badge, from the (taller) bold metrics.
    val refFm = g.getFontMetrics(labelFont)
    val h      = refFm.getAscent + refFm.getDescent + padY * 2
    var x = x0
    var y = top
    for (b <- badges) {
      val fms   = b.segs.map(s => g.getFontMetrics(fontFor(s)))
      val segW  = b.segs.zip(fms).map { case (s, fm) => fm.stringWidth(s.text) + s.padX * 2 }
      val w     = segW.sum
      if (x + w > xMax && x > x0) { x = x0; y += h + 12 }
      val outer = new RoundRectangle2D.Float(x.toFloat, y.toFloat, w.toFloat, h.toFloat, arcD, arcD)
      val saved = g.getClip
      g.setClip(outer) // rounds the outer corners; the per-segment fills keep a square seam
      var sx = x
      for (((s, fm), sw) <- b.segs.zip(fms).zip(segW)) {
        g.setColor(s.bg)
        g.fillRect(sx, y, sw, h)
        g.setColor(s.fg)
        g.setFont(fontFor(s))
        val ty = y + (h - (fm.getAscent + fm.getDescent)) / 2 + fm.getAscent
        g.drawString(s.text, sx + s.padX, ty)
        sx += sw
      }
      g.setClip(saved)
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
