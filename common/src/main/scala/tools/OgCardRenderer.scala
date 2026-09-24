package tools

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{Color, Font, GradientPaint, Graphics2D, RenderingHints}
import java.io.ByteArrayOutputStream
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}

/**
 * Renders the 1200×630 Open Graph "share card" for a film page — the image
 * WhatsApp / Messenger / Slack / Telegram / X embed when a `/movie` link is
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
 * HTTP and no Mongo — the worker fetches and decodes the poster
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

  /** The card's wire format. Named here rather than spelled at each call site so
   *  the controller's Content-Type, the `og:image:type` meta tag and the encoder
   *  cannot drift apart -- a card served as `image/png` that is really a JPEG is
   *  the kind of disagreement a preview scraper refuses rather than reports. */
  val MimeType = "image/jpeg"

  /** See [[toJpeg]] for why 0.85 and not the ImageIO default. */
  private val JpegQuality = 0.85f

  private val Margin  = 56                  // text inset from the top/right/bottom edges
  // Full-bleed poster: flush to the left/top/bottom edges (no padding), spanning
  // the whole card height, so it's as large as a 2:3 poster can be here.
  private val PosterH = Height               // 630
  private val PosterW = (Height * 2) / 3     // 420 — 2:3 at full height

  /** The box the card cover-scales a poster into (the full-bleed column). [[PosterDecode]] reads
   *  posters subsampled to the smallest size that still covers it, and the worker's poster cache
   *  stores them at exactly this size ([[coverSlot]]). */
  val PosterSlotWidth: Int  = PosterW
  val PosterSlotHeight: Int = PosterH
  private val Gutter  = 48
  private val PosterTextX = PosterW + Gutter // text column when a poster is shown

  private val Bg          = new Color(0x14, 0x17, 0x1f)
  private val BgBottom    = new Color(0x09, 0x0a, 0x0f)
  private val TitleCol    = Color.WHITE
  private val SubCol      = new Color(0x9a, 0xa3, 0xb2)
  private val SynopsisCol = new Color(0xc2, 0xca, 0xd6) // a touch brighter than SubCol so the body copy reads
  private val FooterCol   = new Color(0x70, 0x78, 0x86)

  // Cap on synopsis lines so a long plot summary fills the space below the
  // ratings without crowding the footer; the available-height calc trims it
  // further when the title wraps or there are two rows of rating pills.
  private val MaxSynopsisLines = 6

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
   *  None (text-only card for films with no poster). `host` is the bare domain
   *  drawn in the footer — passed in rather than hardcoded so a UK card says
   *  `showtimes.cc/uk`, which the literal it replaced did not. `directorLine`
   *  (the caller's finished "Regie: Name, Name" — label included, because this
   *  renderer has no `Messages` and a literal here printed Polish onto every
   *  country's card) and `synopsis` fill the space below the ratings — both
   *  optional, each omitted when absent. */
  def render(title: String, subtitle: String, badges: Seq[Badge], poster: Option[BufferedImage],
             host: String, directorLine: Option[String] = None, synopsis: Option[String] = None): Array[Byte] =
    encodeCard(renderImage(title, subtitle, badges, poster, host, directorLine, synopsis))

  /** The film card as a raster, before it is encoded — its base ([[renderBase]]) with the badges
   *  drawn into their slot ([[withBadges]]). Separate from [[render]] so what the card LOOKS like
   *  and what it is ENCODED as are two questions with two answers: the specs that sample pixels
   *  assert on this, and only the format test goes through the lossy encoder. */
  def renderImage(title: String, subtitle: String, badges: Seq[Badge], poster: Option[BufferedImage],
                  host: String, directorLine: Option[String] = None, synopsis: Option[String] = None): BufferedImage =
    withBadges(renderBase(title, subtitle, poster, host, directorLine, synopsis),
               badgeSlot(title, subtitle, poster.isDefined), badges)

  /** Where the rating badges go: left edge, top, right edge and the height reserved for them. */
  final case class BadgeSlot(x: Int, y: Int, right: Int, height: Int)

  /** The badge rows' place on a card with this title and subtitle. A pure function of the LAYOUT
   *  inputs — never of the ratings — and it always reserves room for the widest set of badges the
   *  card can carry (all four, widest values), so the director and synopsis below never move when a
   *  rating does. That is what lets a card's base be cached and reused across rating changes. */
  def badgeSlot(title: String, subtitle: String, hasPoster: Boolean): BadgeSlot = {
    val scratch = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
    val g = scratch.createGraphics()
    try {
      applyHints(g)
      val textLeft  = if (hasPoster) PosterTextX else Margin
      val textRight = Width - Margin
      val top = titleBlockBottom(g, title, subtitle, textLeft, textRight - textLeft, draw = false) + 36
      BadgeSlot(textLeft, top, textRight, drawBadges(g, WidestBadges, textLeft, top, textRight, paint = false) - top)
    } finally g.dispose()
  }

  /** The widest badges a card can show: every source, at its widest value. */
  private val WidestBadges: Seq[Badge] = ratingBadges(imdb = Some(10.0), metascore = Some(100), rottenTomatoes = Some(100), filmweb = Some(10.0))

  /** The card WITHOUT its rating badges: background, poster, title, subtitle, director, synopsis and
   *  footer, with the badge rows ([[badgeSlot]]) left as background. */
  def renderBase(title: String, subtitle: String, poster: Option[BufferedImage], host: String,
                 directorLine: Option[String] = None, synopsis: Option[String] = None): BufferedImage = {
    val img = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_RGB)
    val g   = img.createGraphics()
    try {
      applyHints(g)

      g.setPaint(new GradientPaint(0f, 0f, Bg, 0f, Height.toFloat, BgBottom))
      g.fillRect(0, 0, Width, Height)

      val textLeft = poster match {
        case Some(p) => drawPoster(g, p); PosterTextX
        case None    => Margin
      }
      val textRight = Width - Margin
      val textW     = textRight - textLeft

      titleBlockBottom(g, title, subtitle, textLeft, textW, draw = true)
      val slot = badgeSlot(title, subtitle, poster.isDefined)
      var yPosition = slot.y + slot.height

      val footerBaseline = Height - Margin
      // Keep the body copy just clear of the footer line (~its ascent).
      val bodyBottom = footerBaseline - 30

      directorLine.map(_.trim).filter(_.nonEmpty).foreach { d =>
        yPosition += 26
        g.setFont(regular.deriveFont(27f))
        g.setColor(SubCol)
        val dfm = g.getFontMetrics
        yPosition += dfm.getAscent
        g.drawString(ellipsize(g, d, textW), textLeft, yPosition)
        yPosition += dfm.getDescent
      }

      synopsis.map(_.trim).filter(_.nonEmpty).foreach { text =>
        yPosition += 16
        g.setFont(regular.deriveFont(29f))
        g.setColor(SynopsisCol)
        val pfm    = g.getFontMetrics
        val lineH  = pfm.getAscent + pfm.getDescent + 6
        val fits   = math.max(0, (bodyBottom - yPosition) / lineH)
        val maxLines = math.min(MaxSynopsisLines, fits)
        if (maxLines > 0)
          for (line <- wrap(g, text, textW, maxLines)) {
            yPosition += pfm.getAscent
            g.drawString(line, textLeft, yPosition)
            yPosition += pfm.getDescent + 6
          }
      }

      g.setFont(regular.deriveFont(28f))
      g.setColor(FooterCol)
      val ffm    = g.getFontMetrics
      g.drawString(host, textRight - ffm.stringWidth(host), footerBaseline)
    } finally g.dispose()

    img
  }

  /** `badges` drawn into `slot` on `base` (in place), which is returned. */
  def withBadges(base: BufferedImage, slot: BadgeSlot, badges: Seq[Badge]): BufferedImage = {
    val g = base.createGraphics()
    try { applyHints(g); drawBadges(g, badges, slot.x, slot.y, slot.right) }
    finally g.dispose()
    base
  }

  /** Title (up to three lines) and subtitle from the top margin; the y just below them. Drawn, or
   *  only measured when `draw` is false. */
  private def titleBlockBottom(g: Graphics2D, title: String, subtitle: String, textLeft: Int, textW: Int, draw: Boolean): Int = {
    var yPosition = Margin + 12
    g.setFont(bold.deriveFont(60f))
    g.setColor(TitleCol)
    val titleFm = g.getFontMetrics
    for (line <- wrap(g, title, textW, maxLines = 3)) {
      yPosition += titleFm.getAscent
      if (draw) g.drawString(line, textLeft, yPosition)
      yPosition += titleFm.getDescent + 4
    }
    if (subtitle.nonEmpty) {
      yPosition += 14
      g.setFont(regular.deriveFont(32f))
      g.setColor(SubCol)
      val sfm = g.getFontMetrics
      yPosition += sfm.getAscent
      if (draw) g.drawString(ellipsize(g, subtitle, textW), textLeft, yPosition)
      yPosition += sfm.getDescent
    }
    yPosition
  }

  /** The card encoded for serving: JPEG at [[JpegQuality]]. */
  def encodeCard(img: BufferedImage): Array[Byte] = toJpeg(img)

  /** A card's BASE encoded for the worker's cache: JPEG at 0.95 with full-resolution chroma (4:4:4).
   *  It is decoded again for every ratings change, so it must carry as little loss as possible into
   *  the card encoded from it. Measured on red text over cyan (OgCardBaseLayerSpec): 4:4:4 is 356 KB
   *  at a mean error of 3.1 per pixel, the default 4:2:0 199 KB at 19.8 — a real card's base, mostly
   *  poster and dark gradient, is far smaller than that worst case. */
  def encodeBase(img: BufferedImage): Array[Byte] = toJpeg(img, BaseQuality, fullChroma = true)

  private val BaseQuality = 0.95f

  /** Test seam: an encode at any quality and chroma sampling, for the measurement behind [[encodeBase]]. */
  private[tools] def encodeWith(img: BufferedImage, quality: Float, fullChroma: Boolean): Array[Byte] = toJpeg(img, quality, fullChroma)

  private def applyHints(g: Graphics2D): Unit = {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
  }

  /** JPEG AND NOT PNG, BECAUSE A LOSSLESS PHOTOGRAPH IS HALF A MEGABYTE.
   *
   *  These cards are a full-bleed poster montage -- a photograph, with a
   *  gradient behind it and a few hundred pixels of text on top. PNG stores
   *  that losslessly: measured on a real card, 785 KB and 30 ms to deflate,
   *  against 205 KB and 13 ms at this quality. Nothing about the card wants
   *  lossless -- there is no transparency (the canvas is TYPE_INT_RGB), no flat
   *  colour to keep crisp, and the consumers are Facebook, Slack and iMessage
   *  previews that re-encode it anyway.
   *
   *  THE SIZE IS A HEAP PROBLEM AND NOT ONLY A BANDWIDTH ONE. The web's card cache
   *  held rendered cards, so at 785 KB a crawler sweeping the share cards
   *  fills it with hundreds of megabytes of live byte arrays -- which is what
   *  happened on 2026-09-04, when the old-gen floor on web-uk went from 29% to
   *  71% of its cap within two hours of the sweep starting.
   *
   *  QUALITY 0.85, not the ImageIO default (0.75): the card carries small white
   *  text over a dark gradient, which is where JPEG's chroma subsampling shows
   *  first. 0.85 is the point at which that text stays clean; the difference
   *  from 0.75 costs about 40 KB. */
  private def toJpeg(img: BufferedImage, quality: Float = JpegQuality, fullChroma: Boolean = false): Array[Byte] = {
    val rgb  = opaque(img)
    val baos = new ByteArrayOutputStream()
    val writer = ImageIO.getImageWritersByFormatName("jpg").next()
    val stream = ImageIO.createImageOutputStream(baos)
    try {
      writer.setOutput(stream)
      val params = writer.getDefaultWriteParam
      params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
      params.setCompressionQuality(quality)
      val metadata = writer.getDefaultImageMetadata(javax.imageio.ImageTypeSpecifier.createFromRenderedImage(rgb), params)
      if (fullChroma) {
        // Every component at 1×1 sampling: the encoder's default halves chroma both ways (4:2:0).
        val tree = metadata.getAsTree("javax_imageio_jpeg_image_1.0")
        val sof  = tree.asInstanceOf[org.w3c.dom.Element].getElementsByTagName("sof").item(0).asInstanceOf[org.w3c.dom.Element]
        val specs = sof.getElementsByTagName("componentSpec")
        for (k <- 0 until specs.getLength) {
          val spec = specs.item(k).asInstanceOf[org.w3c.dom.Element]
          spec.setAttribute("HsamplingFactor", "1"); spec.setAttribute("VsamplingFactor", "1")
        }
        metadata.setFromTree("javax_imageio_jpeg_image_1.0", tree)
      }
      writer.write(null, new IIOImage(rgb, null, metadata), params)
    } finally {
      writer.dispose()
      stream.close()
    }
    baos.toByteArray
  }

  /** Cover-scale `p` to fill the (x, y, w, h) box, cropping the overflow
   *  (`object-fit: cover`), clipped to that box. */
  private def drawCover(g: Graphics2D, p: BufferedImage, x: Int, y: Int, w: Int, h: Int): Unit = {
    val prev  = g.getClip
    g.setClip(x, y, w, h)
    val scale = math.max(w.toDouble / p.getWidth, h.toDouble / p.getHeight)
    val sw    = math.round(p.getWidth * scale).toInt
    val sh    = math.round(p.getHeight * scale).toInt
    g.drawImage(p, x - (sw - w) / 2, y - (sh - h) / 2, sw, sh, null)
    g.setClip(prev)
  }

  /** `p` cover-scaled and cropped to exactly the film card's poster column
   *  ([[PosterSlotWidth]] × [[PosterSlotHeight]]) — what the worker's poster cache stores, so a
   *  render from the cache draws it 1:1. */
  def coverSlot(p: BufferedImage): BufferedImage = opaqueCopy(p, PosterSlotWidth, PosterSlotHeight)

  /** `p` as opaque RGB, the only colour model the JPEG encoder takes whole: transparency composited
   *  over the card's background, grayscale / palette / CMYK-derived pixels converted. A poster
   *  arrives in whatever model its site saved it in, and a PNG with alpha reaching the encoder
   *  untouched is what failed every render of 41 PL films on 2026-09-24 ("Bogus input colorspace"). */
  def opaque(p: BufferedImage): BufferedImage =
    if (p.getType == BufferedImage.TYPE_INT_RGB) p else opaqueCopy(p, p.getWidth, p.getHeight)

  /** `p` cover-scaled onto a new opaque RGB canvas of `w` × `h`, over the card's background. */
  private def opaqueCopy(p: BufferedImage, w: Int, h: Int): BufferedImage = {
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val g   = out.createGraphics()
    try {
      applyHints(g)
      g.setColor(Bg); g.fillRect(0, 0, w, h)
      drawCover(g, p, 0, 0, w, h)
    } finally g.dispose()
    out
  }

  /** Cover-scale the film poster to the full-bleed left column. */
  private def drawPoster(g: Graphics2D, p: BufferedImage): Unit =
    drawCover(g, p, 0, 0, PosterW, PosterH)

  /** A row (wrapping to a second row if needed) of two-segment rating badges.
   *  Each badge is filled segment-by-segment while clipped to its rounded outer
   *  rect, so the outer corners are rounded (radius like the web's 3px, scaled)
   *  and the label/value seam stays square — exactly the web pill.
   *
   *  Returns the y of the bottom edge of the last badge row, so the caller can
   *  place the director/synopsis directly beneath however many rows wrapped. */
  private def drawBadges(g: Graphics2D, badges: Seq[Badge], x0: Int, top: Int, xMax: Int, fontSize: Float = 30f,
                         paint: Boolean = true): Int = {
    val labelFont = bold.deriveFont(fontSize)
    val valueFont = regular.deriveFont(fontSize)
    def fontFor(s: Seg)  = if (s.bold) labelFont else valueFont
    // The 30f badge's paddings/gap/corner, scaled so a smaller font (the
    // in-card mini pills) keeps the same proportions.
    val scale = fontSize / 30f
    val padY  = math.round(11 * scale)
    val gap   = math.round(14 * scale)
    val arcD  = 16f * scale
    def padX(s: Seg) = math.round(s.padX * scale)
    // Uniform height across every badge, from the (taller) bold metrics.
    val refFm = g.getFontMetrics(labelFont)
    val height      = refFm.getAscent + refFm.getDescent + padY * 2
    var xPosition = x0
    var yPosition = top
    for (b <- badges) {
      val fms   = b.segs.map(s => g.getFontMetrics(fontFor(s)))
      val segW  = b.segs.zip(fms).map { case (s, fm) => fm.stringWidth(s.text) + padX(s) * 2 }
      val width     = segW.sum
      if (xPosition + width > xMax && xPosition > x0) { xPosition = x0; yPosition += height + math.round(12 * scale) }
      if (paint) {
      val outer = new RoundRectangle2D.Float(xPosition.toFloat, yPosition.toFloat, width.toFloat, height.toFloat, arcD, arcD)
      val saved = g.getClip
      g.setClip(outer) // rounds the outer corners; the per-segment fills keep a square seam
      var sx = xPosition
      for (((s, fm), sw) <- b.segs.zip(fms).zip(segW)) {
        g.setColor(s.bg)
        g.fillRect(sx, yPosition, sw, height)
        g.setColor(s.fg)
        g.setFont(fontFor(s))
        val ty = yPosition + (height - (fm.getAscent + fm.getDescent)) / 2 + fm.getAscent
        g.drawString(s.text, sx + padX(s), ty)
        sx += sw
      }
      g.setClip(saved)
      }
      xPosition += width + gap
    }
    yPosition + height
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
