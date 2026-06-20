package tools

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{Color, Font, GradientPaint, Graphics2D, LinearGradientPaint, RenderingHints}
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

  private val Bg          = new Color(0x14, 0x17, 0x1f)
  private val BgBottom    = new Color(0x09, 0x0a, 0x0f)
  private val TitleCol    = Color.WHITE
  private val SubCol      = new Color(0x9a, 0xa3, 0xb2)
  private val SynopsisCol = new Color(0xc2, 0xca, 0xd6) // a touch brighter than SubCol so the body copy reads
  private val FooterCol   = new Color(0x70, 0x78, 0x86)
  private val UrlCol      = new Color(0xbd, 0xa4, 0xff) // the city card's accent "kinowo.fly.dev" line

  // ── City page-like card: a grid of repertoire `.card`s as the background.
  //    Colours/metrics copied from the live page CSS so it reads as the site. ──
  private val GridCols   = 5
  private val GridSide   = 24            // page container px-4 ≈ 1.5rem
  private val GridGap    = 16            // bootstrap g-3 ≈ 1rem
  private val GridTop    = 22
  private val ColW       = (Width - 2 * GridSide - (GridCols - 1) * GridGap) / GridCols // ≈ 217
  private val PosterColH = math.round(ColW * 1.48f)  // page .poster-wrap padding-top:148%
  private val PageBg       = new Color(0x0d, 0x0d, 0x22) // page navy background
  private val CardBg       = new Color(0x1e, 0x1e, 0x2e) // .card
  private val PosterBg     = new Color(0x2a, 0x2a, 0x3e) // .poster-wrap fallback
  private val MetaPillBg     = new Color(0x25, 0x25, 0x40) // .pill (runtime/year/genre)
  private val MetaPillBorder = new Color(0x3a, 0x3a, 0x6e)
  private val MetaPillText   = new Color(0x99, 0x99, 0xbb)
  private val DayCol       = new Color(0x9a, 0x9a, 0xc0) // date-group label
  private val CinemaCol    = new Color(0x66, 0xaa, 0xdd) // .cinema-label
  private val ChipBg       = new Color(0x3a, 0x3a, 0x6e) // .badge-time
  private val ChipText     = new Color(0xaa, 0xd4, 0xff)
  private val ChipFmt      = new Color(0xaa, 0xd4, 0xff, 0xb3) // .badge-fmt (opacity ~.7)

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

  // Decorative IMDb·Metacritic·RT·Filmweb pills for the city card's brand
  // overlay — the card advertises that we carry ratings, not any one score.
  private val OverlayBadges = ratingBadges(imdb = Some(7.8), metascore = Some(81), rottenTomatoes = Some(91), filmweb = Some(7.4))

  /** Compose the card. `subtitle` is the year · genres line; `badges` are the
   *  rating pills from [[ratingBadges]]; `poster` is the decoded poster image or
   *  None (text-only card for films with no poster). `director` (a pre-joined
   *  "Name, Name" string) and `synopsis` fill the space below the ratings —
   *  both optional, each omitted when absent. */
  def render(title: String, subtitle: String, badges: Seq[Badge], poster: Option[BufferedImage],
             director: Option[String] = None, synopsis: Option[String] = None): Array[Byte] = {
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

      var yPosition = Margin + 12

      g.setFont(bold.deriveFont(60f))
      g.setColor(TitleCol)
      val titleFm = g.getFontMetrics
      for (line <- wrap(g, title, textW, maxLines = 3)) {
        yPosition += titleFm.getAscent
        g.drawString(line, textLeft, yPosition)
        yPosition += titleFm.getDescent + 4
      }

      if (subtitle.nonEmpty) {
        yPosition += 14
        g.setFont(regular.deriveFont(32f))
        g.setColor(SubCol)
        val sfm = g.getFontMetrics
        yPosition += sfm.getAscent
        g.drawString(ellipsize(g, subtitle, textW), textLeft, yPosition)
        yPosition += sfm.getDescent
      }

      if (badges.nonEmpty) {
        yPosition += 36
        yPosition = drawBadges(g, badges, textLeft, yPosition, textRight)
      }

      val footerBaseline = Height - Margin
      // Keep the body copy just clear of the footer line (~its ascent).
      val bodyBottom = footerBaseline - 30

      director.map(_.trim).filter(_.nonEmpty).foreach { d =>
        yPosition += 26
        g.setFont(regular.deriveFont(27f))
        g.setColor(SubCol)
        val dfm = g.getFontMetrics
        yPosition += dfm.getAscent
        g.drawString(ellipsize(g, "Reżyseria: " + d, textW), textLeft, yPosition)
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
      val footer = "kinowo.fly.dev"
      g.drawString(footer, textRight - ffm.stringWidth(footer), footerBaseline)
    } finally g.dispose()

    toPng(img)
  }

  /** Compose the per-city share card so its background reads as the real
   *  repertoire page: a 5-column grid of film cards (poster on top, then title,
   *  meta + rating pills, the day + per-cinema showtime chips) on the page's
   *  dark navy, with the left→right dark gradient + "Kinowo / {cityLine}" brand
   *  overlay on the left. `columns` are the city's first few DISTINCT films
   *  (no movie/poster repeats), each paired with its decoded poster (or None).
   *  No films → a clean brand-only card rather than an empty frame. */
  def renderCityPageCard(cityLine: String, columns: Seq[(CityCardFilm, Option[BufferedImage])]): Array[Byte] = {
    val img = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_RGB)
    val g   = img.createGraphics()
    try {
      applyHints(g)
      g.setColor(PageBg)
      g.fillRect(0, 0, Width, Height)

      for (((film, poster), i) <- columns.zipWithIndex.take(GridCols))
        drawColumn(g, GridSide + i * (ColW + GridGap), film, poster)

      // Left→right dark gradient: opaque on the left so the brand text reads,
      // fading out by ~64% so the page grid shows on the right.
      val stops  = Array(0f, 0.30f, 0.64f, 1f)
      val shades = Array(new Color(13, 13, 34, 247), new Color(13, 13, 34, 235),
                         new Color(13, 13, 34, 77),  new Color(13, 13, 34, 0))
      g.setPaint(new LinearGradientPaint(0f, 0f, Width.toFloat, 0f, stops, shades))
      g.fillRect(0, 0, Width, Height)

      drawBrandOverlay(g, cityLine)
    } finally g.dispose()

    toPng(img)
  }

  /** The left brand block: "Kinowo" wordmark + the city line + decorative rating
   *  pills + the URL, vertically centred. */
  private def drawBrandOverlay(g: Graphics2D, cityLine: String): Unit = {
    val leftX   = 80
    val brandFm = g.getFontMetrics(bold.deriveFont(86f))
    val tagFm   = g.getFontMetrics(regular.deriveFont(33f))
    val urlFm   = g.getFontMetrics(regular.deriveFont(23f))
    val pillFm  = g.getFontMetrics(bold.deriveFont(30f))
    val pillH   = pillFm.getAscent + pillFm.getDescent + 22 // padY*2, matching drawBadges at 30f
    val (gapBrandTag, gapTagPills, gapPillsUrl) = (14, 30, 26)
    val blockH = (brandFm.getAscent + brandFm.getDescent) + gapBrandTag +
                 (tagFm.getAscent + tagFm.getDescent) + gapTagPills + pillH + gapPillsUrl +
                 (urlFm.getAscent + urlFm.getDescent)
    var y = (Height - blockH) / 2

    g.setFont(bold.deriveFont(86f)); g.setColor(TitleCol)
    y += brandFm.getAscent; g.drawString("Kinowo", leftX, y); y += brandFm.getDescent

    y += gapBrandTag
    g.setFont(regular.deriveFont(33f)); g.setColor(TitleCol)
    y += tagFm.getAscent
    g.drawString(ellipsize(g, cityLine, Width - Margin - leftX), leftX, y)
    y += tagFm.getDescent

    y += gapTagPills
    y = drawBadges(g, OverlayBadges, leftX, y, Width - Margin)

    y += gapPillsUrl
    g.setFont(regular.deriveFont(23f)); g.setColor(UrlCol)
    y += urlFm.getAscent; g.drawString("kinowo.fly.dev", leftX, y)
  }

  /** One film column, drawn like the page's `.card`: rounded `#1e1e2e` panel,
   *  poster cover-filling the rounded top, then title / meta pills / rating pills
   *  / day / per-cinema time chips. The panel runs past the canvas bottom so it
   *  reads as a scrolled, cropped page rather than a floating card. */
  private def drawColumn(g: Graphics2D, x: Int, film: CityCardFilm, poster: Option[BufferedImage]): Unit = {
    val top  = GridTop
    val card = new RoundRectangle2D.Float(x.toFloat, top.toFloat, ColW.toFloat, (Height - top + 80).toFloat, 12f, 12f)
    g.setColor(CardBg); g.fill(card)

    poster match {
      case Some(p) =>
        val saved = g.getClip
        g.setClip(card); g.clipRect(x, top, ColW, PosterColH) // intersect → rounded top corners
        drawCoverInto(g, p, x, top, ColW, PosterColH)
        g.setClip(saved)
      case None =>
        g.setColor(PosterBg); g.fillRect(x, top, ColW, PosterColH)
    }

    val bx = x + 12
    val bw = ColW - 24
    val rightEdge = x + ColW - 12
    var y = top + PosterColH + 12

    g.setFont(bold.deriveFont(17f)); g.setColor(TitleCol)
    val tfm = g.getFontMetrics
    for (line <- wrap(g, film.title, bw, maxLines = 2)) {
      y += tfm.getAscent; g.drawString(line, bx, y); y += tfm.getDescent + 2
    }

    if (film.meta.nonEmpty) { y += 6; y = drawMetaPills(g, film.meta, bx, y, rightEdge) }
    if (film.badges.nonEmpty) { y += 7; y = drawBadges(g, film.badges, bx, y, rightEdge, fontSize = 14f) }

    if (film.dayLabel.nonEmpty && y < Height) {
      y += 12; g.setFont(regular.deriveFont(12.5f)); g.setColor(DayCol)
      val dfm = g.getFontMetrics; y += dfm.getAscent; g.drawString(ellipsize(g, film.dayLabel, bw), bx, y); y += dfm.getDescent
    }
    for ((cinema, chips) <- film.showings if y < Height) {
      y += 8; g.setFont(regular.deriveFont(11.5f)); g.setColor(CinemaCol)
      val cfm = g.getFontMetrics; y += cfm.getAscent; g.drawString(ellipsize(g, cinema, bw), bx, y); y += cfm.getDescent
      y += 4; y = drawTimeChips(g, chips, bx, y, rightEdge)
    }
  }

  /** A wrapping row of the page's small grey meta pills (runtime / year / genre):
   *  `#252540` fill, `#3a3a6e` 1px border, `#99b` text. Returns the bottom y. */
  private def drawMetaPills(g: Graphics2D, pills: Seq[String], x0: Int, top: Int, rightEdge: Int): Int = {
    g.setFont(regular.deriveFont(12.5f))
    val fm = g.getFontMetrics
    val (padX, padY, gap) = (6, 3, 5)
    val h = fm.getAscent + fm.getDescent + padY * 2
    var x = x0; var y = top
    for (p <- pills) {
      val w = fm.stringWidth(p) + padX * 2
      if (x + w > rightEdge && x > x0) { x = x0; y += h + 4 }
      g.setColor(MetaPillBg); g.fillRoundRect(x, y, w, h, 4, 4)
      g.setColor(MetaPillBorder); g.drawRoundRect(x, y, w - 1, h - 1, 4, 4)
      g.setColor(MetaPillText)
      g.drawString(p, x + padX, y + padY + fm.getAscent)
      x += w + gap
    }
    y + h
  }

  /** A wrapping row of the page's showtime chips: `#3a3a6e` fill, `#aad4ff` time
   *  text, the format tag (e.g. "DUB") dimmer alongside. Chip label is
   *  "HH:MM[ FORMAT…]". Returns the bottom y. */
  private def drawTimeChips(g: Graphics2D, chips: Seq[String], x0: Int, top: Int, rightEdge: Int): Int = {
    val font    = regular.deriveFont(13.5f)
    val fmtFont = regular.deriveFont(10.5f)
    val fm  = g.getFontMetrics(font)
    val ffm = g.getFontMetrics(fmtFont)
    val (padX, padY, gap) = (7, 4, 5)
    val h = fm.getAscent + fm.getDescent + padY * 2
    var x = x0; var y = top
    for (chip <- chips if y + h <= Height + 40) {
      val parts = chip.split(" ", 2)
      val time  = parts(0)
      val fmt   = if (parts.length > 1) parts(1) else ""
      val timeW = fm.stringWidth(time)
      val fmtW  = if (fmt.nonEmpty) ffm.stringWidth(fmt) + 4 else 0
      val w     = timeW + fmtW + padX * 2
      if (x + w > rightEdge && x > x0) { x = x0; y += h + 4 }
      g.setColor(ChipBg); g.fillRoundRect(x, y, w, h, 6, 6)
      val ty = y + padY + fm.getAscent
      g.setColor(ChipText); g.setFont(font); g.drawString(time, x + padX, ty)
      if (fmt.nonEmpty) { g.setColor(ChipFmt); g.setFont(fmtFont); g.drawString(fmt, x + padX + timeW + 4, ty) }
      x += w + gap
    }
    y + h
  }

  private def applyHints(g: Graphics2D): Unit = {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
  }

  private def toPng(img: BufferedImage): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    ImageIO.write(img, "png", baos)
    baos.toByteArray
  }

  /** Cover-scale `p` to fill the (x, y, w, h) box, cropping the overflow
   *  (`object-fit: cover`), clipped to that box. */
  private def drawCover(g: Graphics2D, p: BufferedImage, x: Int, y: Int, w: Int, h: Int): Unit = {
    val prev = g.getClip
    g.setClip(x, y, w, h)
    drawCoverInto(g, p, x, y, w, h)
    g.setClip(prev)
  }

  /** Cover-scale `p` into the box WITHOUT touching the clip — the caller has
   *  already set a (possibly rounded) clip. */
  private def drawCoverInto(g: Graphics2D, p: BufferedImage, x: Int, y: Int, w: Int, h: Int): Unit = {
    val scale = math.max(w.toDouble / p.getWidth, h.toDouble / p.getHeight)
    val sw    = math.round(p.getWidth * scale).toInt
    val sh    = math.round(p.getHeight * scale).toInt
    g.drawImage(p, x - (sw - w) / 2, y - (sh - h) / 2, sw, sh, null)
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
  private def drawBadges(g: Graphics2D, badges: Seq[Badge], x0: Int, top: Int, xMax: Int, fontSize: Float = 30f): Int = {
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
