package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

/**
 * The card as a BASE (everything but the rating badges, kept as a high-quality JPEG) plus the
 * badges drawn onto it: a ratings change then costs a decode and a few pills instead of a poster
 * and a full composite. That only works if the badges' place does not depend on anything the base
 * does not already know — and if base + badges is the same picture as the whole card drawn at once.
 */
class OgCardBaseLayerSpec extends AnyFlatSpec with Matchers {

  private def poster(): BufferedImage = {
    val img = new BufferedImage(420, 630, BufferedImage.TYPE_INT_RGB)
    val rnd = new scala.util.Random(7)
    for (x <- 0 until 420; y <- 0 until 630) img.setRGB(x, y, (rnd.nextInt(90) << 16) | ((80 + (x * y) % 120) << 8) | (y % 200))
    img
  }

  private val synopsis = (1 to 30).map(i => if (i % 2 == 0) "podróż" else "Bohaterka").mkString(" ")
  private val allFour  = OgCardRenderer.ratingBadges(Some(8.8), Some(74), Some(91), Some(7.9))

  private def base(title: String = "Diuna: Część pierwsza") =
    OgCardRenderer.renderBase(title, "2021 · Sci-Fi, Przygodowy", Some(poster()), "kinowo.net",
      Some("Reżyseria: Denis Villeneuve"), Some(synopsis))
  private def slot(title: String = "Diuna: Część pierwsza") =
    OgCardRenderer.badgeSlot(title, "2021 · Sci-Fi, Przygodowy", hasPoster = true)

  private def roundTrip(img: BufferedImage): BufferedImage =
    ImageIO.read(new ByteArrayInputStream(OgCardRenderer.encodeBase(img)))

  /** Mean SSIM over 8×8 windows of luminance, within `(x0, y0, w, h)`. */
  private def ssim(a: BufferedImage, b: BufferedImage, x0: Int = 0, y0: Int = 0, w: Int = 1200, h: Int = 630): Double = {
    def lum(img: BufferedImage, x: Int, y: Int) = { val c = new Color(img.getRGB(x, y)); 0.299 * c.getRed + 0.587 * c.getGreen + 0.114 * c.getBlue }
    val (c1, c2) = (6.5025, 58.5225)
    val scores = for (wx <- x0 until x0 + w - 7 by 8; wy <- y0 until y0 + h - 7 by 8) yield {
      val pa = for (x <- wx until wx + 8; y <- wy until wy + 8) yield lum(a, x, y)
      val pb = for (x <- wx until wx + 8; y <- wy until wy + 8) yield lum(b, x, y)
      val (ma, mb) = (pa.sum / 64, pb.sum / 64)
      val va  = pa.map(v => (v - ma) * (v - ma)).sum / 63
      val vb  = pb.map(v => (v - mb) * (v - mb)).sum / 63
      val cov = pa.zip(pb).map { case (x, y) => (x - ma) * (y - mb) }.sum / 63
      ((2 * ma * mb + c1) * (2 * cov + c2)) / ((ma * ma + mb * mb + c1) * (va + vb + c2))
    }
    scores.sum / scores.size
  }

  "The base's encoding" should "keep full-resolution chroma, which a colour edge needs" in {
    // Red text on cyan: the worst case for chroma subsampling, and the RT badge's colours.
    val img = new BufferedImage(1200, 630, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(new Color(0x00, 0xcc, 0xcc)); g.fillRect(0, 0, 1200, 630)
    g.setColor(new Color(0xfa, 0x32, 0x0a)); g.setFont(new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.BOLD, 28))
    for (row <- 0 until 18) g.drawString("RT 91% IMDb 8.8 Metacritic 74 FW 7.9", 20, 40 + row * 34)
    g.dispose()
    def error(bytes: Array[Byte]): Double = {
      val back = ImageIO.read(new ByteArrayInputStream(bytes))
      val diffs = for (x <- 0 until 1200 by 2; y <- 0 until 630 by 2) yield {
        val (a, b) = (new Color(img.getRGB(x, y)), new Color(back.getRGB(x, y)))
        math.abs(a.getRed - b.getRed) + math.abs(a.getGreen - b.getGreen) + math.abs(a.getBlue - b.getBlue)
      }
      diffs.sum.toDouble / diffs.size
    }
    val full = OgCardRenderer.encodeBase(img)
    val half = OgCardRenderer.encodeWith(img, 0.95f, fullChroma = false)
    info(f"q95 4:4:4: ${full.length / 1024} KB, mean error ${error(full)}%.2f; q95 4:2:0: ${half.length / 1024} KB, mean error ${error(half)}%.2f")
    error(full) should be < error(half) * 0.75
  }

  "A cached base plus the badges" should "be the same picture as the whole card drawn at once" in {
    val whole    = OgCardRenderer.renderImage("Diuna: Część pierwsza", "2021 · Sci-Fi, Przygodowy", allFour, Some(poster()),
      "kinowo.net", Some("Reżyseria: Denis Villeneuve"), Some(synopsis))
    val composed = OgCardRenderer.withBadges(roundTrip(base()), slot(), allFour)
    val s = slot()
    ssim(composed, whole) should be >= 0.98
    ssim(composed, whole, s.x, s.y, s.right - s.x, s.height) should be >= 0.98
  }

  "The badge area" should "not move the rest of the card: a base is the same with or without ratings" in {
    val without = OgCardRenderer.renderImage("Diuna: Część pierwsza", "2021 · Sci-Fi, Przygodowy", Nil, Some(poster()),
      "kinowo.net", Some("Reżyseria: Denis Villeneuve"), Some(synopsis))
    val s = slot()
    val rated = OgCardRenderer.withBadges(base(), s, allFour)
    // Everything below the reserved badge rows is identical whatever the ratings are.
    val moved = for (x <- 0 until 1200; y <- s.y + s.height until 630 if without.getRGB(x, y) != rated.getRGB(x, y)) yield (x, y)
    moved shouldBe empty
  }

  it should "sit below a longer title's extra lines, which the base's key covers" in {
    slot("Krótki").y should be < slot("Bardzo długi tytuł filmu który zawija się na drugą i trzecią linię").y
  }
}
