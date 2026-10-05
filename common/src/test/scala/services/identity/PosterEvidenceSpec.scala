package services.identity

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage

/** A poster's hash tells two prints of one poster from two posters, and the cluster's venue posters vote for the one
 *  candidate they match and veto a film when they match another. */
class PosterEvidenceSpec extends AnyFlatSpec with Matchers {

  /** A synthetic poster: blocks of grey whose layout `seed` picks, at `w` × `h`. */
  private def poster(seed: Int, w: Int = 300, h: Int = 450): BufferedImage = {
    val rnd   = new scala.util.Random(seed)
    val cells = Array.fill(6, 4)(rnd.nextInt(256))
    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for (y <- 0 until h; x <- 0 until w) {
      val g = cells(y * 6 / h)(x * 4 / w)
      image.setRGB(x, y, (g << 16) | (g << 8) | g)
    }
    image
  }

  /** `image` resampled to `w` × `h`, as a CDN serves a smaller print. */
  private def resized(image: BufferedImage, w: Int, h: Int): BufferedImage = {
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val g   = out.createGraphics()
    try {
      g.setRenderingHint(java.awt.RenderingHints.KEY_INTERPOLATION, java.awt.RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g.drawImage(image, 0, 0, w, h, null)
    } finally g.dispose()
    out
  }

  "a poster's hash" should "barely move between two prints of one poster" in {
    val original = poster(1, 600, 900)
    PosterHash.of(original).distance(PosterHash.of(resized(original, 185, 278))) should be <= 2
  }

  it should "stand far apart for two different posters" in {
    PosterHash.of(poster(1)).distance(PosterHash.of(poster(2))) should be > 16
  }

  it should "read a landscape banner by its centred 2:3 portrait" in {
    val portrait = poster(3)
    val banner   = new BufferedImage(900, 450, BufferedImage.TYPE_INT_RGB)
    val g = banner.createGraphics()
    try g.drawImage(portrait, 300, 0, null) finally g.dispose()
    PosterHash.of(banner).distance(PosterHash.of(portrait)) should be <= 2
  }

  "the posters' vote" should "take the one candidate within the vote's bits" in {
    PosterEvidence.vote(Map(1 -> Some(2), 2 -> Some(30), 3 -> None)) shouldBe Some(1 -> 2)
  }

  it should "take none when two candidates are as near, or none is" in {
    PosterEvidence.vote(Map(1 -> Some(2), 2 -> Some(4))) shouldBe None
    PosterEvidence.vote(Map(1 -> Some(6), 2 -> Some(30))) shouldBe None
  }

  "the posters' veto" should "refuse a film far from the venue's poster when it matches another candidate" in {
    PosterEvidence.veto(Some(1), Map(1 -> Some(28), 2 -> Some(3))) shouldBe Some(2 -> 3)
    PosterEvidence.veto(Some(1), Map(1 -> Some(30), 2 -> Some(PosterEvidence.VetoMatchBits))) shouldBe Some(2 -> PosterEvidence.VetoMatchBits)
    PosterEvidence.veto(None, Map(2 -> Some(3))) shouldBe Some(2 -> 3)
    PosterEvidence.veto(Some(1), Map(1 -> None, 2 -> Some(0))) shouldBe Some(2 -> 0)
  }

  it should "refuse nothing by distance alone, nor a film near the venue's poster" in {
    PosterEvidence.veto(Some(1), Map(1 -> Some(32), 2 -> Some(PosterEvidence.VetoMatchBits + 2))) shouldBe None
    PosterEvidence.veto(Some(1), Map(1 -> Some(PosterEvidence.VetoBits), 2 -> Some(2))) shouldBe None
  }

  "a listing's poster" should "speak for its film, but not a stage relay's nor a double bill's" in {
    def billed(title: String) = FilmTable.listing(Multikino, title).copy(poster = Some(s"https://posters/$title.jpg"))
    PosterEvidence.urls(Seq(billed("Lalka"), billed("The Royal Ballet: The Nutcracker"), billed("Psychoza + Ptaki"))) shouldBe
      Seq("https://posters/Lalka.jpg")
    PosterEvidence.urls(Seq(FilmTable.listing(KinoMuza, "Lalka"))) shouldBe empty
  }
}
