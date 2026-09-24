package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

/**
 * A poster must never cost the worker CONTAINER more memory than the vips child's cap: a
 * progressive JPEG's decoder holds every coefficient of the WHOLE image (measured ~360 MB for
 * 8000×12000), and the child shares the container's cgroup with a JVM already at 700-850 MB of a
 * 960 MiB limit. The header says how big that buffer will be before anything decodes.
 */
class PosterMemoryCapSpec extends AnyFlatSpec with Matchers {

  /** A stand-in `vips` that records it ran and exits with `exit`. */
  private def fakeVips(exit: Int): (String, Path) = {
    val ran    = Files.createTempFile("vips-ran-", ".txt"); Files.delete(ran)
    val script = Files.createTempFile("fake-vips-", ".sh")
    Files.writeString(script, s"#!/bin/sh\ntouch '$ran'\nexit $exit\n")
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"))
    (script.toString, ran)
  }

  "The JPEG header" should "say progressive, the dimensions and the coefficient buffer a decode will hold" in {
    val giant = JpegHeader.read(PosterMemoryCapSpec.jpegHeader(8000, 12000, progressive = true)).get
    (giant.progressive, giant.width, giant.height) shouldBe ((true, 8000, 12000))
    // 4:2:0 — a full-size luma plane plus two quarter-size chroma planes, 2 bytes a coefficient.
    giant.coefficientBytes shouldBe (8000L * 12000 + 2 * 4000L * 6000) * 2
    val full = JpegHeader.read(PosterMemoryCapSpec.jpegHeader(8000, 12000, progressive = true, sampling = Seq(0x11, 0x11, 0x11))).get
    full.coefficientBytes shouldBe 8000L * 12000 * 3 * 2
    JpegHeader.read(PosterMemoryCapSpec.jpegHeader(780, 1144, progressive = false)).map(_.progressive) shouldBe Some(false)
    JpegHeader.read(Files.write(Files.createTempFile("png-", ".png"), Array[Byte](0x89.toByte, 'P', 'N', 'G'))) shouldBe None
  }

  "The progressive pre-check" should "admit an ordinary 11-megapixel poster, even at 4:4:4, and refuse what cannot fit" in {
    // Measured in the worker image under the 256 MB cap (2026-09-24): 2764×4096 4:4:4 (68 MB of
    // coefficients) and 3000×4500 4:4:4 (81 MB) fit; 3500×5250 4:4:4 (110 MB) needs 320 MB.
    val ordinary = JpegHeader.read(PosterMemoryCapSpec.jpegHeader(2764, 4096, progressive = true, sampling = Seq(0x11, 0x11, 0x11))).get
    ordinary.coefficientBytes should be <= PosterPipeline.ProgressiveCoefficientBudget
    JpegHeader.read(PosterMemoryCapSpec.jpegHeader(3000, 4500, progressive = true, sampling = Seq(0x11, 0x11, 0x11))).get
      .coefficientBytes should be <= PosterPipeline.ProgressiveCoefficientBudget
    JpegHeader.read(PosterMemoryCapSpec.jpegHeader(3500, 5250, progressive = true, sampling = Seq(0x11, 0x11, 0x11))).get
      .coefficientBytes should be > PosterPipeline.ProgressiveCoefficientBudget
  }

  it should "let a real 2764×4096 progressive poster render" in {
    val vips = VipsPosterShrinker.locate()
    assume(vips.isDefined, "vips is not installed")
    val dir = Files.createTempDirectory("progressive-")
    def run(args: String*): Unit = new ProcessBuilder((vips.get +: args)*).inheritIO().start().waitFor() shouldBe 0
    run("gaussnoise", dir.resolve("n.v").toString, "2764", "4096")
    run("cast", dir.resolve("n.v").toString, dir.resolve("u.v").toString, "uchar")
    run("jpegsave", dir.resolve("u.v").toString, dir.resolve("p.jpg").toString, "--interlace", "--Q", "85", "--subsample-mode", "off")
    new VipsPosterShrinker(binary = vips).coverSlot(dir.resolve("p.jpg")).map(i => (i.getWidth, i.getHeight)) shouldBe Right((420, 630))
  }

  "A giant progressive JPEG" should "be routed away by its header, never decoded" in {
    val (bin, ran) = fakeVips(exit = 0)
    new VipsPosterShrinker(binary = Some(bin)).coverSlot(PosterMemoryCapSpec.jpegHeader(8000, 12000, progressive = true)) shouldBe Left(PosterFailure.ProgressiveEstimate)
    Files.exists(ran) shouldBe false
  }

  it should "leave a normal-sized progressive poster to vips" in {
    val (bin, ran) = fakeVips(exit = 1)
    new VipsPosterShrinker(binary = Some(bin)).coverSlot(PosterMemoryCapSpec.jpegHeader(780, 1144, progressive = true))
    Files.exists(ran) shouldBe true
  }

  "A vips child that hits its cap" should "fail only itself: no decode of the same poster in the JVM" in {
    val (bin, ran) = fakeVips(exit = 134)                              // SIGABRT, as libjpeg's "Insufficient memory"
    val poster = Files.write(Files.createTempFile("poster-", ".jpg"), posterJpeg)
    new VipsPosterShrinker(binary = Some(bin)).coverSlot(poster) shouldBe Left(PosterFailure.VipsCap)
    Files.exists(ran) shouldBe true
  }

  "A poster the whole chain refuses" should "degrade the card to no poster, counted as a failed fetch, trying smaller renditions first" in {
    val tried = collection.mutable.Buffer.empty[String]
    val giant = PosterMemoryCapSpec.jpegHeader(8000, 12000, progressive = true)
    val download = new PosterDownload {
      def fetch(url: String): Either[String, Path] = { tried += url; Right(Files.copy(giant, Files.createTempFile("dl-", ".jpg"), java.nio.file.StandardCopyOption.REPLACE_EXISTING)) }
    }
    val store = tempStore()
    val posters = new ShareCardPosters(store, download, new VipsPosterShrinker(binary = None), ShareCardMetrics.noop)
    posters.load("f1", Seq("https://m.media-amazon.com/images/M/abc@._V1_.jpg")) shouldBe None
    tried.toSeq shouldBe Seq(
      "https://m.media-amazon.com/images/M/abc@._V1_SX780.jpg",
      "https://m.media-amazon.com/images/M/abc@._V1_SX500.jpg",
      "https://m.media-amazon.com/images/M/abc@._V1_.jpg")
  }

  "Poster renditions" should "ask TMDB and IMDb's Amazon CDN for a card-sized copy before the original" in {
    PosterRendition.candidates("https://image.tmdb.org/t/p/original/abc.jpg") shouldBe Seq(
      "https://image.tmdb.org/t/p/w780/abc.jpg", "https://image.tmdb.org/t/p/w500/abc.jpg", "https://image.tmdb.org/t/p/original/abc.jpg")
    PosterRendition.candidates("https://image.tmdb.org/t/p/w500/abc.jpg") shouldBe Seq("https://image.tmdb.org/t/p/w500/abc.jpg")
    PosterRendition.candidates("https://m.media-amazon.com/images/M/abc@._V1_.jpg").head shouldBe
      "https://m.media-amazon.com/images/M/abc@._V1_SX780.jpg"
    PosterRendition.candidates("https://cdn.example/a.jpg") shouldBe Seq("https://cdn.example/a.jpg")
  }
}

object PosterMemoryCapSpec {
  /** SOI + one SOF segment, nothing else: a header-only "poster" of any declared size. */
  def jpegHeader(width: Int, height: Int, progressive: Boolean, sampling: Seq[Int] = Seq(0x22, 0x11, 0x11)): Path = {
    val out = new ByteArrayOutputStream()
    def u16(v: Int): Unit = { out.write(v >> 8); out.write(v & 0xff) }
    out.write(0xff); out.write(0xd8)                                   // SOI
    out.write(0xff); out.write(0xe0); u16(16); out.write("JFIF\u0000".getBytes("US-ASCII")); out.write(Array[Byte](1, 1, 0, 0, 1, 0, 1, 0, 0)) // APP0
    out.write(0xff); out.write(if (progressive) 0xc2 else 0xc0)        // SOF2 / SOF0
    u16(8 + 3 * sampling.size); out.write(8); u16(height); u16(width); out.write(sampling.size)
    sampling.zipWithIndex.foreach { case (s, i) => out.write(i + 1); out.write(s); out.write(0) }
    Files.write(Files.createTempFile("header-", ".jpg"), out.toByteArray)
  }

}
