package services.sharecards

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage
import java.nio.file.Path

/** The cases of [[PosterColourSpaceSpec]] that render through a real vips child, or need one to make the CMYK fixture. */
class PosterColourSpaceIntegrationSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  import PosterColourSpaceSpec.{file, image, posters, rendersThrough}

  /** A CMYK JPEG, made by vips (the JDK cannot write one). */
  private def cmykJpeg(vips: String): Path = {
    val dir = tempDirectory("cmyk-")
    val png = file(image(BufferedImage.TYPE_INT_RGB), "png")
    def run(args: String*): Unit = new ProcessBuilder((vips +: args)*).inheritIO().start().waitFor() shouldBe 0
    run("colourspace", png.toString, dir.resolve("c.v").toString, "cmyk")
    run("jpegsave", dir.resolve("c.v").toString, dir.resolve("cmyk.jpg").toString)
    dir.resolve("cmyk.jpg")
  }

  for ((what, poster) <- posters) {
    s"$what" should "render through vips" in {
      val vips = VipsPosterShrinker.locate(settings.ProcessConfiguration.resolve().executableSearchPath)
      assume(vips.isDefined, "vips is not installed")
      rendersThrough(new VipsPosterShrinker(binary = vips), poster)
    }
  }

  "A CMYK JPEG" should "render through vips" in {
    val vips = VipsPosterShrinker.locate(settings.ProcessConfiguration.resolve().executableSearchPath)
    assume(vips.isDefined, "vips is not installed")
    rendersThrough(new VipsPosterShrinker(binary = vips), () => cmykJpeg(vips.get.value))
  }

  it should "render through the JDK decode" in {
    val vips = VipsPosterShrinker.locate(settings.ProcessConfiguration.resolve().executableSearchPath)
    assume(vips.isDefined, "vips is needed to make the CMYK fixture")
    val cmyk = cmykJpeg(vips.get.value)
    rendersThrough(new VipsPosterShrinker(binary = None), () => cmyk)
  }
}
