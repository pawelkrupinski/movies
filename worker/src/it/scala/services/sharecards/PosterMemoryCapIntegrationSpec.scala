package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.nio.file.Files

/** The cases of [[PosterMemoryCapSpec]] that run a real vips child (or a stand-in script as one). */
class PosterMemoryCapIntegrationSpec extends AnyFlatSpec with Matchers {

  "The progressive pre-check" should "let a real 2764×4096 progressive poster render" in {
    val vips = VipsPosterShrinker.locate(settings.ProcessConfiguration.resolve().executableSearchPath)
    assume(vips.isDefined, "vips is not installed")
    val dir = Files.createTempDirectory("progressive-")
    def run(args: String*): Unit = new ProcessBuilder((vips.get.value +: args)*).inheritIO().start().waitFor() shouldBe 0
    run("gaussnoise", dir.resolve("n.v").toString, "2764", "4096")
    run("cast", dir.resolve("n.v").toString, dir.resolve("u.v").toString, "uchar")
    run("jpegsave", dir.resolve("u.v").toString, dir.resolve("p.jpg").toString, "--interlace", "--Q", "85", "--subsample-mode", "off")
    new VipsPosterShrinker(binary = vips).coverSlot(dir.resolve("p.jpg")).map(i => (i.getWidth, i.getHeight)) shouldBe Right((420, 630))
  }

  "A giant progressive JPEG" should "leave a normal-sized progressive poster to vips" in {
    val (bin, ran) = PosterMemoryCapSpec.fakeVips(exit = 1)
    new VipsPosterShrinker(binary = Some(VipsPosterShrinker.Binary(bin))).coverSlot(PosterMemoryCapSpec.jpegHeader(780, 1144, progressive = true))
    Files.exists(ran) shouldBe true
  }

  "A vips child that hits its cap" should "fail only itself: no decode of the same poster in the JVM" in {
    val (bin, ran) = PosterMemoryCapSpec.fakeVips(exit = 134)                              // SIGABRT, as libjpeg's "Insufficient memory"
    val poster = Files.write(Files.createTempFile("poster-", ".jpg"), posterJpeg)
    new VipsPosterShrinker(binary = Some(VipsPosterShrinker.Binary(bin))).coverSlot(poster) shouldBe Left(PosterFailure.VipsCap)
    Files.exists(ran) shouldBe true
  }
}
