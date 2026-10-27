package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The case of [[AotCacheOptionsSpec]] that runs a real `java`: each tier's baked launcher options must start a JVM. */
class AotCacheOptionsIntegrationSpec extends AnyFlatSpec with Matchers {

  LauncherOptions.files.keys.toSeq.sorted.foreach { tier =>
    // ArchiveRelocationMode is diagnostic: a launcher that names it before -XX:+UnlockDiagnosticVMOptions
    // does not start at all. Starting a JVM under exactly the baked options, in their order, is the check
    // a pod would otherwise make.
    s"the $tier launcher" should "start a JVM under exactly the options it bakes, in order" in {
      val java    = ProcessHandle.current().info().command().orElseThrow() // this JVM's own `java`
      val process = new ProcessBuilder((java +: LauncherOptions.of(tier) :+ "-version")*).redirectErrorStream(true).start()
      val output  = new String(process.getInputStream.readAllBytes())
      withClue(s"infra/jvm options for $tier:\n$output")(process.waitFor() shouldBe 0)
    }
  }
}
