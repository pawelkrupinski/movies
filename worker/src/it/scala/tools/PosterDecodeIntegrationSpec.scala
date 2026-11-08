package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The case of [[PosterDecodeSpec]] that runs a child JVM (its `CountingReaderRace`) for a fresh JVM's first use of ImageIO. */
class PosterDecodeIntegrationSpec extends AnyFlatSpec with Matchers {

  private lazy val harness = settings.HarnessConfiguration.resolve()

  // `sbt common/test`, 2026-09-25: both counting-reader tests failed in the full suite and passed
  // alone. IIORegistry.getDefaultInstance creates the JVM's registry lazily and WITHOUT a lock, so a
  // registration racing ImageIO's own first use (another suite's ImageIO.read, started in parallel)
  // could create a second registry that ImageIO never consults -- for the rest of that JVM. Only a
  // fresh JVM still has that first use ahead of it, so each attempt runs in one.
  "a counting reader" should "register where ImageIO looks, even racing another suite's first use of ImageIO" in {
    val outcomes = (1 to 5).map { _ =>
      val (exit, output) = ChildJvm(harness).run("tools.CountingReaderRace",jvmArgs = Seq("-Djava.awt.headless=true"))
      (exit, output.trim)
    }
    all(outcomes) shouldBe ((0, "VISIBLE"))
  }
}
