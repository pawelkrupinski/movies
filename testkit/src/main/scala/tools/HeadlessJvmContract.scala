package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.GraphicsEnvironment

/** The share-card and poster specs draw with AWT. On macOS a non-headless JVM
 *  that touches AWT registers as a foreground app and steals focus, dragging
 *  the user off whatever Space they are on whenever a test run reaches those
 *  specs. `.jvmopts` pins the sbt JVM headless and build.sbt pins web's forked
 *  Test JVM; one spec per kind of JVM guards each. */
abstract class HeadlessJvmContract extends AnyFlatSpec with Matchers {

  "the test JVM" should "run AWT headless so rendering never grabs desktop focus" in {
    GraphicsEnvironment.isHeadless shouldBe true
  }
}
