package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.GraphicsEnvironment

/** What every test JVM must be pinned to. `.jvmopts` pins the sbt JVM (worker
 *  specs and PageTest run unforked inside it) and build.sbt pins web's forked
 *  Test JVM; one spec per kind of JVM guards each. */
abstract class TestJvmContract extends AnyFlatSpec with Matchers {

  // The share-card and poster specs draw with AWT. On macOS a non-headless JVM
  // that touches AWT registers as a foreground app and steals focus, dragging
  // the user off whatever Space they are on whenever a test run reaches those
  // specs.
  "the test JVM" should "run AWT headless so rendering never grabs desktop focus" in {
    GraphicsEnvironment.isHeadless shouldBe true
  }

  // Twirl's `f"…%.1f"` and friends format with the JVM's default locale. Prod's
  // image (eclipse-temurin) sets `LANG`/`LC_ALL=en_US.UTF-8`, so it renders
  // "6.4"; a dev Mac's `en_PL` rendered "6,4", and the page snapshots flipped
  // between the two from one local run to the next. The pin matches prod.
  it should "format decimals the way production does" in {
    f"${6.4}%.1f" shouldBe "6.4"
  }
}
