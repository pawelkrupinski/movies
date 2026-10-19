package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.GraphicsEnvironment

/** What every unit-test JVM must be pinned to. Each module's unit specs run in a JVM of their own, forked by sbt:
 *  project/ForkedTests.scala pins worker's (and every other unit module's), build.sbt pins web's (Play forks it). */
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

  // A spec run inside the kept sbt server leaves there whatever it does not release: a scheduler a wiring started
  // pins that run's whole test classloader. Measured 2026-10-19: every worker/Test/test grew the server by ~260 MB of
  // live heap and ~200 MB of metaspace, until a later testUnit in the same server died ("sbt server disconnected").
  // A forked JVM ends with its run. In the server, specs are loaded by one of sbt's own classloaders; in a fork, by
  // the JVM's application classloader.
  it should "be a test JVM of its own, never the sbt server" in {
    getClass.getClassLoader.getClass.getName should not startWith "sbt."
  }
}
