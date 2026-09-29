package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Both apps log through one logback.xml and the sentry-logback appender, so they have to run
 * one logback. The worker takes `logbackVersion` from project/Dependencies.scala directly; the
 * web app would take whatever play-logback brings unless it declares the same dependency --
 * and it once didn't: the worker sat on 1.5.22 while Play 3.0.11 gave the web app 1.5.32, and
 * Scala Steward could not raise the pin without splitting them further.
 */
class LogbackVersionParitySpec extends AnyFlatSpec with Matchers {
  private val Declared = """logbackVersion\s*=\s*"([^"]+)"""".r

  private lazy val declared: String =
    Declared
      .findFirstMatchIn(RepoFile.read(RepoFile.locate("project/Dependencies.scala")))
      .map(_.group(1))
      .getOrElse(fail("no logbackVersion in project/Dependencies.scala"))

  "the web app" should "run the logback version the worker is pinned to" in {
    classOf[ch.qos.logback.classic.LoggerContext].getPackage.getImplementationVersion shouldBe declared
  }
}
