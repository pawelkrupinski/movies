package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.LogCapture

/** The once-per-label report of an unrecognised screening label. Here rather than beside
 *  `ScreeningTokensSpec` because `common`'s tests cannot reach testkit's [[LogCapture]]. */
class ScreeningTokensLoggingSpec extends AnyFlatSpec with Matchers {

  private def reports(body: => Unit): Int =
    LogCapture.thisThread(classOf[ScreeningTokens].getName)(body).count(_.getFormattedMessage.contains("unrecognised"))

  "An unrecognised screening label" should "be reported once per vocabulary, not once per process" in {
    val label = s"Mystery Format ${java.util.UUID.randomUUID()}"
    reports {
      val first = ScreeningTokens.of(models.Country.UnitedKingdom)
      first.canonical(label); first.canonical(label)
      ScreeningTokens.of(models.Country.UnitedKingdom).canonical(label)
    } shouldBe 2
  }
}
