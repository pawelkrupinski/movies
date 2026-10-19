package services.alerts

import models.{Opening, SeasonWindow}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackEvent, FallbackState}

import java.time.Instant

class FallbackAlertSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val entered = Instant.parse("2026-10-18T00:00:00Z")
  private def at(hours: Long) = entered.plusSeconds(hours * 3600)

  private def event(kind: String, hoursIn: Long = 0, reason: String = "down") = FallbackEvent(at(hoursIn), kind, reason)

  /** The state a transition hands the alert: its own event at the head of the spell's history. */
  private def stateWith(history: FallbackEvent*) = FallbackState(
    cinema = "Kino Praha", active = true, fallbackSource = "Filmweb", fallbackRef = Some("2180"), since = Some(entered),
    lastReason = Some("RuntimeException: down"), consecutiveFailures = 1,
    lastPrimaryProbeAt = None, nextPrimaryProbeAt = None, updatedAt = entered, history = history.toList
  )

  private def page(history: FallbackEvent*) = FallbackAlert.messageFor(stateWith(history*), history.head, Opening.YearRound)

  private val enter = event(FallbackEvent.Enter, reason = "HTTP 503")

  "FallbackAlert" should "stay quiet on ENTER: the fallback is serving the venue" in {
    page(enter) shouldBe None
  }

  it should "page the re-probe that finds the venue a day on fallback, with the cinema and reason" in {
    val probe23 = event(FallbackEvent.ProbeFailed, 23, "HTTP 503")
    val probe24 = event(FallbackEvent.ProbeFailed, 24, "HTTP 503")
    page(probe23, enter) shouldBe None
    val message = page(probe24, probe23, enter).value
    message should include ("Kino Praha")
    message should include ("serving via Filmweb fallback for 24h")
    message should include ("HTTP 503")
    page(event(FallbackEvent.ProbeFailed, 25), probe24, probe23, enter) shouldBe None   // only the crossing pages
  }

  it should "page RECOVERED only for a spell that lasted long enough to have paged" in {
    page(event(FallbackEvent.Recovered, 5), event(FallbackEvent.ProbeFailed, 4), enter) shouldBe None
    page(event(FallbackEvent.Recovered, 30), event(FallbackEvent.ProbeFailed, 29), enter).value should include ("recovered")
  }

  it should "count the day-long page as the spell's ENTER page, so its kind is the one the metrics seed" in {
    FallbackAlert.kindOf(event(FallbackEvent.ProbeFailed, 24)) shouldBe TelegramAlertKind.fallback(FallbackEvent.Enter)
    FallbackAlert.kindOf(event(FallbackEvent.Recovered, 30)) shouldBe TelegramAlertKind.fallback(FallbackEvent.Recovered)
  }

  it should "stay quiet on a PROBE_FAILED inside the first day (routine backoff noise)" in {
    page(event(FallbackEvent.ProbeFailed, 2), enter) shouldBe None
  }

  // Nothing serving a drive-in in January is the drive-in's winter, not a venue that has gone.
  it should "not page UNCOVERED for a seasonal venue off its season, and page it in season" in {
    val driveIn  = Opening.Seasonal(SeasonWindow.UnitedStates)    // April..October
    val january  = FallbackEvent(Instant.parse("2027-01-15T12:00:00Z"), FallbackEvent.Uncovered, "HTTP 404")
    val july     = january.copy(at = Instant.parse("2026-07-15T12:00:00Z"))
    FallbackAlert.messageFor(stateWith(january), january, Opening.YearRound).value should include ("nothing is covering it")
    FallbackAlert.messageFor(stateWith(january), january, driveIn) shouldBe None
    FallbackAlert.messageFor(stateWith(july), july, driveIn).value should include ("nothing is covering it")
  }
}
