package services.alerts

import models.{Opening, SeasonWindow}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackEvent, FallbackState}

import java.time.Instant

class FallbackAlertSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val state = FallbackState(
    cinema = "Kino Praha", active = true, fallbackSource = "Filmweb", fallbackRef = Some("2180"), since = Some(Instant.EPOCH),
    lastReason = Some("RuntimeException: down"), consecutiveFailures = 1,
    lastPrimaryProbeAt = None, nextPrimaryProbeAt = None, updatedAt = Instant.EPOCH, history = Nil,
    alerted = true   // the scraper set this on entering fallback (i.e. after the grace window)
  )

  private def event(kind: String, reason: String = "down") = FallbackEvent(Instant.EPOCH, kind, reason)

  "FallbackAlert" should "alert on ENTER with the cinema and reason once the spell is alerted" in {
    val message = FallbackAlert.messageFor(state, event(FallbackEvent.Enter, "HTTP 503"), Opening.YearRound).value
    message should include ("Kino Praha")
    message should include ("HTTP 503")
  }

  it should "alert on RECOVERED once the spell is alerted" in {
    FallbackAlert.messageFor(state, event(FallbackEvent.Recovered), Opening.YearRound).value should include ("recovered")
  }

  it should "stay quiet on PROBE_FAILED (routine backoff noise)" in {
    FallbackAlert.messageFor(state, event(FallbackEvent.ProbeFailed), Opening.YearRound) shouldBe None
  }

  it should "stay quiet on ENTER and RECOVERED for a grace-window spell that never entered fallback (alerted=false)" in {
    val unalerted = state.copy(alerted = false)
    FallbackAlert.messageFor(unalerted, event(FallbackEvent.Enter), Opening.YearRound) shouldBe None
    FallbackAlert.messageFor(unalerted, event(FallbackEvent.Recovered), Opening.YearRound) shouldBe None
  }

  // Nothing serving a drive-in in January is the drive-in's winter, not a venue that has gone.
  it should "not page UNCOVERED for a seasonal venue off its season, and page it in season" in {
    val driveIn  = Opening.Seasonal(SeasonWindow.UnitedStates)    // April..October
    val january  = FallbackEvent(Instant.parse("2027-01-15T12:00:00Z"), FallbackEvent.Uncovered, "HTTP 404")
    val july     = january.copy(at = Instant.parse("2026-07-15T12:00:00Z"))
    FallbackAlert.messageFor(state, january, Opening.YearRound).value should include ("nothing is covering it")
    FallbackAlert.messageFor(state, january, driveIn) shouldBe None
    FallbackAlert.messageFor(state, july, driveIn).value should include ("nothing is covering it")
  }
}
