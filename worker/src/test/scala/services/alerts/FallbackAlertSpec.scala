package services.alerts

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackEvent, FilmwebFallbackState}

import java.time.Instant

class FallbackAlertSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val state = FilmwebFallbackState(
    cinema = "Kino Praha", active = true, filmwebCinemaId = Some(2180), since = Some(Instant.EPOCH),
    lastReason = Some("RuntimeException: down"), consecutiveFailures = 1,
    lastPrimaryProbeAt = None, nextPrimaryProbeAt = None, updatedAt = Instant.EPOCH, history = Nil
  )

  private def event(kind: String, reason: String = "down") = FallbackEvent(Instant.EPOCH, kind, reason)

  "FallbackAlert" should "alert on ENTER with the cinema and reason" in {
    val msg = FallbackAlert.messageFor(state, event(FallbackEvent.Enter, "HTTP 503")).value
    msg should include ("Kino Praha")
    msg should include ("HTTP 503")
  }

  it should "alert on RECOVERED" in {
    FallbackAlert.messageFor(state, event(FallbackEvent.Recovered)).value should include ("recovered")
  }

  it should "stay quiet on PROBE_FAILED (routine backoff noise)" in {
    FallbackAlert.messageFor(state, event(FallbackEvent.ProbeFailed)) shouldBe None
  }
}
