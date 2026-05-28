package controllers

import org.apache.pekko.http.scaladsl.model.{EntityStreamException, ErrorInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.http.HttpErrorConfig
import play.api.{Environment, Mode}

/**
 * Regression: `EntityStreamException` from a sendBeacon-truncated POST
 * to `/uptime/img-event` used to log at ERROR via Play's default error
 * handler and reach Sentry. The predicate now classifies those as
 * client-side truncation so the handler can route them to WARN.
 */
class TruncationTolerantHttpErrorHandlerSpec extends AnyFlatSpec with Matchers {

  private val handler = new TruncationTolerantHttpErrorHandler(
    Environment.simple(mode = Mode.Test),
    HttpErrorConfig(showDevErrors = false, playEditor = None),
    sourceMapper = None,
    router       = None
  )

  private def truncation(msg: String = "truncated"): EntityStreamException =
    EntityStreamException(new ErrorInfo(msg))

  "isClientTruncation" should "match a bare EntityStreamException cause" in {
    handler.isClientTruncation(truncation()) shouldBe true
  }

  it should "see through Play's UnexpectedException wrapper to the EntityStreamException cause" in {
    val nested = new RuntimeException("wrapper", truncation())
    handler.isClientTruncation(nested) shouldBe true
  }

  it should "see through multiple wrapper layers (up to 4)" in {
    val deep = new RuntimeException("L1",
      new RuntimeException("L2",
        new RuntimeException("L3", truncation())))
    handler.isClientTruncation(deep) shouldBe true
  }

  it should "ignore an unrelated exception chain (Mongo blip, NPE, parse error, …)" in {
    handler.isClientTruncation(new RuntimeException("Mongo connection lost")) shouldBe false
    handler.isClientTruncation(new NullPointerException()) shouldBe false
  }

  it should "treat a null cause as non-truncation (don't crash on it)" in {
    handler.isClientTruncation(null) shouldBe false
  }

  // Pekko 1.x's HTTP parser sometimes wraps the truncation deep enough
  // that the chain is longer than 4. The predicate bottoms out at depth
  // 4 to keep this cheap and to avoid a runaway chain; verify the
  // bottom-out doesn't accidentally classify a long non-truncation
  // chain as truncation.
  it should "stop walking the cause chain past depth 4 (returns false, not true)" in {
    val deep = new RuntimeException("L1",
      new RuntimeException("L2",
        new RuntimeException("L3",
          new RuntimeException("L4",
            new RuntimeException("L5",
              new RuntimeException("L6", truncation()))))))
    handler.isClientTruncation(deep) shouldBe false
  }
}
