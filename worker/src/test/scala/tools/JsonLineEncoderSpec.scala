package tools

import ch.qos.logback.classic.spi.LoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.event.KeyValuePair
import play.api.libs.json.{JsNumber, JsObject, JsString, Json}

import java.nio.charset.StandardCharsets

/** What a worker log line is on stdout: ONE JSON object per line, whose fields the fleet's Vector merges into the
 *  VictoriaLogs row (`infra/nix/modules/fleet/logs.nix`, `mergeJsonBodyVrl`) — so a key-value pair a log call adds,
 *  the MDC (a projection's id) and the logger context's properties (the build) are each a field LogsQL filters on,
 *  and `message` alone is `_msg`. */
class JsonLineEncoderSpec extends AnyFlatSpec with Matchers {

  private def encoded(build: Option[String] = None)(event: LoggerContext => LoggingEvent): JsObject = {
    val context = new LoggerContext()
    context.setMDCAdapter(new ch.qos.logback.classic.util.LogbackMDCAdapter)
    build.foreach(context.putProperty("build", _))
    val encoder = new JsonLineEncoder
    encoder.setContext(context)
    encoder.start()
    val line = new String(encoder.encode(event(context)), StandardCharsets.UTF_8)
    line should endWith("\n")
    line.dropRight(1) should not include "\n"
    Json.parse(line).as[JsObject]
  }

  private def event(context: LoggerContext, message: String, level: Level = Level.INFO, error: Throwable = null): LoggingEvent = {
    val logger = context.getLogger("services.identity.IdentityProjection")
    new LoggingEvent("fqcn", logger, level, message, error, null)
  }

  "A log line" should "carry its key-value pairs, MDC and context properties as top-level fields, numbers as numbers" in {
    val json = encoded(build = Some("0e41a4812")) { context =>
      val e = event(context, "identity take change: lost Kino \"Muza\" | Lalka\tnew")
      e.addKeyValuePair(new KeyValuePair("kind", "lost"))
      e.addKeyValuePair(new KeyValuePair("tmdb_before", Int.box(501)))
      e.addKeyValuePair(new KeyValuePair("confidence", Double.box(0.875)))
      e.setMDCPropertyMap(java.util.Map.of("projection_id", "2026-10-13T21:24:50.290Z"))
      e
    }
    json("message") shouldBe JsString("identity take change: lost Kino \"Muza\" | Lalka\tnew")
    json("level") shouldBe JsString("INFO")
    json("logger") shouldBe JsString("services.identity.IdentityProjection")
    json("kind") shouldBe JsString("lost")
    json("tmdb_before") shouldBe JsNumber(501)
    json("confidence") shouldBe JsNumber(0.875)
    json("projection_id") shouldBe JsString("2026-10-13T21:24:50.290Z")
    json("build") shouldBe JsString("0e41a4812")
  }

  it should "keep a thrown exception in ONE line, its stack trace a field of its own" in {
    val json = encoded() { context => event(context, "identity projection failed", Level.WARN, new IllegalStateException("bus down\nhard")) }
    json("level") shouldBe JsString("WARN")
    json("message") shouldBe JsString("identity projection failed")
    json("stack_trace").as[String] should (include("java.lang.IllegalStateException: bus down") and include("JsonLineEncoderSpec"))
  }

  it should "never let a pair rename the line's own fields" in {
    val json = encoded() { context =>
      val e = event(context, "the text")
      e.addKeyValuePair(new KeyValuePair("message", "a pair"))
      e.addKeyValuePair(new KeyValuePair("level", "ERROR"))
      e
    }
    json("message") shouldBe JsString("the text")
    json("level") shouldBe JsString("INFO")
  }

  it should "escape control characters and keep non-ASCII text as it is" in {
    val json = encoded() { context => event(context, "Łódź \u0001 \\ end") }
    json("message") shouldBe JsString("Łódź \u0001 \\ end")
  }

  it should "leave out a pair whose value is null" in {
    val json = encoded() { context =>
      val e = event(context, "text")
      e.addKeyValuePair(new KeyValuePair("runner_up", null))
      e
    }
    json.keys should not contain "runner_up"
  }
}
