package tools

import ch.qos.logback.classic.spi.{ILoggingEvent, ThrowableProxyUtil}
import ch.qos.logback.core.encoder.EncoderBase

import java.nio.charset.StandardCharsets

/**
 * The worker's stdout format: ONE JSON object per log line — `level`, `logger`, `thread`, `message`, then the logger
 * context's properties (the `build` the boot stamps, `LogFields.stampBuild`), the MDC (a projection's
 * `projection_id`), the key-value pairs a log call adds (`LogFields.info` / `warn`), and a thrown exception's `stack_trace` (which `message` also
 * ends with, so a word search over `_msg` finds it).
 *
 * The fleet's Vector parses a JSON line and merges its fields into the VictoriaLogs row (`mergeJsonBodyVrl` in
 * `infra/nix/modules/fleet/logs.nix`, checked by `infra/test/test_log_fields.sh`): `message` becomes `_msg`, every other
 * field one LogsQL filters and groups on (`kind:lost | stats by (basis) count()`), where a text line left each of them
 * to a regex over `_msg`. The shipper's own fields (`host`, `app`, `country`, …) win a name clash, and so do this line's
 * own over a pair's or the MDC's, so nothing a call adds can rename the line.
 *
 * Hand-written rather than logstash-logback-encoder: that one brings Jackson 2.18+, and the worker's Play JSON pins
 * jackson-module-scala 2.14, which refuses any other databind at start-up. The line is a few appends per field.
 */
final class JsonLineEncoder extends EncoderBase[ILoggingEvent] {
  import JsonLineEncoder.*

  def headerBytes(): Array[Byte] = Array.emptyByteArray
  def footerBytes(): Array[Byte] = Array.emptyByteArray

  def encode(event: ILoggingEvent): Array[Byte] = {
    val line = new java.lang.StringBuilder(256)
    line.append('{')
    field(line, "level", event.getLevel.toString, first = true)
    field(line, "logger", event.getLoggerName)
    field(line, "thread", event.getThreadName)
    val stackTrace = Option(event.getThrowableProxy).map(ThrowableProxyUtil.asString)
    field(line, "message", message(Option(event.getFormattedMessage).filterNot(_.isBlank), stackTrace))
    val context = Option(event.getLoggerContextVO).map(_.getPropertyMap).orNull
    if (context != null) context.forEach((k, v) => if (!Reserved(k) && !ContextLeftOut(k)) field(line, k, v))
    val mdc = event.getMDCPropertyMap
    if (mdc != null) mdc.forEach((k, v) => if (!Reserved(k)) field(line, k, v))
    val pairs = event.getKeyValuePairs
    if (pairs != null) pairs.forEach(pair => if (pair.key != null && !Reserved(pair.key) && pair.value != null) field(line, pair.key, pair.value))
    stackTrace.foreach(field(line, "stack_trace", _))
    line.append("}\n").toString.getBytes(StandardCharsets.UTF_8)
  }
}

object JsonLineEncoder {
  /** The line's own fields, which no MDC entry or key-value pair may stand in for. */
  private val Reserved = Set("level", "logger", "thread", "message", "stack_trace")
  /** Context properties logback keeps for itself: the pod's name is the shipper's `pod` already. */
  private val ContextLeftOut = Set("HOSTNAME", "CONTEXT_NAME")

  /** `message` is `_msg`, the field a bare LogsQL word searches, so a thrown exception's trace goes there too — as it
   *  did when each trace line was a text row of its own — besides its `stack_trace` field. It is never empty: an empty
   *  `_msg` is stored under VictoriaLogs' "missing _msg field" placeholder, which says nothing about the line. */
  private def message(text: Option[String], stackTrace: Option[String]): String =
    (text ++ stackTrace).mkString("\n") match {
      case "" => "(empty log message)"
      case m  => m
    }

  private def field(line: java.lang.StringBuilder, key: String, value: Any, first: Boolean = false): Unit = {
    if (!first) line.append(',')
    quoted(line, key)
    line.append(':')
    value match {
      case n: (java.lang.Integer | java.lang.Long | java.lang.Short | java.lang.Byte) => line.append(n.toString)
      case d: java.lang.Double if !d.isNaN && !d.isInfinite => line.append(d.toString)
      case f: java.lang.Float if !f.isNaN && !f.isInfinite  => line.append(f.toString)
      case b: java.lang.Boolean                             => line.append(b.toString)
      case other                                            => quoted(line, String.valueOf(other))
    }
  }

  private def quoted(line: java.lang.StringBuilder, text: String): Unit = {
    line.append('"')
    var i = 0
    while (i < text.length) {
      text.charAt(i) match {
        case '"'  => line.append("\\\"")
        case '\\' => line.append("\\\\")
        case '\n' => line.append("\\n")
        case '\r' => line.append("\\r")
        case '\t' => line.append("\\t")
        case c if c < 0x20 => line.append(f"\\u${c.toInt}%04x")
        case c => line.append(c)
      }
      i += 1
    }
    line.append('"')
  }
}
