package tools

import ch.qos.logback.classic.LoggerContext
import org.slf4j.{LoggerFactory, MDC}
import settings.CommitSha

/**
 * Fields on worker log lines beside their text — each one a VictoriaLogs field LogsQL filters and groups on, once
 * [[JsonLineEncoder]] has written the line as JSON:
 *  - [[stampBuild]]: the image's commit, as `build`, on EVERY line from the boot on — what ties a line to a deploy;
 *  - [[within]]: fields on every line a block logs on its thread (the MDC) — a projection's `projection_id`;
 *  - [[info]] / [[warn]]: fields of one line (SLF4J key-value pairs) — a take change's `kind`, `basis`, `venue`, ….
 */
object LogFields {
  /** The field every worker line carries its image's commit in. */
  val Build = "build"

  /** Put `commit` on every line this process logs from now on, as [[Build]]. */
  def stampBuild(commit: CommitSha): Unit = LoggerFactory.getILoggerFactory match {
    case context: LoggerContext => stamp(context, commit)
    case _                      => ()
  }

  /** [[stampBuild]] on `context` — the process's own, or a spec's. */
  def stamp(context: LoggerContext, commit: CommitSha): Unit = context.putProperty(Build, commit.value)

  /** Run `body` with `fields` on every line it logs on this thread, the fields it found there put back after. */
  def within[A](fields: (String, String)*)(body: => A): A = {
    val before = fields.map { case (key, _) => key -> Option(MDC.get(key)) }
    fields.foreach { case (key, value) => MDC.put(key, value) }
    try body
    finally before.foreach { case (key, was) => was.fold(MDC.remove(key))(MDC.put(key, _)) }
  }

  /** This thread's [[within]] fields, to run work handed to another thread [[carrying]] them. */
  def current(): Map[String, String] = Option(MDC.getCopyOfContextMap).fold(Map.empty[String, String])(m =>
    scala.jdk.CollectionConverters.MapHasAsScala(m).asScala.toMap)

  /** Run `body` with `fields` (another thread's [[current]]) on every line it logs. */
  def carrying[A](fields: Map[String, String])(body: => A): A = within(fields.toSeq*)(body)

  /** Log `message` at INFO with `fields`: a `None` or `null` value leaves its field out, a `Some` is its value. */
  def info(logger: org.slf4j.Logger, message: String, fields: Iterable[(String, Any)]): Unit =
    if (logger.isInfoEnabled) withPairs(logger.atInfo(), fields).log(message)

  /** [[info]], at WARN. */
  def warn(logger: org.slf4j.Logger, message: String, fields: Iterable[(String, Any)]): Unit =
    if (logger.isWarnEnabled) withPairs(logger.atWarn(), fields).log(message)

  private def withPairs(builder: org.slf4j.spi.LoggingEventBuilder, fields: Iterable[(String, Any)]): org.slf4j.spi.LoggingEventBuilder = {
    fields.foreach {
      case (_, None | null) => ()
      case (key, Some(value)) => builder.addKeyValue(key, value.asInstanceOf[AnyRef])
      case (key, value)       => builder.addKeyValue(key, value.asInstanceOf[AnyRef])
    }
    builder
  }
}
