package tools

import scala.io.Source
import scala.util.Try

/**
 * Reads environment-style configuration with a layered fallback:
 *
 *   1. Process environment variable (System.getenv)
 *   2. JVM system property of the same name (-Dkey=value or sys.props)
 *   3. `.env.local` in the working directory — a gitignored file for local dev.
 *      Format: simple `KEY=VALUE` lines, `#` for comments, optional quoting.
 *
 * The `.env.local` route is meant for putting secrets like ZYTE_API_KEY in
 * a local file without polluting the shell or the repository.
 */
object Env {

  def get(key: String): Option[String] =
    Option(System.getenv(key)).filter(_.nonEmpty)
      .orElse(Option(System.getProperty(key)).filter(_.nonEmpty))
      .orElse(fileVars.get(key))
      .filter(_.nonEmpty)

  /** A strictly-positive Int from `key`, or `default` when unset, unparseable,
   *  or ≤ 0. For numeric tuning knobs (concurrency budgets, …) where a bad value
   *  should fall back to the sane default rather than crash or disable the work. */
  def positiveInt(key: String, default: Int): Int =
    get(key).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)

  /** A strictly-positive Long from `key`, or `default` when unset, unparseable,
   *  or ≤ 0. For numeric tuning knobs (intervals in seconds, …). */
  def positiveLong(key: String, default: Long): Long =
    get(key).flatMap(_.toLongOption).filter(_ > 0).getOrElse(default)

  private lazy val fileVars: Map[String, String] =
    Try {
      val file = new java.io.File(".env.local")
      if (!file.exists()) Map.empty[String, String]
      else {
        val source = Source.fromFile(file, "UTF-8")
        try {
          source.getLines()
            .map(_.trim)
            .filterNot(line => line.isEmpty || line.startsWith("#"))
            .flatMap { line =>
              val idx = line.indexOf('=')
              if (idx <= 0) None
              else {
                val k = line.take(idx).trim
                val v = line.drop(idx + 1).trim
                  .stripPrefix("\"").stripSuffix("\"")
                  .stripPrefix("'").stripSuffix("'")
                Some(k -> v)
              }
            }.toMap
        } finally source.close()
      }
    }.getOrElse(Map.empty)
}
