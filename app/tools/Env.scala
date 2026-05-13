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
 * The `.env.local` route is meant for putting secrets like SCRAPINGANT_KEY in
 * a local file without polluting the shell or the repo.
 */
object Env {

  def get(key: String): Option[String] =
    Option(System.getenv(key)).filter(_.nonEmpty)
      .orElse(Option(System.getProperty(key)).filter(_.nonEmpty))
      .orElse(fileVars.get(key))
      .filter(_.nonEmpty)

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
