package services.identity

import play.api.libs.json.{Json, Reads}

import java.nio.charset.StandardCharsets
import scala.io.Source

/** The identity rules' checked-in artefacts read off the classpath — weights, tables, the build's version stamps — each
 *  as what a missing one means to its reader. */
object ClasspathResource {
  /** The JSON artefact at `path`, if on the classpath. */
  def json[A: Reads](path: String): Option[A] = read(path)(in => Json.parse(in).as[A])

  /** The text file's lines at `path`; none when it is missing. */
  def lines(path: String): Seq[String] = read(path)(in => Source.fromInputStream(in, "UTF-8").getLines().toVector).getOrElse(Nil)

  /** A table's rows: its lines trimmed, blank lines and `#` comments left out. */
  def rows(lines: Iterator[String]): Iterator[String] = lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))

  /** The build's version stamp at `path` (trimmed); "unknown" when it is missing. */
  def version(path: String): String = read(path)(in => new String(in.readAllBytes(), StandardCharsets.UTF_8).trim).getOrElse("unknown")

  private def read[A](path: String)(f: java.io.InputStream => A): Option[A] =
    Option(getClass.getClassLoader.getResourceAsStream(path.stripPrefix("/"))).map(in => try f(in) finally in.close())
}
