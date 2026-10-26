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

  /** Where the build writes its stamps (`build.sbt`): under the identity module's own package. At the classpath's root,
   *  a stamp an older build left in another module's output (common's, which generated them until the resolver moved
   *  into its own module) came first on the worker's classpath and shadowed the build's own: the rules versions read
   *  stood still while the rules changed, and no stored family or verdict was decided again. */
  val Stamps = "services/identity"

  /** The build's version stamp `name` (`<name>-version.txt`, trimmed). Missing, it THROWS: a constant "unknown" would
   *  keep every stored family and verdict across any change of the rules. */
  def stamp(name: String): String = {
    val path = s"$Stamps/$name-version.txt"
    read(path)(in => new String(in.readAllBytes(), StandardCharsets.UTF_8).trim)
      .getOrElse(throw new IllegalStateException(s"$path is not on the classpath: the build generates it (build.sbt)"))
  }

  private def read[A](path: String)(f: java.io.InputStream => A): Option[A] =
    Option(getClass.getClassLoader.getResourceAsStream(path.stripPrefix("/"))).map(in => try f(in) finally in.close())
}
