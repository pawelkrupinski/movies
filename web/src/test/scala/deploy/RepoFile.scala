package deploy

import java.io.File
import scala.io.{Codec, Source}

/**
 * Repo files for the web module's config-lock specs. Found by walking UP from the working
 * directory rather than assumed relative to it: sbt runs each module's tests from its own
 * `baseDirectory`, so the worker's specs see the repo root and the web's see `web/`.
 * Hard-coding either makes a spec fail -- or pass vacuously -- in the other module.
 */
object RepoFile {

  /** `relative` (a file or a directory) under the nearest ancestor that has it. */
  def locate(relative: String): File =
    Iterator
      .iterate(new File(".").getAbsoluteFile)(_.getParentFile)
      .takeWhile(_ != null)
      .map(dir => new File(dir, relative))
      .find(_.exists)
      .getOrElse(throw new AssertionError(s"no $relative in any parent of ${new File(".").getAbsolutePath}"))

  def read(file: File): String = {
    val src = Source.fromFile(file)(using Codec.UTF8)
    try src.mkString
    finally src.close()
  }

  /** The version project/Dependencies.scala pins as `private val <name> = "..."`. */
  def declaredVersion(name: String): String =
    s"""$name\\s*=\\s*"([^"]+)"""".r
      .findFirstMatchIn(read(locate("project/Dependencies.scala")))
      .map(_.group(1))
      .getOrElse(throw new AssertionError(s"no $name in project/Dependencies.scala"))
}
