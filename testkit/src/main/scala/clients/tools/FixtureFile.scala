package clients.tools

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import scala.io.{Codec, Source}

/** Reads a recorded fixture file whole, as a `String` — the one place this
 *  repo's ~10 cinema/rating-client specs load a captured response from disk
 *  before feeding it through the client's pure parser. Every one of them used
 *  to hand-roll `Source.fromFile(path); try src.mkString finally src.close()`
 *  itself; forced UTF-8 rather than the platform default (which `Source.fromFile`
 *  falls back to when no codec is given) so a fixture with non-ASCII cast/title
 *  text reads the same on every machine and CI runner, not just ones whose
 *  default charset happens to already be UTF-8.
 */
object FixtureFile {
  def read(path: String): String = {
    val src = Source.fromFile(path)(using Codec.UTF8)
    try src.mkString finally src.close()
  }

  /** A classpath resource read whole as UTF-8 — `fixtures/tmdb/search_lalka_2026.json`, a leading `/` or none —
   *  failing loudly, naming it, when the classpath holds no such resource (a bare `getResourceAsStream` hands back
   *  null and the spec dies on a NullPointerException somewhere else). */
  def resource(name: String): String = {
    val path = name.stripPrefix("/")
    val in   = Option(getClass.getClassLoader.getResourceAsStream(path))
      .getOrElse(throw new FileNotFoundException(s"no classpath resource $path"))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  }
}
