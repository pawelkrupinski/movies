package tools

import scala.io.Source

/** A hand-kept table checked in as a classpath resource (a title-words, title-rules or stage-house TSV), read whole. */
object ClasspathTable {
  /** `path`'s lines read by `parse`. A missing resource fails loudly: an empty table would silently read as no rows. */
  def read[A](path: String)(parse: Iterator[String] => A): A = {
    val in = Option(getClass.getClassLoader.getResourceAsStream(path)).getOrElse(throw new IllegalStateException(s"no $path on the classpath"))
    try parse(Source.fromInputStream(in, "UTF-8").getLines()) finally in.close()
  }
}
