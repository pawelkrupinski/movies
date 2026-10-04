package services.identity

import org.scalatest.Assertions.{fail, withClue}
import org.scalatest.matchers.should.Matchers.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** What the version specs read: a generated resource (a version, its digested paths) and common's sources. */
object VersionedSources {

  def resource(name: String): String = {
    val stream = getClass.getResourceAsStream(name)
    withClue(s"$name is not on the classpath: ")(stream should not be null)
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim finally stream.close()
  }

  /** `common/src/main`, which the digested paths are relative to. */
  lazy val main: Path =
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent).takeWhile(_ != null).map(_.resolve("common/src/main"))
      .find(Files.isDirectory(_)).getOrElse(fail("common/src/main not found above the working directory"))

  /** The digested Scala sources of `paths` the source digest cannot lex, so would digest as bytes. */
  def unlexable(paths: Seq[String]): Seq[String] =
    paths.filter(_.endsWith(".scala"))
      .filter(path => kinowo.build.SourceDigest.lexed(new String(Files.readAllBytes(main.resolve(path)), StandardCharsets.UTF_8)).isEmpty)
}
