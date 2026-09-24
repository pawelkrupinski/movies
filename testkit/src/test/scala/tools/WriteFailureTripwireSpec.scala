package tools

import org.bson.codecs.configuration.CodecConfigurationException
import org.scalatest.ConfigMap
import org.scalatest.events.{Ordinal, RunCompleted, RunStarting}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The tripwire's own half: which log lines it records, where, and when it stops. The build
 * half (`project/WriteFailureTripwire.scala`) only reads the files this writes and fails the
 * run when there are any; its end-to-end proof is in the commit that added it (the
 * reintroduced codec bug trips `RekeyScreeningsIntegrationSpec`, which otherwise passes).
 *
 * Every line here carries a per-test token, and only lines with it are counted: the
 * reporter listens on the ROOT logger, so a parallel suite's failed write could land in the file.
 */
class WriteFailureTripwireSpec extends AnyFlatSpec with Matchers {

  private val logger = LoggerFactory.getLogger("kinowo.write-failure-tripwire-spec")

  /** Runs `body` under a reporter writing into a fresh directory; returns the recorded lines
   *  carrying `token`. */
  private def tripwire(token: String)(body: Path => Unit): Seq[String] = {
    val dir      = Files.createTempDirectory("write-failure-tripwire")
    val reporter = new WriteFailureTripwire
    reporter(RunStarting(new Ordinal(1), 0, ConfigMap(WriteFailureTripwire.DirKey -> dir.toString)))
    try body(dir)
    finally reporter(RunCompleted(new Ordinal(1)))
    Files.list(dir).iterator().asScala.filter(_.toString.endsWith(".txt")).toSeq
      .flatMap(f => Files.readAllLines(f).asScala).filter(_.contains(token))
  }

  private val playLogger = play.api.Logger("kinowo.write-failure-tripwire-spec")

  /** A write that throws, through the one path every Mongo repository writes by. */
  private def failedWrite(what: String): Unit = {
    services.movies.RepositoryWrite.unit("movies", "upsert", what, services.movies.RepositoryWriteMetrics.noop, playLogger)(
      throw new CodecConfigurationException("Can't find a codec for class [I"))
    ()
  }

  "the write-failure tripwire" should "record a write RepositoryWrite.attempt reports as failed" in {
    val token = s"t${System.nanoTime()}"
    val lines = tripwire(token)(_ => failedWrite(s"MovieRepository.upsert($token, Some(2026))"))
    lines should have size 1
    lines.head should include(s"MovieRepository.upsert($token, Some(2026)) failed: Can't find a codec")
  }

  it should "ignore every other line, even one that reads like a failed write" in {
    val token = s"t${System.nanoTime()}"
    tripwire(token) { _ =>
      logger.warn(s"MovieRepository.upsert($token, Some(2026)) failed: not through RepositoryWrite")
      logger.error(s"$token", new CodecConfigurationException("no codec"))
    } shouldBe empty
  }

  it should "excuse a failure a spec provokes on purpose, by its listed fragment" in {
    val token = s"t${System.nanoTime()}"
    tripwire(token)(_ => failedWrite(s"MovieRepository.upsert(__repository-write-failure-sentinel__ $token, Some(2026))")) shouldBe empty
  }

  it should "stop writing once the build has read the run (a new generation)" in {
    val token = s"t${System.nanoTime()}"
    tripwire(token) { dir =>
      Files.writeString(dir.resolve("generation"), "read")
      failedWrite(s"MovieRepository.upsert($token, Some(2026))")
    } shouldBe empty
  }
}
