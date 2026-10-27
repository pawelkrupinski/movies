package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The process-bound [[Env]] against a real `.env.local` in the real working directory, which no temp directory can stand in for; the precedence rules are [[EnvSpec]]. */
class EnvIntegrationSpec extends AnyFlatSpec with Matchers {

  // The app reads its process and nothing else: secrets reach a local run through
  // `secrets run movies … -- <command>`, never a file. `.env.local` (deleted 2026-10-09)
  // used to be read from the working directory — and every unforked test layer has the
  // repo root as its working directory, so a key left in that file switched live services
  // on under the tests.
  "The process-bound Env" should "never read a .env.local in the working directory" in {
    val file = new java.io.File(".env.local").getAbsoluteFile  // the working directory
    assume(!file.exists(), s"$file already exists; refusing to overwrite it")
    val key = s"KINOWO_TEST_ONLY_IN_FILE_${java.util.UUID.randomUUID().toString.replace("-", "")}"
    Files.writeString(file.toPath, s"$key=file\n")
    try settings.ProcessConfiguration.resolve().env.get(key) shouldBe None
    finally Files.deleteIfExists(file.toPath)
  }
}
