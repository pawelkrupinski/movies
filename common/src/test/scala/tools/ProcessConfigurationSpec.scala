package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoAddress

import java.nio.file.Path

/** The typed values a `main` hands down, resolved from an `Env` — driven here over maps, so
 *  nothing in this spec reads (or writes) the real process. */
class ProcessConfigurationSpec extends AnyFlatSpec with Matchers {

  private def configuration(vars: (String, String)*) = new ProcessConfiguration(Env.of(vars*))

  "ProcessConfiguration" should "resolve the serving country and the Mongo address" in {
    val resolved = configuration("KINOWO_COUNTRY" -> "uk", "MONGODB_URI" -> "mongodb://probe:1", "MONGODB_DB" -> "kinowo_probe")
    resolved.country      shouldBe Country.UnitedKingdom
    resolved.mongoAddress shouldBe MongoAddress(Some("mongodb://probe:1"), Some("kinowo_probe"))
    configuration().country shouldBe Country.default
  }

  it should "resolve the commit and the port, with their defaults" in {
    configuration("COMMIT_SHA" -> "abc123", "PORT" -> "9123").commit shouldBe "abc123"
    configuration("PORT" -> "9123").port(default = 9000) shouldBe 9123
    configuration().commit shouldBe "unknown"
    configuration("PORT" -> "not-a-port").port(default = 9000) shouldBe 9000
  }

  it should "pass APP_MODE through only when set" in {
    configuration("APP_MODE" -> "prod").applicationMode shouldBe Some("prod")
    configuration().applicationMode shouldBe None
  }

  it should "split PATH into the directories an executable is looked for in" in {
    val separator = java.io.File.pathSeparator
    configuration("PATH" -> s"/opt/homebrew/bin$separator$separator/usr/bin").executableSearchPath shouldBe
      Seq(Path.of("/opt/homebrew/bin"), Path.of("/usr/bin"))
    configuration().executableSearchPath shouldBe empty
  }

  // The commit, the port and PATH are facts about the process, not knobs an admin flips —
  // they must not appear on /admin/config as if they were.
  it should "not register process facts as admin-page knobs" in {
    val resolved = configuration("COMMIT_SHA" -> "abc", "PORT" -> "1", "PATH" -> "/bin", "APP_MODE" -> "dev")
    resolved.commit; resolved.port(9000); resolved.executableSearchPath; resolved.applicationMode
    resolved.env.knobs shouldBe empty
  }
}
