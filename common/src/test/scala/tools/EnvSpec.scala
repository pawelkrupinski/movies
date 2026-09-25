package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/**
 * `Env` resolves an installed admin override first, then its static source.
 * Most cases drive an instance over a fixed map ([[Env.of]]); the process
 * source's precedence (env var → system property → `.env.local`) is driven through
 * [[Env.layered]] over maps and a temp vars file — [[Env.fromProcess]] is that same
 * precedence bound to the real process, which a spec must not mutate.
 */
class EnvSpec extends AnyFlatSpec with Matchers {

  "Env.fromProcess().positiveInt" should "use the default when unset" in {
    Env.of().positiveInt("KINOWO_TEST_UNSET_INT", 8) shouldBe 8
  }

  it should "parse a positive value" in {
    Env.of("KINOWO_TEST_INT" -> "3").positiveInt("KINOWO_TEST_INT", 8) shouldBe 3
  }

  it should "fall back to the default for non-positive or unparseable values" in {
    Env.of("KINOWO_TEST_INT" -> "0").positiveInt("KINOWO_TEST_INT", 8)   shouldBe 8
    Env.of("KINOWO_TEST_INT" -> "-4").positiveInt("KINOWO_TEST_INT", 8)  shouldBe 8
    Env.of("KINOWO_TEST_INT" -> "abc").positiveInt("KINOWO_TEST_INT", 8) shouldBe 8
  }

  "Env.fromProcess().flag" should "be off when unset, so a switch nobody set stays off" in {
    Env.of().flag("KINOWO_TEST_UNSET_FLAG") shouldBe false
  }

  // Both spellings, because both are what a deployment surface produces: whoever
  // sets a Fly secret or a Kubernetes env value writes whichever of the two they
  // think in, and neither should be a silent no-op.
  it should "accept either spelling of on" in {
    Env.of("KINOWO_TEST_FLAG" -> "true").flag("KINOWO_TEST_FLAG") shouldBe true
    Env.of("KINOWO_TEST_FLAG" -> "1").flag("KINOWO_TEST_FLAG")    shouldBe true
  }

  it should "treat anything else as off rather than as set" in {
    Env.of("KINOWO_TEST_FLAG" -> "false").flag("KINOWO_TEST_FLAG") shouldBe false
    Env.of("KINOWO_TEST_FLAG" -> "yes").flag("KINOWO_TEST_FLAG")   shouldBe false
    Env.of("KINOWO_TEST_FLAG" -> "0").flag("KINOWO_TEST_FLAG")     shouldBe false
  }

  "Env.fromProcess().positiveLong" should "use the default when unset" in {
    Env.of().positiveLong("KINOWO_TEST_UNSET_LONG", 300L) shouldBe 300L
  }

  it should "parse a positive value and reject non-positive / unparseable ones" in {
    Env.of("KINOWO_TEST_LONG" -> "30").positiveLong("KINOWO_TEST_LONG", 300L)  shouldBe 30L
    Env.of("KINOWO_TEST_LONG" -> "0").positiveLong("KINOWO_TEST_LONG", 300L)   shouldBe 300L
    Env.of("KINOWO_TEST_LONG" -> "xyz").positiveLong("KINOWO_TEST_LONG", 300L) shouldBe 300L
  }

  it should "treat an empty static value as unset" in {
    Env.of("KINOWO_TEST_EMPTY" -> "").get("KINOWO_TEST_EMPTY") shouldBe None
  }

  // ── admin override source ─────────────────────────────────────────────────────
  // Installed by EnvConfigService; wins over the static source so an admin flip
  // takes effect, and is read live (so a per-use read changes mid-flight).
  "An installed override" should "win over the static value and apply live" in {
    val env = Env.of("KINOWO_TEST_OVR" -> "5")
    env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 5            // no override yet
    env.installOverrides(Map("KINOWO_TEST_OVR" -> "9").get)
    env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 9            // override wins over the static value
    env.installOverrides(_ => None)
    env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 5            // removed → back to the static value
  }

  it should "fall back to the default when the override value is non-positive" in {
    val env = Env.of()
    env.installOverrides(Map("KINOWO_TEST_OVR2" -> "-3").get)
    env.positiveInt("KINOWO_TEST_OVR2", 7) shouldBe 7
  }

  // The reason Env is an instance: an override installed into one process's (or
  // one spec's) Env must not leak into another's.
  it should "reach only the instance it was installed into" in {
    val flipped = Env.of("KINOWO_TEST_ISO" -> "2")
    val other   = Env.of("KINOWO_TEST_ISO" -> "2")
    flipped.installOverrides(Map("KINOWO_TEST_ISO" -> "8").get)
    flipped.positiveInt("KINOWO_TEST_ISO", 1) shouldBe 8
    other.positiveInt("KINOWO_TEST_ISO", 1)   shouldBe 2
  }

  "currentValue" should "report the post-override value the process is using" in {
    val env = Env.of("KINOWO_TEST_CUR" -> "2")
    env.currentValue("KINOWO_TEST_UNSET_NONE") shouldBe None
    env.currentValue("KINOWO_TEST_CUR") shouldBe Some("2")
    env.installOverrides(Map("KINOWO_TEST_CUR" -> "4").get)
    env.currentValue("KINOWO_TEST_CUR") shouldBe Some("4")
  }

  "Reading a knob" should "self-register its key, kind and default" in {
    val env = Env.of()
    env.positiveLong("KINOWO_TEST_REG", 42L)
    val knob = env.knobs.find(_.key == "KINOWO_TEST_REG")
    knob.map(_.kind)        shouldBe Some(Env.Kind.Long)
    knob.flatMap(_.default) shouldBe Some("42")
  }

  it should "register into the reading instance only" in {
    val reader = Env.of()
    val other  = Env.of()
    reader.positiveInt("KINOWO_TEST_REG_ISO", 1)
    reader.knobs.map(_.key) should contain("KINOWO_TEST_REG_ISO")
    other.knobs shouldBe empty
  }

  // ── process source ────────────────────────────────────────────────────────────
  // `Env.fromProcess` is `Env.layered` bound to System.getenv / System.getProperty, so
  // the precedence is proven over maps here — never by setting a real system property,
  // which every suite sharing this JVM would see.
  private def varsFile(lines: String*): java.io.File = {
    val path = Files.createTempFile("env-spec", ".env")
    Files.writeString(path, lines.mkString("\n"))
    path.toFile.deleteOnExit()
    path.toFile
  }

  private val NoFile = new java.io.File("/nonexistent/.env.local")

  private def layered(environment: Map[String, String], properties: Map[String, String], file: java.io.File = NoFile): Env =
    Env.layered(environment.get, properties.get, file)

  "Env.layered" should "let an environment variable win over a system property and the vars file" in {
    val env = layered(Map("KINOWO_TEST_PROC" -> "env"), Map("KINOWO_TEST_PROC" -> "prop"), varsFile("KINOWO_TEST_PROC=file"))
    env.get("KINOWO_TEST_PROC") shouldBe Some("env")
  }

  it should "let a system property win over the vars file" in {
    val env = layered(Map.empty, Map("KINOWO_TEST_PROC" -> "prop"), varsFile("KINOWO_TEST_PROC=file"))
    env.get("KINOWO_TEST_PROC") shouldBe Some("prop")
  }

  it should "fall through to the vars file when neither process source has the key" in {
    layered(Map.empty, Map.empty, varsFile("KINOWO_TEST_PROC=file")).get("KINOWO_TEST_PROC") shouldBe Some("file")
  }

  it should "treat an empty value at any layer as unset, so the next layer answers" in {
    val env = layered(Map("KINOWO_TEST_PROC" -> ""), Map("KINOWO_TEST_PROC" -> ""), varsFile("KINOWO_TEST_PROC=file"))
    env.get("KINOWO_TEST_PROC") shouldBe Some("file")
  }

  it should "parse comments, blank lines and quoting in the vars file" in {
    val env = layered(Map.empty, Map.empty, varsFile("# comment", "", "KINOWO_TEST_Q1=\"double\"", "KINOWO_TEST_Q2='single'", "noequals"))
    env.get("KINOWO_TEST_Q1") shouldBe Some("double")
    env.get("KINOWO_TEST_Q2") shouldBe Some("single")
    env.get("noequals")       shouldBe None
  }

  it should "treat a missing vars file as empty" in {
    layered(Map.empty, Map.empty).get("KINOWO_TEST_MISSING") shouldBe None
  }

  "Env.fromProcess" should "read the vars file it is pointed at" in {
    Env.fromProcess(varsFile("KINOWO_TEST_FROM_PROCESS_ONLY_IN_FILE=file")).get("KINOWO_TEST_FROM_PROCESS_ONLY_IN_FILE") shouldBe Some("file")
  }
}
