package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `Env` reads from the process environment, JVM system properties, then
 * `.env.local`. We can't set process env vars from a test, but `get` also
 * honours system properties — so the numeric helpers are driven through
 * `System.setProperty` here (cleared in `finally` to avoid cross-test leakage).
 */
class EnvSpec extends AnyFlatSpec with Matchers {

  private def withProp[A](key: String, value: String)(body: => A): A =
    try { System.setProperty(key, value); body } finally System.clearProperty(key)

  "Env.positiveInt" should "use the default when unset" in {
    Env.positiveInt("KINOWO_TEST_UNSET_INT", 8) shouldBe 8
  }

  it should "parse a positive value" in {
    withProp("KINOWO_TEST_INT", "3") { Env.positiveInt("KINOWO_TEST_INT", 8) shouldBe 3 }
  }

  it should "fall back to the default for non-positive or unparseable values" in {
    withProp("KINOWO_TEST_INT", "0")   { Env.positiveInt("KINOWO_TEST_INT", 8) shouldBe 8 }
    withProp("KINOWO_TEST_INT", "-4")  { Env.positiveInt("KINOWO_TEST_INT", 8) shouldBe 8 }
    withProp("KINOWO_TEST_INT", "abc") { Env.positiveInt("KINOWO_TEST_INT", 8) shouldBe 8 }
  }

  "Env.positiveLong" should "use the default when unset" in {
    Env.positiveLong("KINOWO_TEST_UNSET_LONG", 300L) shouldBe 300L
  }

  it should "parse a positive value and reject non-positive / unparseable ones" in {
    withProp("KINOWO_TEST_LONG", "30")  { Env.positiveLong("KINOWO_TEST_LONG", 300L) shouldBe 30L }
    withProp("KINOWO_TEST_LONG", "0")   { Env.positiveLong("KINOWO_TEST_LONG", 300L) shouldBe 300L }
    withProp("KINOWO_TEST_LONG", "xyz") { Env.positiveLong("KINOWO_TEST_LONG", 300L) shouldBe 300L }
  }

  // ── admin override source ─────────────────────────────────────────────────────
  // Installed by the composition root; wins over env/property/file so an admin
  // flip takes effect, and is read live (so a per-use read changes mid-flight).
  private def withOverrides[A](pairs: (String, String)*)(body: => A): A =
    try { Env.installOverrides(pairs.toMap.get); body } finally Env.installOverrides(_ => None)

  "An installed override" should "win over a system property and apply live" in {
    withProp("KINOWO_TEST_OVR", "5") {
      Env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 5            // no override yet
      withOverrides("KINOWO_TEST_OVR" -> "9") {
        Env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 9         // override wins over the property
      }
      Env.positiveInt("KINOWO_TEST_OVR", 1) shouldBe 5            // removed → back to the property
    }
  }

  it should "fall back to the default when the override value is non-positive" in {
    withOverrides("KINOWO_TEST_OVR2" -> "-3") {
      Env.positiveInt("KINOWO_TEST_OVR2", 7) shouldBe 7
    }
  }

  "currentValue" should "report the post-override value the process is using" in {
    Env.currentValue("KINOWO_TEST_UNSET_NONE") shouldBe None
    withProp("KINOWO_TEST_CUR", "2") {
      Env.currentValue("KINOWO_TEST_CUR") shouldBe Some("2")
      withOverrides("KINOWO_TEST_CUR" -> "4") {
        Env.currentValue("KINOWO_TEST_CUR") shouldBe Some("4")
      }
    }
  }

  "Reading a knob" should "self-register its key, kind and default" in {
    Env.positiveLong("KINOWO_TEST_REG", 42L)
    val knob = Env.knobs.find(_.key == "KINOWO_TEST_REG")
    knob.map(_.kind)        shouldBe Some(Env.Kind.Long)
    knob.flatMap(_.default) shouldBe Some("42")
  }
}
