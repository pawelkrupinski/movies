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
}
