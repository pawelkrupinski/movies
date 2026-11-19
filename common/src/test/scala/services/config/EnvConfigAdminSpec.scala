package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

class EnvConfigAdminSpec extends AnyFlatSpec with Matchers {

  "rows" should "merge a key across apps and attach its override" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("web",    Seq(RegisteredKnob("web",    "KINOWO_X", Env.Kind.Long, Some("5"), Some("5"))))
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_X", Env.Kind.Long, Some("5"), Some("7"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_X", "7")
    val rows = new EnvConfigAdmin(overrides, registry).rows()
    rows.map(_.key) shouldBe Seq("KINOWO_X")
    val row = rows.head
    row.overrideValue shouldBe Some("7")
    row.apps.map(a => a.app -> a.current) should contain theSameElementsAs Seq("web" -> Some("5"), "worker" -> Some("7"))
  }

  it should "show no override (Set affordance) for a key with no flip" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_Y", Env.Kind.Int, Some("3"), Some("3"))))
    val rows = new EnvConfigAdmin(new InMemoryEnvOverrideStore, registry).rows()
    rows.head.overrideValue shouldBe None
  }

  "set" should "accept a valid numeric flip and reject a non-numeric one for an Int/Long knob" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    val svc = new EnvConfigAdmin(overrides, registry)
    svc.set("KINOWO_N", "12") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe Some("12")
    svc.set("KINOWO_N", "abc") shouldBe false             // not a Long
    overrides.lookup("KINOWO_N") shouldBe Some("12")      // unchanged
  }

  it should "reject an unknown or secret key" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val svc = new EnvConfigAdmin(new InMemoryEnvOverrideStore, registry)
    svc.set("KINOWO_UNKNOWN", "1") shouldBe false
    svc.set("ZYTE_API_KEY", "x")   shouldBe false         // even if it were registered, secret
  }

  "reset" should "clear an override for a known key and reject a secret one" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_N", "9")
    val svc = new EnvConfigAdmin(overrides, registry)
    svc.reset("KINOWO_N") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe None
    svc.reset("TELEGRAM_BOT_TOKEN") shouldBe false
  }
}
