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
    svc.set("KINOWO_N", "12") shouldBe FlipOutcome.Applied
    overrides.lookup("KINOWO_N") shouldBe Some("12")
    svc.set("KINOWO_N", "abc") shouldBe FlipOutcome.Rejected             // not a Long
    overrides.lookup("KINOWO_N") shouldBe Some("12")      // unchanged
  }

  it should "reject an unknown or secret key" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val svc = new EnvConfigAdmin(new InMemoryEnvOverrideStore, registry)
    svc.set("KINOWO_UNKNOWN", "1") shouldBe FlipOutcome.Rejected
    svc.set("ZYTE_API_KEY", "x")   shouldBe FlipOutcome.Rejected         // even if it were registered, secret
  }

  "reset" should "clear an override for a known key and reject a secret one" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_N", "9")
    val svc = new EnvConfigAdmin(overrides, registry)
    svc.reset("KINOWO_N") shouldBe FlipOutcome.Applied
    overrides.lookup("KINOWO_N") shouldBe None
    svc.reset("TELEGRAM_BOT_TOKEN") shouldBe FlipOutcome.Rejected
  }

  // A write that failed was only logged: the page said the flip was applied, this process ran it until its next refresh
  // reverted it, and no other process ever saw it.
  "set and reset" should "report NotStored, and apply nothing, when the override store cannot be written" in {
    val unreachable = org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
    try {
      val registry = new InMemoryEnvRegistryStore
      registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
      val overrides = new MongoEnvOverrideStore(Some(unreachable.getDatabase("unreachable")))
      val svc = new EnvConfigAdmin(overrides, registry)
      svc.set("KINOWO_N", "12") shouldBe FlipOutcome.NotStored
      overrides.lookup("KINOWO_N") shouldBe None
      svc.reset("KINOWO_N") shouldBe FlipOutcome.NotStored
    } finally unreachable.close()
  }
}
