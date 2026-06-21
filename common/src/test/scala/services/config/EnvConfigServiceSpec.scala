package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

class EnvConfigServiceSpec extends AnyFlatSpec with Matchers {

  private def knob(key: String, kind: Env.Kind = Env.Kind.Long, default: Option[String] = Some("5")) =
    Env.Knob(key, kind, default)

  private def service(
    app: String,
    overrides: EnvOverrideStore,
    registry: EnvRegistryStore,
    knobs: Seq[Env.Knob],
    current: String => Option[String] = _ => None
  ) = new EnvConfigService(app, overrides, registry,
    knobSource = () => knobs, currentValueOf = current)

  "publishTick" should "publish this app's non-secret knobs with their current values, dropping secrets" in {
    val registry = new InMemoryEnvRegistryStore
    val knobs = Seq(knob("KINOWO_A"), knob("KINOWO_B"), knob("ZYTE_API_KEY", Env.Kind.Str, None))
    val svc = service("worker", new InMemoryEnvOverrideStore, registry, knobs,
      current = Map("KINOWO_A" -> "5", "KINOWO_B" -> "9", "ZYTE_API_KEY" -> "secret").get)
    svc.publishTick()
    val keys = registry.all().map(_.key).toSet
    keys shouldBe Set("KINOWO_A", "KINOWO_B")             // ZYTE_API_KEY excluded
    registry.all().find(_.key == "KINOWO_A").flatMap(_.current) shouldBe Some("5")
  }

  it should "replace the prior publish so a key no longer read disappears" in {
    val registry = new InMemoryEnvRegistryStore
    service("worker", new InMemoryEnvOverrideStore, registry, Seq(knob("KINOWO_OLD"), knob("KINOWO_KEEP"))).publishTick()
    service("worker", new InMemoryEnvOverrideStore, registry, Seq(knob("KINOWO_KEEP"))).publishTick()
    registry.all().map(_.key).toSet shouldBe Set("KINOWO_KEEP")
  }

  "rows" should "merge a key across apps and attach its override" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("web",    Seq(RegisteredKnob("web",    "KINOWO_X", Env.Kind.Long, Some("5"), Some("5"))))
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_X", Env.Kind.Long, Some("5"), Some("7"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_X", "7")
    val rows = service("web", overrides, registry, Seq.empty).rows()
    rows.map(_.key) shouldBe Seq("KINOWO_X")
    val row = rows.head
    row.overrideValue shouldBe Some("7")
    row.apps.map(a => a.app -> a.current) should contain theSameElementsAs Seq("web" -> Some("5"), "worker" -> Some("7"))
  }

  it should "show no override (Set affordance) for a key with no flip" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_Y", Env.Kind.Int, Some("3"), Some("3"))))
    val rows = service("web", new InMemoryEnvOverrideStore, registry, Seq.empty).rows()
    rows.head.overrideValue shouldBe None
  }

  "set" should "accept a valid numeric flip and reject a non-numeric one for an Int/Long knob" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    val svc = service("web", overrides, registry, Seq.empty)
    svc.set("KINOWO_N", "12") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe Some("12")
    svc.set("KINOWO_N", "abc") shouldBe false             // not a Long
    overrides.lookup("KINOWO_N") shouldBe Some("12")      // unchanged
  }

  it should "reject an unknown or secret key" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val svc = service("web", new InMemoryEnvOverrideStore, registry, Seq.empty)
    svc.set("KINOWO_UNKNOWN", "1") shouldBe false
    svc.set("ZYTE_API_KEY", "x")   shouldBe false         // even if it were registered, secret
  }

  "reset" should "clear an override for a known key and reject a secret one" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_N", "9")
    val svc = service("web", overrides, registry, Seq.empty)
    svc.reset("KINOWO_N") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe None
    svc.reset("TELEGRAM_BOT_TOKEN") shouldBe false
  }
}
