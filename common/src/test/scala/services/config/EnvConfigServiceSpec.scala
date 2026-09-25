package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

class EnvConfigServiceSpec extends AnyFlatSpec with Matchers {

  private def service(
    app: String,
    overrides: EnvOverrideStore,
    registry: EnvRegistryStore,
    env: Env = Env.of()
  ) = new EnvConfigService(app, overrides, registry, env)

  /** An Env over `vars` that has already read `longKnobs` (so they sit in its registry). */
  private def envHaving(longKnobs: Seq[String], vars: (String, String)*): Env = {
    val env = Env.of(vars*)
    longKnobs.foreach(env.positiveLong(_, 5L))
    env
  }

  "publishTick" should "publish this app's non-secret knobs with their current values, dropping secrets" in {
    val registry = new InMemoryEnvRegistryStore
    val env = envHaving(Seq("KINOWO_A", "KINOWO_B"), "KINOWO_A" -> "5", "KINOWO_B" -> "9", "ZYTE_API_KEY" -> "secret")
    env.get("ZYTE_API_KEY")
    val svc = service("worker", new InMemoryEnvOverrideStore, registry, env)
    svc.publishTick()
    val keys = registry.all().map(_.key).toSet
    keys shouldBe Set("KINOWO_A", "KINOWO_B")             // ZYTE_API_KEY excluded
    registry.all().find(_.key == "KINOWO_A").flatMap(_.current) shouldBe Some("5")
  }

  it should "replace the prior publish so a key no longer read disappears" in {
    val registry = new InMemoryEnvRegistryStore
    service("worker", new InMemoryEnvOverrideStore, registry, envHaving(Seq("KINOWO_OLD", "KINOWO_KEEP"))).publishTick()
    service("worker", new InMemoryEnvOverrideStore, registry, envHaving(Seq("KINOWO_KEEP"))).publishTick()
    registry.all().map(_.key).toSet shouldBe Set("KINOWO_KEEP")
  }

  // The service publishes the knobs of the Env it was HANDED — not some
  // process-wide registry another wiring (or spec) has been reading into.
  it should "publish only the knobs its own Env has read" in {
    val registry = new InMemoryEnvRegistryStore
    envHaving(Seq("KINOWO_ELSEWHERE"))
    service("worker", new InMemoryEnvOverrideStore, registry, envHaving(Seq("KINOWO_MINE"))).publishTick()
    registry.all().map(_.key).toSet shouldBe Set("KINOWO_MINE")
  }

  "start" should "install the override store into its own Env and no other" in {
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_FLIPPED", "9")
    val mine  = Env.of("KINOWO_FLIPPED" -> "1")
    val other = Env.of("KINOWO_FLIPPED" -> "1")
    val svc = service("worker", overrides, new InMemoryEnvRegistryStore, mine)
    try {
      svc.start()
      mine.positiveInt("KINOWO_FLIPPED", 3)  shouldBe 9
      other.positiveInt("KINOWO_FLIPPED", 3) shouldBe 1
    } finally svc.stop()
  }

  "rows" should "merge a key across apps and attach its override" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("web",    Seq(RegisteredKnob("web",    "KINOWO_X", Env.Kind.Long, Some("5"), Some("5"))))
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_X", Env.Kind.Long, Some("5"), Some("7"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_X", "7")
    val rows = service("web", overrides, registry).rows()
    rows.map(_.key) shouldBe Seq("KINOWO_X")
    val row = rows.head
    row.overrideValue shouldBe Some("7")
    row.apps.map(a => a.app -> a.current) should contain theSameElementsAs Seq("web" -> Some("5"), "worker" -> Some("7"))
  }

  it should "show no override (Set affordance) for a key with no flip" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_Y", Env.Kind.Int, Some("3"), Some("3"))))
    val rows = service("web", new InMemoryEnvOverrideStore, registry).rows()
    rows.head.overrideValue shouldBe None
  }

  "set" should "accept a valid numeric flip and reject a non-numeric one for an Int/Long knob" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    val svc = service("web", overrides, registry)
    svc.set("KINOWO_N", "12") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe Some("12")
    svc.set("KINOWO_N", "abc") shouldBe false             // not a Long
    overrides.lookup("KINOWO_N") shouldBe Some("12")      // unchanged
  }

  it should "reject an unknown or secret key" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val svc = service("web", new InMemoryEnvOverrideStore, registry)
    svc.set("KINOWO_UNKNOWN", "1") shouldBe false
    svc.set("ZYTE_API_KEY", "x")   shouldBe false         // even if it were registered, secret
  }

  "reset" should "clear an override for a known key and reject a secret one" in {
    val registry = new InMemoryEnvRegistryStore
    registry.publish("worker", Seq(RegisteredKnob("worker", "KINOWO_N", Env.Kind.Long, Some("5"), Some("5"))))
    val overrides = new InMemoryEnvOverrideStore
    overrides.set("KINOWO_N", "9")
    val svc = service("web", overrides, registry)
    svc.reset("KINOWO_N") shouldBe true
    overrides.lookup("KINOWO_N") shouldBe None
    svc.reset("TELEGRAM_BOT_TOKEN") shouldBe false
  }
}
