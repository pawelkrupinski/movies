package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

/** The Mongo doc mapping must round-trip knobs with absent fields. A None default
 *  (a `get` knob) or None current (an unset knob) previously went through the
 *  Document builder as `null`, which throws — failing the whole insertMany batch
 *  and leaving env_registry empty, so the /admin/config page showed nothing. */
class MongoEnvRegistryStoreSpec extends AnyFlatSpec with Matchers {

  private val store = new MongoEnvRegistryStore(None) // no Mongo needed for the doc mapping

  "toDoc → fromDoc" should "round-trip a fully-populated knob" in {
    val k = RegisteredKnob("worker", "KINOWO_X", Env.Kind.Long, Some("60"), Some("15"))
    store.fromDoc(store.toDoc(k)) shouldBe Some(k)
  }

  it should "round-trip a knob with no current value (unset → None) without throwing" in {
    val k = RegisteredKnob("worker", "KINOWO_Y", Env.Kind.Int, Some("50"), None)
    store.fromDoc(store.toDoc(k)) shouldBe Some(k)
  }

  it should "round-trip a knob with no default (a get-knob) without throwing" in {
    val k = RegisteredKnob("web", "KINOWO_Z", Env.Kind.Str, None, Some("on"))
    store.fromDoc(store.toDoc(k)) shouldBe Some(k)
  }
}
