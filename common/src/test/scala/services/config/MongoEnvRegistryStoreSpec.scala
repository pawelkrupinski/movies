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

  // The registry is republished every config-refresh tick (~30s). Prod capture
  // showed the old delete-all + insert-all rewrote all ~47 knobs every tick, 100%
  // byte-identical — ~46% of the worker's change-stream traffic, all redundant.
  // publish now reconciles against the stored slice, so an unchanged set writes
  // nothing.
  import EnvRegistryStore.reconcile

  private val knobA = RegisteredKnob("worker", "KINOWO_A", Env.Kind.Int, Some("1"), Some("1"))
  private val knobB = RegisteredKnob("worker", "KINOWO_B", Env.Kind.Str, None, Some("on"))

  "reconcile" should "be a no-op when the stored knobs already match" in {
    reconcile(Seq(knobA, knobB), Seq(knobB, knobA)).isNoOp shouldBe true // order-independent
  }

  it should "upsert only the knob whose value changed" in {
    val knobB2 = knobB.copy(current = Some("off"))
    val plan   = reconcile(Seq(knobA, knobB), Seq(knobA, knobB2))
    plan.deleteIds shouldBe empty
    plan.upserts   shouldBe Seq(knobB2)
  }

  it should "upsert a newly added knob and delete a key no longer read" in {
    val knobC = RegisteredKnob("worker", "KINOWO_C", Env.Kind.Long, Some("60"), Some("60"))
    val plan  = reconcile(Seq(knobA, knobB), Seq(knobA, knobC)) // B dropped, C added, A unchanged
    plan.deleteIds shouldBe Seq("worker|KINOWO_B")
    plan.upserts   shouldBe Seq(knobC)
  }
}
