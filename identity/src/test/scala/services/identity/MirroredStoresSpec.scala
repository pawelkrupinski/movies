package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.DebugMirror

/** The local prod mirror names the identity stores' collections without reaching the identity code: the names it
 *  copies are the ones the stores write. */
class MirroredStoresSpec extends AnyFlatSpec with Matchers {
  "the debug mirror" should "copy the collections the identity model and trace stores write" in {
    DebugMirror.IdentityFamilies shouldBe MongoIdentityModelStore.FamiliesCollection
    DebugMirror.IdentityTraces shouldBe MongoIdentityTraceStore.Collection
    DebugMirror.Collections should contain allOf (MongoIdentityModelStore.FamiliesCollection, MongoIdentityTraceStore.Collection)
  }

  it should "copy the staging screen's collection, which each country's worker writes" in {
    DebugMirror.IdentityStaging shouldBe staging.IdentityStaging.Collection
    DebugMirror.Collections should contain (staging.IdentityStaging.Collection)
  }
}
