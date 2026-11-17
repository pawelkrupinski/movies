package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Dependency

/** What the agreement stage's takes read of TMDB afresh ([[TmdbReadTracking]]): a filing reaches the stage while the
 *  last PUBLISHED apply read it — not what an apply under way has read so far, and not what an earlier apply read. */
class TmdbReadTrackingSpec extends AnyFlatSpec with Matchers {
  private val find = AgreementStage.FindPrefix + "tt16315948"

  "A filing" should "reach the stage once the apply that read it published, and not before" in {
    val reads = new TmdbReadTracking
    reads.begin()
    reads.record(438631); reads.find("tt16315948"); reads.queried()
    Seq(AgreementStage.recordReadId(438631), find, Dependency.QueryFiled).map(reads.reads) shouldBe Seq(false, false, false)
    reads.publish()
    Seq(AgreementStage.recordReadId(438631), find, Dependency.QueryFiled).map(reads.reads) shouldBe Seq(true, true, true)
    Seq(AgreementStage.recordReadId(913760), AgreementStage.FindPrefix + "tt0000001").map(reads.reads) shouldBe Seq(false, false)
  }

  it should "reach it no more once a later apply published without reading it, the later apply's reads standing until then" in {
    val reads = new TmdbReadTracking
    reads.begin(); reads.record(438631); reads.queried(); reads.publish()
    reads.begin()
    reads.reads(AgreementStage.recordReadId(438631)) shouldBe true   // the next apply under way: the last one's stand
    reads.record(913760)
    reads.publish()
    Seq(AgreementStage.recordReadId(438631), AgreementStage.recordReadId(913760), Dependency.QueryFiled).map(reads.reads) shouldBe
      Seq(false, true, false)
  }
}
