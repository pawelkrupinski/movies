package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import tools.ShareCardPool

import java.util.concurrent.CountDownLatch

/** The share cards render on ONE pool per process, owned by the composition root —
 *  the bound it exists to hold is per process, so a pool a constructor default
 *  quietly built beside it would double what a crawler can run at once. */
class ShareCardPoolWiringSpec extends AnyFlatSpec with Matchers {

  "the wiring's share cards" should "render on the wiring's one pool, and 503 once it is full" in {
    val wiring = new TestWebWiring {
      override lazy val shareCardPool: ShareCardPool = new ShareCardPool(threads = 1, queueDepth = 0)
    }
    val release = new CountDownLatch(1)
    try {
      wiring.shareCardPool.submit(release.await()) shouldBe defined
      status(wiring.movieController.ogImage("poznan", "Any Film")(FakeRequest())) shouldBe SERVICE_UNAVAILABLE
      status(wiring.movieController.cityOgImage("poznan")(FakeRequest())) shouldBe SERVICE_UNAVAILABLE
    } finally release.countDown()
  }
}
