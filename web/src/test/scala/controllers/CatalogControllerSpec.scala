package controllers

import models.Catalog
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * `GET /api/catalog` is the catalog JSON under the ETag the apps' bundled seed
 * carries (`Catalog.etag`, written by `CatalogSeedSpec`), revalidated on every
 * use. The conditional-GET mechanics themselves are [[FixedBodySpec]]'s; this
 * pins the contract the apps hold on to.
 */
class CatalogControllerSpec extends AnyFlatSpec with Matchers {

  private def controller = new CatalogController(stubControllerComponents())

  "GET /api/catalog" should "return the catalog JSON with the seed's ETag and no-cache" in {
    val result = controller.catalog()(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("application/json")
    header("ETag", result) shouldBe Some(Catalog.etag)
    header("Cache-Control", result) shouldBe Some("no-cache")
    contentAsString(result) shouldBe Catalog.json
  }

  // An app's stored tag, handed back the way a cache in between may spell it:
  // weakened, or offered beside another. Exact string equality answered both
  // with the whole catalog again.
  it should "answer 304 to the seed's ETag offered weak or in a list" in {
    status(controller.catalog()(FakeRequest().withHeaders("If-None-Match" -> s"W/${Catalog.etag}"))) shouldBe NOT_MODIFIED
    status(controller.catalog()(FakeRequest().withHeaders("If-None-Match" -> s""""0000000000000000", ${Catalog.etag}"""))) shouldBe NOT_MODIFIED
  }
}
