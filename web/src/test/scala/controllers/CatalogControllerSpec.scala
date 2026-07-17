package controllers

import models.Catalog
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * `GET /api/catalog` serves the catalog JSON with a content ETag and honours
 * `If-None-Match`: the current ETag → `304 Not Modified` (no body); a stale or
 * absent one → a fresh `200`. This is what lets the apps revalidate on every
 * open while transferring only headers when nothing changed.
 */
class CatalogControllerSpec extends AnyFlatSpec with Matchers {

  private def controller = new CatalogController(stubControllerComponents())

  "GET /api/catalog" should "return the catalog JSON with an ETag and no-cache" in {
    val result = controller.catalog()(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("application/json")
    header("ETag", result) shouldBe Some(Catalog.etag)
    header("Cache-Control", result) shouldBe Some("no-cache")
    contentAsString(result) shouldBe Catalog.json
  }

  it should "answer 304 Not Modified when If-None-Match carries the current ETag" in {
    val result = controller.catalog()(FakeRequest().withHeaders("If-None-Match" -> Catalog.etag))
    status(result) shouldBe NOT_MODIFIED
    header("ETag", result) shouldBe Some(Catalog.etag)
    contentAsString(result) shouldBe ""
  }

  it should "return a fresh 200 when If-None-Match is stale" in {
    val result = controller.catalog()(FakeRequest().withHeaders("If-None-Match" -> "\"0000000000000000\""))
    status(result) shouldBe OK
    contentAsString(result) shouldBe Catalog.json
  }
}
