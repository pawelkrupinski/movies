package controllers

import models.{CreateItemRequest, Item}
import org.mockito.MockitoSugar
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.ItemService

import java.time.Instant
import scala.concurrent.Future

class ItemControllerSpec extends PlaySpec with GuiceOneAppPerTest with MockitoSugar with ScalaFutures {

  private val mockService = mock[ItemService]

  private val sampleItem = Item(
    id          = Some("64a1f2b3c4d5e6f7a8b9c0d1"),
    name        = "Widget",
    description = "A useful widget",
    price       = BigDecimal("9.99"),
    tags        = List("sale"),
    createdAt   = Some(Instant.now),
    updatedAt   = Some(Instant.now)
  )

  "ItemController GET /api/items" should {

    "return 200 with a list of items" in {
      when(mockService.listAll()).thenReturn(Future.successful(Seq(sampleItem)))

      val result = route(app, FakeRequest(GET, "/api/items")).get
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
    }
  }

  "ItemController POST /api/items" should {

    "return 201 on valid input" in {
      val req = CreateItemRequest("Widget", "A useful widget", BigDecimal("9.99"), List("sale"))
      when(mockService.create(req)).thenReturn(Future.successful(Right(sampleItem)))

      val result = route(
        app,
        FakeRequest(POST, "/api/items")
          .withJsonBody(Json.toJson(req))
      ).get
      status(result) mustBe CREATED
    }

    "return 400 on invalid JSON" in {
      val result = route(
        app,
        FakeRequest(POST, "/api/items")
          .withJsonBody(Json.obj("wrong" -> "body"))
      ).get
      status(result) mustBe BAD_REQUEST
    }
  }
}
