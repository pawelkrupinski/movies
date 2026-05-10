package controllers

import models.{CreateItemRequest, UpdateItemRequest}
import play.api.libs.json._
import play.api.mvc._
import services.ItemService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ItemController @Inject() (
    cc: ControllerComponents,
    itemService: ItemService
)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  // ── GET /api/items ─────────────────────────────────────────────────────────
  def list: Action[AnyContent] = Action.async {
    itemService.listAll().map(items => Ok(Json.toJson(items)))
  }

  // ── POST /api/items ────────────────────────────────────────────────────────
  def create: Action[JsValue] = Action.async(parse.json) { request =>
    request.body.validate[CreateItemRequest] match {
      case JsError(errors) =>
        Future.successful(BadRequest(Json.obj("error" -> JsError.toJson(errors))))

      case JsSuccess(req, _) =>
        itemService.create(req).map {
          case Right(item) => Created(Json.toJson(item))
          case Left(msg)   => UnprocessableEntity(Json.obj("error" -> msg))
        }
    }
  }

  // ── GET /api/items/:id ─────────────────────────────────────────────────────
  def get(id: String): Action[AnyContent] = Action.async {
    itemService.getById(id).map {
      case Some(item) => Ok(Json.toJson(item))
      case None       => NotFound(Json.obj("error" -> s"Item $id not found"))
    }
  }

  // ── PUT /api/items/:id ─────────────────────────────────────────────────────
  def update(id: String): Action[JsValue] = Action.async(parse.json) { request =>
    request.body.validate[UpdateItemRequest] match {
      case JsError(errors) =>
        Future.successful(BadRequest(Json.obj("error" -> JsError.toJson(errors))))

      case JsSuccess(req, _) =>
        itemService.update(id, req).map {
          case Some(item) => Ok(Json.toJson(item))
          case None       => NotFound(Json.obj("error" -> s"Item $id not found"))
        }
    }
  }

  // ── DELETE /api/items/:id ──────────────────────────────────────────────────
  def delete(id: String): Action[AnyContent] = Action.async {
    itemService.delete(id).map {
      case true  => NoContent
      case false => NotFound(Json.obj("error" -> s"Item $id not found"))
    }
  }
}
