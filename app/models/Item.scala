package models

import play.api.libs.json._
import java.time.Instant

/** Core domain model for an Item. */
case class Item(
    id: Option[String],       // None until persisted; maps to Mongo _id
    name: String,
    description: String,
    price: BigDecimal,
    tags: List[String] = Nil,
    createdAt: Option[Instant] = None,
    updatedAt: Option[Instant] = None
)

object Item {

  /** JSON format used for API serialisation / deserialisation. */
  implicit val instantFormat: Format[Instant] =
    Format(
      Reads(js => js.validate[Long].map(Instant.ofEpochMilli)),
      Writes(i => JsNumber(i.toEpochMilli))
    )

  implicit val itemFormat: OFormat[Item] = Json.format[Item]
}

// ── Request / response shapes ──────────────────────────────────────────────────

/** Body accepted for POST /api/items */
case class CreateItemRequest(
    name: String,
    description: String,
    price: BigDecimal,
    tags: List[String] = Nil
)

object CreateItemRequest {
  implicit val format: OFormat[CreateItemRequest] = Json.format[CreateItemRequest]
}

/** Body accepted for PUT /api/items/:id (all fields optional — patch-style) */
case class UpdateItemRequest(
    name: Option[String],
    description: Option[String],
    price: Option[BigDecimal],
    tags: Option[List[String]]
)

object UpdateItemRequest {
  implicit val format: OFormat[UpdateItemRequest] = Json.format[UpdateItemRequest]
}
