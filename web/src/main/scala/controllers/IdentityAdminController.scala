package controllers

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._
import services.identity.ConfidenceCalibration.Calibration
import services.identity.{ConfidenceCalibration, Decision, Pin, PinClaim, Pins, ShadowDecisions}
import services.movies.ListingKey
import services.users.UserRepository

/**
 * `/admin/identity` — a read-only diagnostic of the identity resolver's shadow output, plus pin
 * create/remove for emergencies (docs/design/identity-resolver.md, "Phase 3: curation").
 *
 * It lists the decisions under constraint pressure (a must-link a cannot-link refused, an
 * ambiguous node left alone) and those the calibrated rating gate would withhold, each with the
 * resolver's own explanation. It is not a review queue: the resolver is expected to be right
 * without anyone looking, and a pin is the escape hatch for a case the evidence cannot decide.
 *
 * Gated by [[AdminAction]] (login + ADMIN_ALLOWLIST), like `/admin/config`. The pin POSTs are
 * `nocsrf` JSON (the page posts with `fetch`); `CrossSiteWriteFilter` refuses cross-site writes
 * (`RouteProtectionMatrixSpec`).
 */
class IdentityAdminController(cc: ControllerComponents, adminAction: AdminAction, users: UserRepository,
                              pins: Pins, shadow: ShadowDecisions) extends AbstractController(cc) {
  import IdentityAdminController._

  def index: Action[AnyContent] = adminAction { Ok(views.html.admin.identity(page(shadow, pins.all()))) }

  /** `{ kind: "is-film"|"same-film"|"never-film", tmdbId?, reason, listings: [listing…] }`. The
   *  author is the signed-in admin. 400 with the pin rules' refusal. */
  def createPin: Action[JsValue] = adminAction(parse.json) { request =>
    request.body.validate(using pinRequestReads) match {
      case JsError(_) => BadRequest(Json.obj("error" -> "expected { kind, tmdbId?, reason, listings }"))
      case JsSuccess((claim, reason, listings), _) =>
        val author = SignedInUser(request, users).flatMap(_.email).getOrElse("unknown")
        pins.add(listings, claim, author, reason) match {
          case Right(pin)    => Ok(Json.obj("id" -> pin.id))
          case Left(refusal) => BadRequest(Json.obj("error" -> refusal))
        }
    }
  }

  /** `{ id }`. 404 when no such pin is held. */
  def removePin: Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "id").asOpt[String].filter(pins.remove) match {
      case Some(_) => Ok(Json.obj("ok" -> true))
      case None    => NotFound(Json.obj("error" -> "no such pin"))
    }
  }
}

object IdentityAdminController {

  /** What the page renders. */
  final case class Page(contradicted: Seq[Decision], lowConfidence: Seq[Decision], calibration: Option[Calibration], pins: Seq[Pin])

  def page(shadow: ShadowDecisions, pins: Seq[Pin]): Page = {
    val calibration = ConfidenceCalibration.calibrate(shadow.verdicts())
    val decisions   = shadow.latest().sortBy(d => (d.confidence, d.listings.toSeq.sorted.headOption.map(_.toString)))
    Page(
      contradicted  = decisions.filter(_.contradictions.nonEmpty),
      lowConfidence = decisions.filter(d => calibration.exists(_.gates(d.confidence))),
      calibration   = calibration,
      pins          = pins)
  }

  def claimLabel(claim: PinClaim): String = claim match {
    case PinClaim.IsFilm(id)    => s"is film $id"
    case PinClaim.SameFilm      => "one film"
    case PinClaim.NeverFilm(id) => s"never film $id"
  }

  def listingJson(k: ListingKey): JsObject = k match {
    case ListingKey.Native(venue, page, raw) => Json.obj("venue" -> venue, "page" -> page, "rawTitle" -> raw)
    case ListingKey.Published(venue, raw, year, directors) =>
      Json.obj("venue" -> venue, "rawTitle" -> raw, "year" -> year, "directors" -> directors)
  }

  val listingReads: Reads[ListingKey] = (
    (__ \ "venue").read[String] and
    (__ \ "page").readNullable[String] and
    (__ \ "rawTitle").read[String] and
    (__ \ "year").readNullable[Int] and
    (__ \ "directors").readWithDefault[Seq[String]](Nil)
  ) { (venue, page, raw, year, directors) =>
    page.fold[ListingKey](ListingKey.Published(venue, raw, year, directors))(ListingKey.Native(venue, _, raw))
  }

  private val pinRequestReads: Reads[(PinClaim, String, Seq[ListingKey])] = (
    (__ \ "kind").read[String] and
    (__ \ "tmdbId").readNullable[Int] and
    (__ \ "reason").read[String] and
    (__ \ "listings").read(using Reads.seq(using listingReads))
  ).tupled.flatMap { case (kind, tmdbId, reason, listings) =>
    val claim = (kind, tmdbId) match {
      case ("is-film", Some(id))    => Some(PinClaim.IsFilm(id))
      case ("same-film", _)         => Some(PinClaim.SameFilm)
      case ("never-film", Some(id)) => Some(PinClaim.NeverFilm(id))
      case _                        => None
    }
    claim.fold[Reads[(PinClaim, String, Seq[ListingKey])]](Reads.failed("unknown kind, or a film kind without its tmdbId"))(
      c => Reads.pure((c, reason, listings)))
  }
}
