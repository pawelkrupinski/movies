package controllers

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._
import services.identity.{Pin, PinClaim, Pins}
import services.movies.ListingKey
import services.users.UserRepository

/**
 * `/admin/identity` — the identity model's pins, created and removed for emergencies
 * (docs/design/identity-resolver.md, "Phase 3: curation"), and `/admin/identity/traces`, which rules
 * decided each listing. It is not a review queue: the resolver is expected to be right without anyone
 * looking, and a pin is the escape hatch for a case the evidence cannot decide.
 *
 * Gated by [[AdminAction]] (login + ADMIN_ALLOWLIST), like `/admin/config`. The pin POSTs are
 * `nocsrf` JSON (the page posts with `fetch`); `CrossSiteWriteFilter` refuses cross-site writes
 * (`RouteProtectionMatrixSpec`).
 */
class IdentityAdminController(cc: ControllerComponents, adminAction: AdminAction, users: UserRepository,
                              pins: Pins,
                              traces: services.identity.IdentityTraceReads = services.identity.IdentityTraceReads.Empty)
    extends AbstractController(cc) {
  import IdentityAdminController._

  /** `/admin/identity/traces` — the identity trace both ways: a rule's listings (`rule`), a film's (`film`), a title's
   *  (`title`), each with its rules and weighed evidence; asked nothing, every rule's count. */
  def traces(rule: Option[String], film: Option[Int], title: Option[String], blocker: Option[String] = None): Action[AnyContent] = adminAction {
    Ok(views.html.admin.identityTraces(tracesPage(traces, rule.map(_.trim).filter(_.nonEmpty), film, title.map(_.trim).filter(_.nonEmpty),
      blocker.map(_.trim).filter(_.nonEmpty))))
  }

  def index: Action[AnyContent] = adminAction { Ok(views.html.admin.identity(Page(pins.all()))) }

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

  /** How many listings a trace query shows. */
  val TraceLimit = 500

  /** What the trace page renders: the query, its listings — or, asked nothing, what keeps listings unresolved (the
   *  next wins, most listings first) and every rule's count. */
  final case class TracesPage(rule: Option[String], film: Option[Int], title: Option[String],
                              traces: Seq[services.identity.ListingTrace], counts: Seq[(String, Int)], limit: Int = TraceLimit,
                              blocker: Option[String] = None, blockers: Seq[services.identity.BlockerCount] = Nil) {
    def asked: Boolean = rule.isDefined || film.isDefined || title.isDefined || blocker.isDefined
  }

  def tracesPage(reads: services.identity.IdentityTraceReads, rule: Option[String], film: Option[Int], title: Option[String],
                 blocker: Option[String] = None): TracesPage = {
    val shown = rule.map(reads.byRule(_, TraceLimit)).orElse(film.map(reads.byFilm(_, TraceLimit)))
      .orElse(title.map(reads.byTitle(_, TraceLimit))).orElse(blocker.map(reads.byBlocker(_, TraceLimit))).getOrElse(Nil)
    val asked = rule.isDefined || film.isDefined || title.isDefined || blocker.isDefined
    TracesPage(rule, film, title, shown, if (asked) Nil else reads.ruleCounts(), blocker = blocker,
      blockers = if (asked) Nil else reads.blockers())
  }

  /** What the page renders. */
  final case class Page(pins: Seq[Pin])

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
