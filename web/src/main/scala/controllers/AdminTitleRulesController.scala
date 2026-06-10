package controllers

import play.api.libs.json._
import play.api.mvc._
import services.movies.{MovieCache, RuleMergePreview, TitleNormalizer}
import services.titlerules.{RuleScope, TitleRule, TitleRuleSet, TitleRulesRepo}

import java.util.UUID

/** Admin page to edit the title-stripping rules. Gated by the login session +
 *  an explicit allowlist of user ids (`ADMIN_ALLOWLIST`). Writes go straight to
 *  the `titleRules` collection; the change stream propagates them to the worker
 *  and back to this process, so an edit takes effect on the next scrape and on
 *  the live display without a redeploy.
 *
 *  Reads use the cache's current corpus to preview which rows a draft rule set
 *  would merge — computed but NOT persisted until the user saves. */
class AdminTitleRulesController(
  cc:             ControllerComponents,
  titleRulesRepo: TitleRulesRepo,
  movieCache:     MovieCache,
  adminAllowlist: Set[String]
) extends AbstractController(cc) {

  import AdminTitleRulesController._

  private def authed(request: RequestHeader): Either[Result, String] =
    request.session.get("userId") match {
      case None                                       => Left(Unauthorized("Not logged in."))
      case Some(uid) if !adminAllowlist.contains(uid) => Left(Forbidden("Not an admin."))
      case Some(uid)                                  => Right(uid)
    }

  def index(): Action[AnyContent] = Action { request =>
    authed(request) match {
      case Left(deny) => deny
      case Right(_)   => Ok(views.html.admin.titleRulesEditor(titleRulesRepo.findAll().sortBy(sortKey)))
    }
  }

  def save(): Action[JsValue] = Action(parse.json) { request =>
    authed(request) match {
      case Left(deny) => deny
      case Right(_) =>
        ruleFromJson(request.body) match {
          case Left(err)   => BadRequest(Json.obj("error" -> err))
          case Right(rule) =>
            if (!rule.patternValid) BadRequest(Json.obj("error" -> s"Invalid regex: ${rule.pattern}"))
            else { titleRulesRepo.upsert(rule); Ok(ruleToJson(rule)) }
        }
    }
  }

  def delete(): Action[JsValue] = Action(parse.json) { request =>
    authed(request) match {
      case Left(deny) => deny
      case Right(_) =>
        (request.body \ "id").asOpt[String] match {
          case Some(id) => titleRulesRepo.delete(id); Ok(Json.obj("deleted" -> id))
          case None     => BadRequest(Json.obj("error" -> "missing id"))
        }
    }
  }

  /** Given the full DRAFT rule list, report the rows that would NEWLY merge
   *  versus the currently-installed set. Read-only — nothing is persisted. */
  def preview(): Action[JsValue] = Action(parse.json) { request =>
    authed(request) match {
      case Left(deny) => deny
      case Right(_) =>
        (request.body \ "rules").validate[JsArray] match {
          case JsError(_) => BadRequest(Json.obj("error" -> "expected { rules: [...] }"))
          case JsSuccess(arr, _) =>
            val parsed = arr.value.toSeq.map(ruleFromJson)
            parsed.collectFirst { case Left(err) => err } match {
              case Some(err) => BadRequest(Json.obj("error" -> err))
              case None =>
                val draft   = TitleRuleSet(parsed.collect { case Right(r) => r })
                val entries = RuleMergePreview.entriesFrom(movieCache.snapshot())
                val merges  = RuleMergePreview.newMerges(TitleNormalizer.currentRules, draft, entries)
                Ok(Json.obj(
                  "newMerges" -> merges.take(200).map(g => Json.obj(
                    "display" -> g.display,
                    "year"    -> g.year,
                    "titles"  -> g.titles)),
                  "newMergeCount" -> merges.size))
            }
        }
    }
  }
}

object AdminTitleRulesController {
  private def sortKey(r: TitleRule): (String, String, Int) =
    (r.scope.name, r.cinemaId.getOrElse(""), r.order)

  def ruleToJson(r: TitleRule): JsObject = Json.obj(
    "id" -> r.id, "scope" -> r.scope.name, "cinemaId" -> r.cinemaId,
    "pattern" -> r.pattern, "replacement" -> r.replacement, "applyAll" -> r.applyAll,
    "order" -> r.order, "enabled" -> r.enabled, "tag" -> r.tag, "note" -> r.note)

  def ruleFromJson(js: JsValue): Either[String, TitleRule] =
    for {
      scopeName <- (js \ "scope").asOpt[String].toRight("missing scope")
      scope     <- RuleScope.byName(scopeName).toRight(s"unknown scope $scopeName")
      pattern   <- (js \ "pattern").asOpt[String].filter(_.nonEmpty).toRight("missing pattern")
    } yield {
      val id = (js \ "id").asOpt[String].filter(_.nonEmpty)
        .getOrElse(s"${scope.name.toLowerCase}-${UUID.randomUUID().toString.take(8)}")
      TitleRule(
        id          = id,
        scope       = scope,
        cinemaId    = (js \ "cinemaId").asOpt[String].filter(_.nonEmpty),
        pattern     = pattern,
        replacement = (js \ "replacement").asOpt[String].getOrElse(""),
        applyAll    = (js \ "applyAll").asOpt[Boolean].getOrElse(false),
        order       = (js \ "order").asOpt[Int].getOrElse(100),
        enabled     = (js \ "enabled").asOpt[Boolean].getOrElse(true),
        tag         = (js \ "tag").asOpt[String].filter(_.nonEmpty),
        note        = (js \ "note").asOpt[String].filter(_.nonEmpty))
    }
}
