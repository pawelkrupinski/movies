package controllers

import models.Cinema
import play.api.libs.json._
import play.api.mvc._
import services.movies.{MovieRepo, NormalizationReportRepo, RuleMergePreview, TitleNormalizer}
import services.titlerules.{RuleScope, TitleRule, TitleRuleKey, TitleRuleRecord, TitleRuleSet, TitleRulesRepo}

import java.util.UUID

/** Admin page to edit the title-stripping rules. Gated by [[AdminAction]] (login
 *  session + an `ADMIN_ALLOWLIST` of admin EMAILS). Writes go straight to the
 *  `titleRules` collection; the change stream propagates them to the worker and
 *  back to this process, so an edit takes effect on the next scrape and on the
 *  live display without a redeploy.
 *
 *  Rules are edited as [[TitleRuleRecord]]s — one card per cinema for
 *  `PerCinema`, one per global scope otherwise, each holding an ordered `rules`
 *  list and a separate ordered `lastRules` list. The preview still works on the
 *  flattened draft (`{ rules: [...] }`) and is read-only.
 *
 *  Reads use the cache's current corpus to preview which rows a draft rule set
 *  would merge — computed but NOT persisted until the user saves. */
class AdminTitleRulesController(
  cc:             ControllerComponents,
  adminAction:    AdminAction,
  titleRulesRepo: TitleRulesRepo,
  // On-demand corpus read only (the rule-merge preview). The web doesn't keep
  // the `movies` model warm, so the preview pulls a fresh snapshot from Mongo.
  movieRepo:      MovieRepo,
  reportRepo:     NormalizationReportRepo
) extends AbstractController(cc) {

  import AdminTitleRulesController._

  def index(): Action[AnyContent] = adminAction {
    Ok(views.html.admin.titleRulesEditor(titleRulesRepo.loadRecords().sortBy(recordSortKey), cinemaOptions))
  }

  def save(): Action[JsValue] = adminAction(parse.json) { request =>
    recordFromJson(request.body) match {
      case Left(err) => BadRequest(Json.obj("error" -> err))
      case Right(rec) =>
        (rec.rules ++ rec.lastRules).find(!_.patternValid) match {
          case Some(bad) => BadRequest(Json.obj("error" -> s"Invalid regex: ${bad.pattern}"))
          case None      => titleRulesRepo.upsertRecord(rec); Ok(recordToJson(rec))
        }
    }
  }

  /** The realized outcome of the last rule-change backfill (written by the
   *  worker) — "what actually got merged / split / re-enriched". */
  def report(): Action[AnyContent] = adminAction {
    reportRepo.readLatest() match {
      case None => Ok(Json.obj("empty" -> true))
      case Some(r) => Ok(Json.obj(
        "atEpochMs" -> r.atEpochMs, "merges" -> r.merges,
        "splits" -> r.splits, "reEnriched" -> r.reEnriched))
    }
  }

  def delete(): Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "id").asOpt[String] match {
      case Some(id) => titleRulesRepo.deleteRecord(id); Ok(Json.obj("deleted" -> id))
      case None     => BadRequest(Json.obj("error" -> "missing id"))
    }
  }

  /** Given the full DRAFT rule list (flattened from the editor's records), report
   *  the rows that would NEWLY merge versus the currently-installed set.
   *  Read-only — nothing is persisted. */
  def preview(): Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "rules").validate[JsArray] match {
      case JsError(_) => BadRequest(Json.obj("error" -> "expected { rules: [...] }"))
      case JsSuccess(arr, _) =>
        val parsed = arr.value.toSeq.map(flatRuleFromJson)
        parsed.collectFirst { case Left(err) => err } match {
          case Some(err) => BadRequest(Json.obj("error" -> err))
          case None =>
            val draft   = TitleRuleSet(parsed.collect { case Right(r) => r })
            val entries = RuleMergePreview.entriesFrom(movieRepo.findAll())
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

object AdminTitleRulesController {
  /** Records sorted by scope (in the canonical tier order) then cinema, so the
   *  editor renders deterministically. */
  private def recordSortKey(rec: TitleRuleRecord): (Int, String) =
    (RuleScope.all.indexOf(rec.scope), rec.cinemaId.getOrElse(""))

  /** Dropdown options for the per-cinema cards: distinct cinema KEY → human
   *  label. Chain branches (Cinema City / Helios / Multikino / BOK) collapse to
   *  one key, so we show the chain name rather than an arbitrary branch. */
  val cinemaOptions: Seq[(String, String)] =
    Cinema.all.groupBy(TitleRuleKey.of).toSeq
      .map { case (key, cinemas) => key -> chainLabel(key, cinemas) }
      .sortBy(_._2.toLowerCase)

  private def chainLabel(key: String, cinemas: Seq[Cinema]): String = key match {
    case "cinema-city" => "Cinema City"
    case "helios"      => "Helios"
    case "multikino"   => "Multikino"
    case "bok"         => "BOK (wszystkie)"
    case _             => cinemas.map(_.displayName).distinct.sorted.headOption.getOrElse(key)
  }

  // ---- record JSON ----

  def recordToJson(rec: TitleRuleRecord): JsObject = Json.obj(
    "id"        -> rec.id,
    "scope"     -> rec.scope.name,
    "cinemaId"  -> rec.cinemaId,
    "rules"     -> rec.rules.map(ruleToJson),
    "lastRules" -> rec.lastRules.map(ruleToJson))

  private def ruleToJson(r: TitleRule): JsObject = Json.obj(
    "id" -> r.id, "pattern" -> r.pattern, "replacement" -> r.replacement,
    "applyAll" -> r.applyAll, "enabled" -> r.enabled, "tag" -> r.tag, "note" -> r.note)

  def recordFromJson(js: JsValue): Either[String, TitleRuleRecord] =
    for {
      scopeName <- (js \ "scope").asOpt[String].toRight("missing scope")
      scope     <- RuleScope.byName(scopeName).toRight(s"unknown scope $scopeName")
      cinemaId   = (js \ "cinemaId").asOpt[String].filter(_.nonEmpty)
      _         <- Either.cond(scope != RuleScope.PerCinema || cinemaId.isDefined, (),
                     "a PerCinema record needs a cinema")
      rules     <- ruleList(js, "rules", scope, cinemaId, last = false)
      lastRules <- ruleList(js, "lastRules", scope, cinemaId, last = true)
    } yield TitleRuleRecord(TitleRuleRecord.idFor(scope, cinemaId), scope, cinemaId, rules, lastRules)

  private def ruleList(js: JsValue, field: String, scope: RuleScope,
                       cinemaId: Option[String], last: Boolean): Either[String, Seq[TitleRule]] = {
    val arr    = (js \ field).asOpt[JsArray].map(_.value.toSeq).getOrElse(Nil)
    val parsed = arr.zipWithIndex.map { case (rj, i) => recordRuleFromJson(rj, scope, cinemaId, i, last) }
    parsed.collectFirst { case Left(e) => e }.toLeft(parsed.collect { case Right(r) => r })
  }

  /** A rule inside a record — scope / cinema / order / last come from the record
   *  and the array position, not the rule's own JSON. */
  private def recordRuleFromJson(js: JsValue, scope: RuleScope, cinemaId: Option[String],
                                 order: Int, last: Boolean): Either[String, TitleRule] =
    (js \ "pattern").asOpt[String].filter(_.nonEmpty).toRight("missing pattern").map { pattern =>
      val id = (js \ "id").asOpt[String].filter(_.nonEmpty).getOrElse(mintId(scope))
      TitleRule(
        id          = id,
        scope       = scope,
        cinemaId    = cinemaId,
        pattern     = pattern,
        replacement = (js \ "replacement").asOpt[String].getOrElse(""),
        applyAll    = (js \ "applyAll").asOpt[Boolean].getOrElse(false),
        order       = order,
        last        = last,
        enabled     = (js \ "enabled").asOpt[Boolean].getOrElse(true),
        tag         = (js \ "tag").asOpt[String].filter(_.nonEmpty),
        note        = (js \ "note").asOpt[String].filter(_.nonEmpty))
    }

  /** Flat rule parse used only by the merge preview, which receives the editor's
   *  records already flattened to `{ scope, cinemaId, order, last, ... }`. */
  def flatRuleFromJson(js: JsValue): Either[String, TitleRule] =
    for {
      scopeName <- (js \ "scope").asOpt[String].toRight("missing scope")
      scope     <- RuleScope.byName(scopeName).toRight(s"unknown scope $scopeName")
      pattern   <- (js \ "pattern").asOpt[String].filter(_.nonEmpty).toRight("missing pattern")
    } yield TitleRule(
      id          = (js \ "id").asOpt[String].filter(_.nonEmpty).getOrElse(mintId(scope)),
      scope       = scope,
      cinemaId    = (js \ "cinemaId").asOpt[String].filter(_.nonEmpty),
      pattern     = pattern,
      replacement = (js \ "replacement").asOpt[String].getOrElse(""),
      applyAll    = (js \ "applyAll").asOpt[Boolean].getOrElse(false),
      order       = (js \ "order").asOpt[Int].getOrElse(100),
      last        = (js \ "last").asOpt[Boolean].getOrElse(false),
      enabled     = (js \ "enabled").asOpt[Boolean].getOrElse(true),
      tag         = (js \ "tag").asOpt[String].filter(_.nonEmpty),
      note        = (js \ "note").asOpt[String].filter(_.nonEmpty))

  private def mintId(scope: RuleScope): String =
    s"${scope.name.toLowerCase}-${UUID.randomUUID().toString.take(8)}"
}
