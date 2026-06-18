package controllers

import models.Cinema
import play.api.libs.json._
import play.api.mvc._
import services.movies.{MovieRepository, NormalizationReportRepository, RuleMergePreview, TitleNormalizer}
import services.titlerules.{RuleScope, TitleRule, TitleRuleKey, TitleRuleRecord, TitleRuleSet, TitleRulesRepository}

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
  titleRulesRepository: TitleRulesRepository,
  // On-demand corpus read only (the rule-merge preview). The web doesn't keep
  // the `movies` model warm, so the preview pulls a fresh snapshot from Mongo.
  movieRepository:      MovieRepository,
  reportRepository:     NormalizationReportRepository
) extends AbstractController(cc) {

  import AdminTitleRulesController._

  def index(): Action[AnyContent] = adminAction {
    Ok(views.html.admin.titleRulesEditor(titleRulesRepository.loadRecords().sortBy(recordSortKey), cinemaOptions))
  }

  def save(): Action[JsValue] = adminAction(parse.json) { request =>
    recordFromJson(request.body) match {
      case Left(err) => BadRequest(Json.obj("error" -> err))
      case Right(record) =>
        (record.rules ++ record.lastRules).find(!_.patternValid) match {
          case Some(bad) => BadRequest(Json.obj("error" -> s"Invalid regex: ${bad.pattern}"))
          case None      => titleRulesRepository.upsertRecord(record); Ok(recordToJson(record))
        }
    }
  }

  /** The realized outcome of the last rule-change backfill (written by the
   *  worker) — "what actually got merged / split / re-enriched". */
  def report(): Action[AnyContent] = adminAction {
    reportRepository.readLatest() match {
      case None => Ok(Json.obj("empty" -> true))
      case Some(r) => Ok(Json.obj(
        "atEpochMs" -> r.atEpochMs, "merges" -> r.merges,
        "splits" -> r.splits, "reEnriched" -> r.reEnriched))
    }
  }

  def delete(): Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "id").asOpt[String] match {
      case Some(id) => titleRulesRepository.deleteRecord(id); Ok(Json.obj("deleted" -> id))
      case None     => BadRequest(Json.obj("error" -> "missing id"))
    }
  }

  /** Given the full DRAFT rule list (flattened from the editor's records), report
   *  the rows that would NEWLY merge versus the currently-installed set.
   *  Read-only — nothing is persisted. */
  def preview(): Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "rules").validate[JsArray] match {
      case JsError(_) => BadRequest(Json.obj("error" -> "expected { rules: [...] }"))
      case JsSuccess(array, _) =>
        val parsed = array.value.toSeq.map(flatRuleFromJson)
        parsed.collectFirst { case Left(err) => err } match {
          case Some(err) => BadRequest(Json.obj("error" -> err))
          case None =>
            val draft   = TitleRuleSet(parsed.collect { case Right(r) => r })
            val entries = RuleMergePreview.entriesFrom(movieRepository.findAll())
            val merges  = RuleMergePreview.newMerges(TitleNormalizer.currentRules, draft, entries)
            Ok(Json.obj(
              "newMerges" -> merges.take(200).map(g => Json.obj(
                "display"      -> g.display,
                "displayTitle" -> g.displayTitle,
                "year"         -> g.year,
                "titles"       -> g.titles)),
              "newMergeCount" -> merges.size))
        }
    }
  }

  /** For the DRAFT rule list, report — per rule of the transient scope that
   *  DOESN'T rewrite the stored record (`GlobalStructural`) — which corpus
   *  titles it rewrites and to what. Drives the editor's per-rule unfoldable
   *  "affected films" list. Read-only — nothing is persisted. */
  def affected(): Action[JsValue] = adminAction(parse.json) { request =>
    (request.body \ "rules").validate[JsArray] match {
      case JsError(_) => BadRequest(Json.obj("error" -> "expected { rules: [...] }"))
      case JsSuccess(array, _) =>
        val parsed = array.value.toSeq.map(flatRuleFromJson)
        parsed.collectFirst { case Left(err) => err } match {
          case Some(err) => BadRequest(Json.obj("error" -> err))
          case None =>
            val draft  = TitleRuleSet(parsed.collect { case Right(r) => r })
            val titles = movieRepository.findAll().map(_.title).filter(_.nonEmpty)
            Ok(Json.obj(
              "affected" -> JsArray(draft.transientAffected(titles).map(affectedToJson)),
              "tiers"    -> JsArray(draft.transientTierAffected(titles).map(tierAffectedToJson))))
        }
    }
  }
}

object AdminTitleRulesController {
  /** Records sorted by scope (in the canonical tier order) then cinema, so the
   *  editor renders deterministically. */
  private def recordSortKey(record: TitleRuleRecord): (Int, String) =
    (RuleScope.all.indexOf(record.scope), record.cinemaId.getOrElse(""))

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

  /** Per-rule affected-titles payload for the editor. `count` is the full tally;
   *  `changes` is capped so a broad rule can't bloat the response. Each change
   *  carries the original, the rewritten search `result`, and the `display` form
   *  the page would actually render (the third preview column). */
  def affectedToJson(a: TitleRuleSet.RuleAffected): JsObject = Json.obj(
    "ruleId" -> a.ruleId,
    "scope"  -> a.scope.name,
    "count"  -> a.changes.size,
    "changes" -> a.changes.take(AffectedSampleCap).map(c =>
      Json.obj("title" -> c.original, "result" -> c.result, "display" -> displayOf(c.result))))

  /** Tier-level affected payload for the editor — the whole transient tier's net
   *  (original → final) rewrites, capped like the per-rule payload. */
  def tierAffectedToJson(t: TitleRuleSet.TierAffected): JsObject = Json.obj(
    "scope"  -> t.scope.name,
    "count"  -> t.changes.size,
    "changes" -> t.changes.take(AffectedSampleCap).map(c =>
      Json.obj("title" -> c.original, "result" -> c.result, "display" -> displayOf(c.result))))

  /** The on-page display title for an affected row's rewritten result — the same
   *  shared display ladder the merge preview / live pipeline use, which for a
   *  lone title reduces to its display casing (`recase`). Empty when the rule
   *  removed the title outright. */
  private def displayOf(result: String): String =
    if (result.nonEmpty) TitleNormalizer.chooseDisplay(Seq(result), result) else ""

  private val AffectedSampleCap = 500

  // ---- record JSON ----

  def recordToJson(record: TitleRuleRecord): JsObject = Json.obj(
    "id"        -> record.id,
    "scope"     -> record.scope.name,
    "cinemaId"  -> record.cinemaId,
    "rules"     -> record.rules.map(ruleToJson),
    "lastRules" -> record.lastRules.map(ruleToJson))

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
    val array    = (js \ field).asOpt[JsArray].map(_.value.toSeq).getOrElse(Nil)
    val parsed = array.zipWithIndex.map { case (rj, i) => recordRuleFromJson(rj, scope, cinemaId, i, last) }
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
