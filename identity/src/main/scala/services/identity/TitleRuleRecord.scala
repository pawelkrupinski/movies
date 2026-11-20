package services.identity

import models.Country
import play.api.libs.json.{JsArray, JsObject, JsString, Json}
import services.titles.TitleNormalizer
import services.titles.rules.{RuleScope, TitleRule, TitleRuleSet, TitleRuleTable}

import scala.util.Try

/**
 * The title rules a country's model decided its families under, as its store keeps them beside the rules version
 * ([[IncrementalResolver.restore]]): every rule of the country's set as it folds -- its placeholders expanded, its note
 * off, its order its place among the set's rules. Two sets that fold every title alike in every tier and at every venue
 * record alike: a row added to another country's rules, or a note edited, moves no country's record, while any rule a
 * country loads that is added, dropped, reordered or edited does.
 *
 * Kept as the rules themselves, not a digest: a restore under other rules rebuilds the normaliser they were decided
 * under ([[normalizer]]) to read again what each stored family read ([[TitleRuleChange]]).
 */
object TitleRuleRecord {

  /** `rules` as they fold. A rule's order only ranks it within its tier, or its venue's rules (`TitleRuleSet`, by order
   *  then id), so the record keeps each rule's rank in the whole set instead: within every tier and venue the same
   *  rules keep the same ranks whatever other countries' rows sit between them in the table. */
  def of(rules: TitleRuleSet): String =
    Json.stringify(JsArray(rules.effectiveRules.sortBy(rule => (rule.order, rule.id)).zipWithIndex.map { case (rule, rank) => encode(rule, rank) }))

  /** `like` -- the same country's facts and code -- under the rules `record` names; `None` for a record this code cannot
   *  read, which a restore takes as rules it cannot compare. */
  def normalizer(record: String, like: TitleNormalizer): Option[TitleNormalizer] =
    Try(Json.parse(record).as[Seq[JsObject]].map(decode)).toOption
      .map(rules => new TitleNormalizer(TitleRuleSet(rules, placeholders = Map.empty), facts = like.facts, country = like.country))

  /** The relay rows of the title-rule table, which `services.titles.RelayBanner` reads in every country alike, outside
   *  any country's normaliser: in the rules version itself ([[IncrementalResolver.rulesVersion]]), so a change to them
   *  re-resolves every family. */
  lazy val relayVersion: String =
    java.lang.Long.toHexString(services.identity.vocabulary.ContentHash.of(
      of(TitleRuleSet(TitleRuleTable.rows.filter(_.effect == TitleRuleTable.RelayEffect).map(_.rule)))))

  private def encode(rule: TitleRule, rank: Int): JsObject = JsObject(Seq(
    "id" -> JsString(rule.id), "scope" -> JsString(rule.scope.name), "pattern" -> JsString(rule.pattern),
    "replacement" -> JsString(rule.replacement), "all" -> Json.toJson(rule.applyAll), "rank" -> Json.toJson(rank),
    "enabled" -> Json.toJson(rule.enabled), "properName" -> Json.toJson(rule.writesProperName)) ++
    rule.cinemaId.map("cinema" -> JsString(_)) ++ rule.tag.map("tag" -> JsString(_)) ++
    rule.countries.map(countries => "countries" -> JsArray(countries.toSeq.map(_.code).sorted.map(JsString(_)))))

  private def decode(js: JsObject): TitleRule = {
    def text(field: String) = (js \ field).as[String]
    def flag(field: String) = (js \ field).as[Boolean]
    TitleRule(
      id = text("id"), scope = RuleScope.byName(text("scope")).get, cinemaId = (js \ "cinema").asOpt[String], pattern = text("pattern"),
      replacement = text("replacement"), applyAll = flag("all"), order = (js \ "rank").as[Int], enabled = flag("enabled"),
      tag = (js \ "tag").asOpt[String],
      countries = (js \ "countries").asOpt[Seq[String]].map(_.map(code => Country.all.find(_.code == code).get).toSet),
      writesProperName = flag("properName"))
  }
}
