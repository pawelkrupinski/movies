package services.titlerules

import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** Persistence boundary for the editable title rules. Kept narrow so the
 *  business logic (compiling rules into a `TitleRuleSet`, installing them on
 *  `TitleNormalizer`, seeding defaults) lives in `TitleRulesCache` and is shared
 *  by the real Mongo path and the in-memory fake.
 *
 *  Rules are stored grouped into [[TitleRuleRecord]]s — one per cinema for
 *  `PerCinema`, one per global scope otherwise — so the editor can reorder by
 *  drag and keep a separate "last" list per record. `findAll` is derived: the
 *  domain still consumes a flat `Seq[TitleRule]`, which is just every record
 *  flattened. */
trait TitleRulesRepository {
  def enabled: Boolean
  def loadRecords(): Seq[TitleRuleRecord]
  def upsertRecord(rec: TitleRuleRecord): Unit
  def deleteRecord(id: String): Unit

  /** The flat rule list the domain consumes — every record's `toRules`. Derived
   *  here so both the Mongo and in-memory stores share one definition. */
  final def findAll(): Seq[TitleRule] = loadRecords().flatMap(_.toRules)

  /** Fire `onChange` whenever any record is inserted/updated/deleted out of band
   *  (the admin page on the web app writing while the worker watches, or vice
   *  versa), so the consumer reloads. Best-effort: a store that can't stream
   *  returns None and the caller relies on the periodic backstop reload. */
  def watchChanges(onChange: () => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/** One stored rule inside a record. Carries no `scope` / `cinemaId` / `order` —
 *  those are inherited from the enclosing record and the array position, so they
 *  can't drift out of sync. `last` is implicit in which array (`rules` vs
 *  `lastRules`) the entry sits in. */
case class StoredRule(
  id:          String,
  pattern:     String,
  replacement: String,
  applyAll:    Boolean,
  enabled:     Boolean,
  tag:         Option[String],
  note:        Option[String]
)

/** Mongo storage shape for a record — `RuleScope` flattened to its name so the
 *  macro codec only sees primitives, Option, and the nested `StoredRule`. `_id`
 *  is the record id (cinema key, or scope name for the global tiers). */
case class StoredTitleRuleRecord(
  _id:       String,
  scope:     String,
  cinemaId:  Option[String],
  rules:     Seq[StoredRule],
  lastRules: Seq[StoredRule]
)

object StoredTitleRuleRecord {
  private def ruleFromDomain(r: TitleRule): StoredRule =
    StoredRule(r.id, r.pattern, r.replacement, r.applyAll, r.enabled, r.tag, r.note)

  private def ruleToDomain(scope: RuleScope, cinemaId: Option[String], order: Int, last: Boolean)(s: StoredRule): TitleRule =
    TitleRule(s.id, scope, cinemaId, s.pattern, s.replacement, s.applyAll, order, last, s.enabled, s.tag, s.note)

  def fromDomain(rec: TitleRuleRecord): StoredTitleRuleRecord =
    StoredTitleRuleRecord(rec.id, rec.scope.name, rec.cinemaId,
      rec.rules.map(ruleFromDomain), rec.lastRules.map(ruleFromDomain))

  /** None when the stored `scope` isn't recognised (a forward-compat document from a
   *  newer schema) — such records are skipped rather than crashing the load. */
  def toDomain(s: StoredTitleRuleRecord): Option[TitleRuleRecord] =
    RuleScope.byName(s.scope).map { sc =>
      val rules     = s.rules.zipWithIndex.map     { case (r, i) => ruleToDomain(sc, s.cinemaId, i, last = false)(r) }
      val lastRules = s.lastRules.zipWithIndex.map { case (r, i) => ruleToDomain(sc, s.cinemaId, i, last = true)(r)  }
      TitleRuleRecord(s._id, sc, s.cinemaId, rules, lastRules)
    }
}

object TitleRuleCodecs {
  val registry: CodecRegistry = fromRegistries(
    fromProviders(
      Macros.createCodecProviderIgnoreNone[StoredRule](),
      Macros.createCodecProviderIgnoreNone[StoredTitleRuleRecord]()),
    DEFAULT_CODEC_REGISTRY
  )
}

/** In-memory rules store — the fallback when Mongo is disabled (tests, local
 *  runs without a DB) and the fake used in specs. No business logic: just a map.
 *  The real cache semantics come from `TitleRulesCache` wrapping this. */
class InMemoryTitleRulesRepository(initial: Seq[TitleRuleRecord] = Nil) extends TitleRulesRepository {
  private val store = new ConcurrentHashMap[String, TitleRuleRecord]()
  initial.foreach(r => store.put(r.id, r))

  def enabled: Boolean                      = true
  def loadRecords(): Seq[TitleRuleRecord]   = store.values().asScala.toSeq
  def upsertRecord(rec: TitleRuleRecord): Unit = { store.put(rec.id, rec); () }
  def deleteRecord(id: String): Unit        = { store.remove(id); () }
}
