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
 *  by the real Mongo path and the in-memory fake. */
trait TitleRulesRepo {
  def enabled: Boolean
  def findAll(): Seq[TitleRule]
  def upsert(rule: TitleRule): Unit
  def delete(id: String): Unit

  /** Fire `onChange` whenever any rule is inserted/updated/deleted out of band
   *  (the admin page on the web app writing while the worker watches, or vice
   *  versa), so the consumer reloads. Best-effort: a store that can't stream
   *  returns None and the caller relies on the periodic backstop reload. */
  def watchChanges(onChange: () => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/** Mongo storage shape — `RuleScope` flattened to its name so the macro codec
 *  only sees primitives + Option. `_id` is the rule id. */
case class StoredTitleRule(
  _id:         String,
  scope:       String,
  cinemaId:    Option[String],
  pattern:     String,
  replacement: String,
  applyAll:    Boolean,
  order:       Int,
  enabled:     Boolean,
  tag:         Option[String],
  note:        Option[String]
)

object StoredTitleRule {
  def fromDomain(r: TitleRule): StoredTitleRule =
    StoredTitleRule(r.id, r.scope.name, r.cinemaId, r.pattern, r.replacement,
      r.applyAll, r.order, r.enabled, r.tag, r.note)

  /** None when the stored `scope` isn't recognised (a forward-compat doc from a
   *  newer schema) — such rows are skipped rather than crashing the load. */
  def toDomain(s: StoredTitleRule): Option[TitleRule] =
    RuleScope.byName(s.scope).map { sc =>
      TitleRule(s._id, sc, s.cinemaId, s.pattern, s.replacement, s.applyAll,
        s.order, s.enabled, s.tag, s.note)
    }
}

object TitleRuleCodecs {
  val registry: CodecRegistry = fromRegistries(
    fromProviders(Macros.createCodecProviderIgnoreNone[StoredTitleRule]()),
    DEFAULT_CODEC_REGISTRY
  )
}

/** In-memory rules store — the fallback when Mongo is disabled (tests, local
 *  runs without a DB) and the fake used in specs. No business logic: just a map.
 *  The real cache semantics come from `TitleRulesCache` wrapping this. */
class InMemoryTitleRulesRepo(initial: Seq[TitleRule] = Nil) extends TitleRulesRepo {
  private val store = new ConcurrentHashMap[String, TitleRule]()
  initial.foreach(r => store.put(r.id, r))

  def enabled: Boolean              = true
  def findAll(): Seq[TitleRule]     = store.values().asScala.toSeq
  def upsert(rule: TitleRule): Unit = store.put(rule.id, rule)
  def delete(id: String): Unit      = store.remove(id)
}
