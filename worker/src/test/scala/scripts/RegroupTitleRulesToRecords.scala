package scripts

import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import services.titlerules.{RuleScope, StoredTitleRuleRecord, TitleRule, TitleRuleCodecs, TitleRuleRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * One-shot migration: regroup the legacy FLAT `titleRules` docs (one document
 * per rule, the original schema) into the new per-record shape (one document per
 * `(scope, cinema)` group, each holding ordered `rules` + `lastRules` arrays).
 *
 * Idempotent: a doc that already has a `rules` array is a record and is left
 * alone; only docs carrying a top-level `pattern` (i.e. legacy flat rules) are
 * read, regrouped via `TitleRuleRecord.fromRules`, written back as records, and
 * then deleted. Re-running after a successful pass finds no flat docs and is a
 * no-op.
 *
 * Run against prod over the tunnel:
 *   flyctl proxy 27017:27017 --app kinowo-mongo
 *   MONGODB_URI=... sbt "worker/Test/runMain scripts.RegroupTitleRulesToRecords"
 */
object RegroupTitleRulesToRecords {
  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client  = MongoClient(uri)
    val rawColl = client.getDatabase(dbName).getCollection[Document]("titleRules")
    val recColl = client.getDatabase(dbName)
      .withCodecRegistry(TitleRuleCodecs.registry)
      .getCollection[StoredTitleRuleRecord]("titleRules")

    println(s"@@ scanning $dbName.titleRules")
    val all = Await.result(rawColl.find().toFuture(), 60.seconds)
    println(s"@@ ${all.size} doc(s) total")

    // Legacy flat rule docs carry a top-level `pattern`; new record docs carry a
    // `rules` array instead.
    val (flat, records) = all.partition(d => d.get("pattern").isDefined && d.get("rules").isEmpty)
    println(s"@@ ${flat.size} legacy flat rule doc(s), ${records.size} already-record doc(s)")
    if (flat.isEmpty) { println("@@ nothing to migrate — already on the record schema."); client.close(); return }

    val rules    = flat.flatMap(toTitleRule)
    val regrouped = TitleRuleRecord.fromRules(rules)
    println(s"@@ regrouping ${rules.size} rule(s) into ${regrouped.size} record(s)")

    regrouped.foreach { rec =>
      Await.result(
        recColl.replaceOne(Filters.eq("_id", rec.id), StoredTitleRuleRecord.fromDomain(rec),
          new ReplaceOptions().upsert(true)).toFuture(), 30.seconds)
      println(s"   wrote record ${rec.id} (${rec.rules.size} rules, ${rec.lastRules.size} last)")
    }

    val flatIds = flat.flatMap(d => str(d, "_id"))
    flatIds.foreach(id => Await.result(rawColl.deleteOne(Filters.eq("_id", id)).toFuture(), 30.seconds))
    println(s"@@ deleted ${flatIds.size} legacy flat doc(s)")
    println("@@ done.")
    client.close()
  }

  private def toTitleRule(d: Document): Option[TitleRule] =
    for {
      id      <- str(d, "_id")
      scopeNm <- str(d, "scope")
      scope   <- RuleScope.byName(scopeNm)
      pattern <- str(d, "pattern")
    } yield TitleRule(
      id          = id,
      scope       = scope,
      cinemaId    = str(d, "cinemaId"),
      pattern     = pattern,
      replacement = str(d, "replacement").getOrElse(""),
      applyAll    = bool(d, "applyAll"),
      order       = int(d, "order").getOrElse(0),
      last        = bool(d, "last"), // legacy docs have no `last` → false
      enabled     = d.get("enabled").flatMap(v => Try(v.asBoolean().getValue).toOption).getOrElse(true),
      tag         = str(d, "tag"),
      note        = str(d, "note"))

  private def str(d: Document, k: String): Option[String] =
    d.get(k).flatMap(v => Try(v.asString().getValue).toOption).filter(_.nonEmpty)
  private def bool(d: Document, k: String): Boolean =
    d.get(k).flatMap(v => Try(v.asBoolean().getValue).toOption).getOrElse(false)
  private def int(d: Document, k: String): Option[Int] =
    d.get(k).flatMap(v => Try(v.asInt32().getValue).toOption)
}
