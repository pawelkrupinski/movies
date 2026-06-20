package scripts

import services.movies.TitleNormalizer.normalize
import services.titlerules.{MongoTitleRulesRepository, TitleRule, TitleRuleDefaults, TitleRuleRecord, TitleRuleSet}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * Applies [[ExtraTitleRules]] (the audited post-baseline additions) to the prod
 * `titleRules` collection — appending new rules and updating drifted ones in each
 * affected scope's record (see `plan`). The upsert trips the change stream, so
 * the running worker re-installs the rules and
 * `NormalizationRebuilder` + `reEnrichSearchChanges` re-key / re-enrich the
 * corpus — no redeploy.
 *
 * Dry-run BY DEFAULT: replays the checked-in prod-titles fixture through the seed
 * set vs seed + extras and prints exactly which titles change, so the blast
 * radius is visible before any write. Pass `--apply` to actually upsert.
 *
 *   flyctl proxy 27017:27017 --app kinowo-mongo
 *   # preview only:
 *   sbt "worker/Test/runMain scripts.ApplyExtraTitleRules"
 *   # write to prod:
 *   MONGODB_URI=... sbt "worker/Test/runMain scripts.ApplyExtraTitleRules --apply"
 *
 * Reconciling, not just additive: a NEW rule is appended, and an EXISTING rule
 * whose code pattern/replacement/note has DRIFTED from prod is UPDATED in place
 * (its order/last/enabled metadata preserved) — because [[ExtraTitleRules]] is
 * the version-controlled source for these records, so editing a rule there and
 * re-running is how a fix (e.g. excluding Ukrainian markers from the trailing-
 * format strip) reaches prod. Idempotent: once prod matches the code, a re-run
 * adds and updates nothing.
 */
object ApplyExtraTitleRules {

  /** One record to upsert: its reconciled form plus the ids it `added` (new
   *  rules) and `updated` (existing rules whose code content drifted). */
  private[scripts] case class Plan(record: TitleRuleRecord, added: Seq[String], updated: Seq[String])

  private val fixture = Paths.get("common/src/test/resources/fixtures/prod-movies/titles.txt")

  /** Parse `--update id1,id2,…` — the EXPLICIT allowlist of existing rule ids
   *  whose prod pattern should be overwritten with the code version. Updates are
   *  opt-in per id because prod legitimately diverges from the seed for many rules
   *  (the `GeneralizeSeparators` pass broadens `: ` → a separator class), so a
   *  blanket "reconcile every drifted rule" would REVERT those improvements. */
  private def updateIdsFrom(args: Array[String]): Set[String] =
    args.sliding(2).collectFirst { case Array("--update", ids) => ids }
      .orElse(args.collectFirst { case a if a.startsWith("--update=") => a.stripPrefix("--update=") })
      .map(_.split(",").map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)

  def main(args: Array[String]): Unit = {
    val updateIds = updateIdsFrom(args)
    preview()
    previewProdPlan(updateIds)
    if (args.contains("--apply")) apply(updateIds)
    else println("\n[dry-run] no --apply flag — nothing written. Re-run with --apply to upsert to prod.")
  }

  /** Show EXACTLY which records the upsert would add/update against the LIVE prod
   *  set (read-only) — the precise blast radius of this run, distinct from the
   *  fixture impact above. No-op when Mongo is unreachable (offline dry-run). */
  private def previewProdPlan(updateIds: Set[String]): Unit = {
    val repository = new MongoTitleRulesRepository()
    if (!repository.enabled) { println("[plan] MONGODB_URI unset — skipping live prod-plan preview."); return }
    val planned = plan(repository.loadRecords(), updateIds)
    repository.close()
    if (planned.isEmpty) println("[plan] prod already matches code — apply would write nothing.")
    else planned.foreach { case Plan(rec, added, updated) =>
      println(s"[plan] [${rec.id}] would add ${added.size} (${added.mkString(", ")}), " +
        s"update ${updated.size} (${updated.mkString(", ")})")
    }
  }

  /** Print every fixture title whose normalisation the additions change. */
  private def preview(): Unit = {
    val seed    = TitleRuleDefaults.ruleSet
    val withNew = TitleRuleSet(TitleRuleDefaults.all ++ ExtraTitleRules.all)
    if (!Files.exists(fixture)) {
      println(s"[preview] fixture $fixture absent — skipping impact preview " +
        "(run tools.SnapshotProdTitlesToFixture to refresh it).")
      return
    }
    val titles = Files.readAllLines(fixture).asScala.toSeq.filter(_.nonEmpty)

    val queryChanges = titles.filter(t => seed.search(t) != withNew.search(t))
      .map(t => (t, seed.search(t), withNew.search(t)))
    val mergeChanges = titles.filter(t => mergeKey(seed, t) != mergeKey(withNew, t))
      .map(t => (t, withNew.structural(t)))

    println(s"[preview] ${titles.size} prod titles replayed.")
    println(s"[preview] enrichment query changes (row kept, now resolves): ${queryChanges.size}")
    queryChanges.foreach { case (t, was, now) => println(s"    \"$t\"\n        query: \"$was\" -> \"$now\"") }
    println(s"[preview] merge-key changes (row collapses into base film): ${mergeChanges.size}")
    mergeChanges.foreach { case (t, base) => println(s"    \"$t\" -> merges as \"$base\"") }
  }

  private def apply(updateIds: Set[String]): Unit = {
    val repository = new MongoTitleRulesRepository()
    if (!repository.enabled) { println("[apply] titleRules repository disabled — set MONGODB_URI. Aborting."); return }

    val planned = plan(repository.loadRecords(), updateIds)
    if (planned.isEmpty) println("[apply] all records already current — nothing to add or update.")
    planned.foreach { case Plan(merged, added, updated) =>
      repository.upsertRecord(merged)
      val parts = Seq(
        Option.when(added.nonEmpty)(s"added ${added.size} (${added.mkString(", ")})"),
        Option.when(updated.nonEmpty)(s"updated ${updated.size} (${updated.mkString(", ")})")
      ).flatten.mkString("; ")
      println(s"[apply] [${merged.id}] $parts -> ${merged.rules.size} total")
    }
    repository.close()
    println("[apply] done. The worker's change stream will re-key + re-enrich the corpus; " +
      "check /admin/title-rules/report for the realized counts.")
  }

  /** Pure planner: the records to upsert (each with the ids it adds + updates),
   *  given the current prod records. Groups by `(scope, cinemaId)` — NOT scope
   *  alone — so a PerCinema rule lands in its own per-cinema record (`idFor` keys
   *  PerCinema by cinema slug), never a bogus single "PerCinema" record. A missing
   *  record is seeded from the frozen defaults for that exact `(scope, cinemaId)`
   *  so we never write a record that DROPS the baseline rules.
   *
   *  Reconciling: a code rule whose id is ABSENT is APPENDED. A rule whose id is
   *  present AND in `updateIds` AND whose pattern/replacement drifted from prod is
   *  UPDATED in place (its order/last/enabled metadata kept, so the rule's
   *  position is stable). Updates are an explicit opt-in allowlist, NOT every
   *  drifted rule: prod legitimately diverges from the seed for many rules (the
   *  `GeneralizeSeparators` pass broadens a literal `: ` into a separator class),
   *  so a blanket reconcile would revert those. Idempotent: a record with nothing
   *  to add AND nothing to update is omitted, so a re-run is a no-op. */
  private[scripts] def plan(current: Seq[TitleRuleRecord], updateIds: Set[String] = Set.empty): Seq[Plan] =
    ExtraTitleRules.all.groupBy(r => (r.scope, r.cinemaId)).toSeq
      .sortBy { case ((scope, cinemaId), _) => (scope.name, cinemaId.getOrElse("")) }
      .flatMap { case ((scope, cinemaId), extras) =>
        val recordId = TitleRuleRecord.idFor(scope, cinemaId)
        val existing = current.find(_.id == recordId)
        val baseRules = existing.map(_.rules)
          .getOrElse(TitleRuleDefaults.all.filter(r => r.scope == scope && r.cinemaId == cinemaId && !r.last))
        val lastRules = existing.map(_.lastRules)
          .getOrElse(TitleRuleDefaults.all.filter(r => r.scope == scope && r.cinemaId == cinemaId && r.last))

        val byId  = extras.map(r => r.id -> r).toMap
        // Update only an EXPLICITLY allowlisted id whose code content (pattern,
        // replacement, OR note) drifted; keep the stored order/last/enabled
        // metadata. The note is synced too — for a hand-picked id that's the point
        // (a fixed rule's doc shouldn't lie); the allowlist gate is what stops the
        // note from flagging the dozens of cosmetically-divergent rules prod
        // carries from `GeneralizeSeparators`, since none of those are listed.
        def drifted(cur: TitleRule): Boolean =
          updateIds.contains(cur.id) &&
            byId.get(cur.id).exists(code => (cur.pattern, cur.replacement, cur.note) != (code.pattern, code.replacement, code.note))
        val updated = baseRules.filter(drifted).map(_.id)
        val reconciledBase = baseRules.map { cur =>
          if (drifted(cur)) cur.copy(pattern = byId(cur.id).pattern, replacement = byId(cur.id).replacement, note = byId(cur.id).note)
          else cur
        }
        val present = baseRules.map(_.id).toSet
        val toAdd   = extras.filterNot(r => present.contains(r.id))
        if (toAdd.isEmpty && updated.isEmpty) None
        else Some(Plan(
          TitleRuleRecord(recordId, scope, cinemaId, reconciledBase ++ toAdd, lastRules),
          added = toAdd.map(_.id), updated = updated))
      }

  private def mergeKey(rs: TitleRuleSet, t: String): String =
    tools.TextNormalization.deburr(rs.canonical(normalize(t)))
      .toLowerCase.replaceAll("[^\\p{L}\\p{N}]+", "")
}
