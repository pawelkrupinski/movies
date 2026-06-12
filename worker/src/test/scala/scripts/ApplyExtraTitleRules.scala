package scripts

import services.movies.TitleNormalizer.normalize
import services.titlerules.{MongoTitleRulesRepo, TitleRuleDefaults, TitleRuleRecord, TitleRuleSet}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * Applies [[ExtraTitleRules]] (the audited post-baseline additions) to the prod
 * `titleRules` collection by APPENDING them to each affected scope's record. The
 * upsert trips the change stream, so the running worker re-installs the rules and
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
 * Idempotent: a rule whose id is already in the record is skipped, so a re-run
 * after a successful apply is a no-op.
 */
object ApplyExtraTitleRules {

  private val fixture = Paths.get("common/src/test/resources/fixtures/prod-movies/titles.txt")

  def main(args: Array[String]): Unit = {
    preview()
    if (args.contains("--apply")) apply()
    else println("\n[dry-run] no --apply flag — nothing written. Re-run with --apply to upsert to prod.")
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

  private def apply(): Unit = {
    val repo = new MongoTitleRulesRepo()
    if (!repo.enabled) { println("[apply] titleRules repo disabled — set MONGODB_URI. Aborting."); return }

    val current = repo.loadRecords()
    ExtraTitleRules.all.groupBy(_.scope).foreach { case (scope, extras) =>
      val recId    = TitleRuleRecord.idFor(scope, None)
      val existing = current.find(_.id == recId)
      // If the record is somehow missing, seed it from the frozen defaults for
      // this scope so we never write a record that DROPS the baseline rules.
      val baseRules = existing.map(_.rules)
        .getOrElse(TitleRuleDefaults.all.filter(r => r.scope == scope && !r.last))
      val lastRules = existing.map(_.lastRules)
        .getOrElse(TitleRuleDefaults.all.filter(r => r.scope == scope && r.last))

      val present = baseRules.map(_.id).toSet
      val toAdd   = extras.filterNot(r => present.contains(r.id))
      if (toAdd.isEmpty) println(s"[apply] [$recId] already current — nothing to add.")
      else {
        val merged = TitleRuleRecord(recId, scope, None, baseRules ++ toAdd, lastRules)
        repo.upsertRecord(merged)
        println(s"[apply] [$recId] appended ${toAdd.size} rules -> ${merged.rules.size} total " +
          s"(${toAdd.map(_.id).mkString(", ")})")
      }
    }
    repo.close()
    println("[apply] done. The worker's change stream will re-key + re-enrich the corpus; " +
      "check /admin/title-rules/report for the realized counts.")
  }

  private def mergeKey(rs: TitleRuleSet, t: String): String =
    tools.TextNormalization.deburr(rs.canonical(normalize(t)))
      .toLowerCase.replaceAll("[^\\p{L}\\p{N}]+", "")
}
