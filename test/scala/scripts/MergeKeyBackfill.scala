package scripts

import models.MovieRecord
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.collection.immutable.Document
import services.enrichment.{MovieRepo, MovieService}
import services.lock.MongoLock
import tools.Env

import scala.concurrent.duration._

/**
 * Phase 2.3 backfill for the MovieCache transition.
 *
 * After `MovieService.normalize` switched to the corpus-independent
 * `TitleNormalizer.sanitize`, every existing row's docId in Mongo is the
 * OLD form (`{normalize-old}|{year}`). New writes go to the NEW form
 * (`{sanitize}|{year}`), so without a migration the old rows are orphans.
 *
 * This script:
 *   1. Holds a Mongo lock so the live app can't race the migration.
 *   2. Reads every existing row.
 *   3. Computes the new docId for each via the new normalize.
 *   4. Groups by (newDocId, year). When two old rows now share a docId,
 *      merges them field-by-field (prefer non-None, union cinemaTitles).
 *   5. Writes the merged record under the new docId. Deletes the old _ids.
 *
 * Idempotent — running it twice is safe (the second run finds rows already
 * under the new docIds and reports no changes).
 *
 * Run: sbt "Test/runMain scripts.MergeKeyBackfill"
 */
object MergeKeyBackfill {
  private val LockName  = "enrichment-merge-key-backfill"
  private val LockTtl   = 5.minutes

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set — nothing to backfill."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

    val client = MongoClient(uri)
    val db     = client.getDatabase(dbName)
    val coll   = db.getCollection[Document]("enrichments")
    val locks  = db.getCollection[Document]("locks")
    val lock   = new MongoLock(locks)
    val repo   = new MovieRepo()

    try {
      lock.withLock(LockName, LockTtl) {
        runBackfill(coll, repo)
      } match {
        case None    => println("Lock already held — backfill is running on another instance. Aborting.")
        case Some(_) => ()
      }
    } finally {
      repo.close()
      client.close()
    }
  }

  private def runBackfill(coll: org.mongodb.scala.MongoCollection[Document], repo: MovieRepo): Unit = {
    if (!repo.enabled) { println("Repo not enabled, aborting."); return }

    val startedAt = System.currentTimeMillis()
    val rows = repo.findAll()
    println(s"@@ ${rows.size} rows read from Mongo")

    // Group rows by their NEW docId. Multiple rows mapping to the same new
    // docId need merging; single rows just get re-written under the new key.
    val byNewKey: Map[String, Seq[(String, Option[Int], MovieRecord)]] =
      rows.groupBy { case (title, year, _) => newDocId(title, year) }

    var rewrittenSingleton = 0
    var mergedGroups       = 0
    var rowsMerged         = 0
    var unchanged          = 0
    val SampleSize         = 15
    var samplesShown       = 0

    byNewKey.foreach { case (newKey, group) =>
      // What was the OLD docId for each row in this group? They differ — the
      // whole point is that rows with different old docIds now collapse.
      val oldKeys = group.map { case (title, year, _) => oldDocId(title, year) }.distinct

      if (group.size == 1) {
        val (title, year, e) = group.head
        val oldKey = oldKeys.head
        if (oldKey == newKey) {
          unchanged += 1   // docId didn't change, nothing to do
        } else {
          // docId changed but no collision — delete the old row, write new.
          rewrittenSingleton += 1
          deleteByOldId(coll, oldKey)
          repo.upsert(title, year, e.copy(cinemaTitles = e.cinemaTitles + title))
          if (samplesShown < SampleSize) {
            samplesShown += 1
            println(s"@@ re-keyed   '$title' (${year.getOrElse("?")})   $oldKey  →  $newKey")
          }
        }
      } else {
        // Multiple old rows collapse into one new row.
        mergedGroups += 1
        rowsMerged   += group.size
        val (mergedTitle, mergedYear, mergedEnrichment) = mergeRows(group)
        // Delete every old _id in this group, then upsert the merged record.
        // `repo.upsert` recomputes the docId via the production normalize,
        // which is now the NEW form — so it writes under `newKey`.
        oldKeys.foreach(deleteByOldId(coll, _))
        deleteByOldId(coll, newKey) // idempotency on re-runs
        repo.upsert(mergedTitle, mergedYear, mergedEnrichment)
        if (samplesShown < SampleSize) {
          samplesShown += 1
          println(s"@@ MERGED ${group.size} rows → 1   key=$newKey   cleanTitle='$mergedTitle' (${mergedYear.getOrElse("?")})")
          group.foreach { case (t, _, _) => println(s"@@     · from: '$t'") }
          println(s"@@     · cinemaTitles=${mergedEnrichment.cinemaTitles}  imdb=${mergedEnrichment.imdbId}")
        }
      }
    }

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    println()
    println("════ Summary ════")
    println(s"  rows read:              ${rows.size}")
    println(s"  unchanged (already on new docId): $unchanged")
    println(s"  re-keyed (no collision):          $rewrittenSingleton")
    println(s"  merged groups:                    $mergedGroups  ($rowsMerged rows → $mergedGroups)")
    println(f"  done in $elapsedSec%.1fs")

    repo.close()
  }

  // ── docId helpers ────────────────────────────────────────────────────────

  private def newDocId(title: String, year: Option[Int]): String =
    s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"

  // The OLD normalize lived inline in `MovieService` — lowercase + NFD
  // diacritic strip + ł folding + whitespace collapse. We reproduce it here
  // verbatim so the script can compute the OLD docId for a given (title, year)
  // and delete the right row, even after the production code has moved on.
  private def oldNormalize(title: String): String = {
    val stripped = java.text.Normalizer
      .normalize(title, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
    stripped.toLowerCase
      .replace('ł', 'l').replace('Ł', 'l')
      .replaceAll("\\s+", " ").trim
  }

  private def oldDocId(title: String, year: Option[Int]): String =
    s"${oldNormalize(title)}|${year.map(_.toString).getOrElse("")}"

  // ── merge rules ──────────────────────────────────────────────────────────
  //
  // When multiple old rows collapse to one new docId, build a single
  // MovieRecord from them: union `cinemaTitles`, prefer the row whose
  // cleanTitle matches `originalTitle` for the surviving title, and take
  // non-None values for every other field (any source beats None).

  private def mergeRows(group: Seq[(String, Option[Int], MovieRecord)]): (String, Option[Int], MovieRecord) = {
    val year = group.head._2  // by construction, all rows in a group share `year`
    // Pick the cleanTitle that matches one of the rows' originalTitle when
    // possible (most canonical); otherwise the longest of the group's titles.
    val originalTitles = group.flatMap(_._3.originalTitle).toSet
    val titles = group.map(_._1).distinct
    val survivingTitle = titles
      .find(t => originalTitles.exists(_.equalsIgnoreCase(t)))
      .getOrElse(titles.maxBy(_.length))

    val unionVariants = group.flatMap { case (t, _, e) => e.cinemaTitles + t }.toSet
    val firstNonNoneEnrichment = group.map(_._3).reduce(mergeEnrichment)

    val merged = firstNonNoneEnrichment.copy(cinemaTitles = unionVariants)
    (survivingTitle, year, merged)
  }

  // Field-by-field: prefer `a`'s value if defined, else `b`'s. originalTitle
  // is a tiebreaker — when both rows have it set and they differ, keep `a`'s
  // (deterministic via the group iteration order).
  private def mergeEnrichment(a: MovieRecord, b: MovieRecord): MovieRecord =
    a.copy(
      imdbId            = a.imdbId.orElse(b.imdbId),
      imdbRating        = a.imdbRating.orElse(b.imdbRating),
      metascore         = a.metascore.orElse(b.metascore),
      originalTitle     = a.originalTitle.orElse(b.originalTitle),
      filmwebUrl        = a.filmwebUrl.orElse(b.filmwebUrl),
      filmwebRating     = a.filmwebRating.orElse(b.filmwebRating),
      rottenTomatoes    = a.rottenTomatoes.orElse(b.rottenTomatoes),
      tmdbId            = a.tmdbId.orElse(b.tmdbId),
      metacriticUrl     = a.metacriticUrl.orElse(b.metacriticUrl),
      rottenTomatoesUrl = a.rottenTomatoesUrl.orElse(b.rottenTomatoesUrl),
      cinemaTitles      = a.cinemaTitles ++ b.cinemaTitles
    )

  // Direct delete by `_id` — `repo.delete(title, year)` computes the docId
  // via the production normalize (NEW form), so it can't address rows still
  // sitting under their OLD docId.
  private def deleteByOldId(
    coll: org.mongodb.scala.MongoCollection[Document],
    _id:  String
  ): Unit = {
    import scala.concurrent.Await
    import org.mongodb.scala.model.Filters
    Await.ready(coll.deleteOne(Filters.eq("_id", _id)).toFuture(), 10.seconds)
    ()
  }
}
