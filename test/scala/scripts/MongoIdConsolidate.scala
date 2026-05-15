package scripts

import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import services.movies.MovieService
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * One-shot: collapse Mongo docs that the runtime merger keeps "merging" on
 * every restart without actually removing the loser docs. Two flavours
 * are cleaned up:
 *
 *   1. **Stale `_id` orphans at the same (title, year)** — a doc whose
 *      `_id` was computed with a prior `docId` formula (e.g.
 *      `"bez wyjscia|2025"`) sitting next to the current-formula doc
 *      (`"bezwyjscia|2025"`). `MovieRepo.upsert` only ever touches the
 *      current-formula one; the orphan stays forever until manually
 *      removed.
 *
 *   2. **Cross-year orphans on the same tmdbId** — e.g. for
 *      `Bez wyjścia` (tmdb=639988): a current-formula doc at year=2025
 *      plus a stale-formula doc at year=None. `mergeAll` correctly picks
 *      the year=Some survivor and calls `repo.delete` for the year=None
 *      loser, but `deleteOne` by `_id` doesn't match the legacy
 *      whitespace-preserving `_id` of the orphan.
 *
 * For each duplicate group the script picks a survivor (a doc whose
 * `_id` matches the current `MovieService.normalize(title)|year` formula,
 * preferring year=Some over year=None, then longest cleanTitle) and
 * `deleteMany`'s the rest by their exact `_id`s. Prints per-group before/
 * after for the first ~20 groups + a summary count.
 *
 * Idempotent: re-running after a clean Mongo finds no duplicates and
 * reports zero deletions.
 *
 * Run: sbt "Test/runMain scripts.MongoIdConsolidate"
 */
object MongoIdConsolidate {
  private case class Row(
    rawId:           String,
    title:           String,
    year:            Option[Int],
    tmdbId:          Option[Int],
    imdbId:          Option[String],
    cleanTitleLen:   Int,
    matchesCurrentDocId: Boolean
  )

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    val coll   = client.getDatabase(dbName).getCollection[Document]("movies")
    val startedAt = System.currentTimeMillis()

    println(s"@@ scanning $dbName.movies")
    val all = Await.result(coll.find().toFuture(), 60.seconds)
    println(s"@@ ${all.size} doc(s) total")

    val rows: Seq[Row] = all.flatMap { d =>
      val rawId = Try(d.get("_id").get.asString().getValue).toOption.getOrElse(return)
      val title = Try(d.get("title").get.asString().getValue).toOption.getOrElse(return)
      val year  = Option(d.get("year").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
      val tmdbId = Option(d.get("tmdbId").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
      val imdbId = Option(d.get("imdbId").orNull).flatMap(v => Try(v.asString().getValue).toOption)
      val expectedId = s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"
      Some(Row(rawId, title, year, tmdbId, imdbId, title.length, rawId == expectedId))
    }

    // Group by (identity, normalisedTitle). identity = tmdbId-or-imdbId so
    // films without TMDB/IMDb resolution still get deduped if duplicated.
    // normalisedTitle keeps us from collapsing cross-language rows that
    // share an identity but are intentionally separate (the runtime
    // merger's `isSibling` rule).
    val grouped = rows.groupBy { r =>
      val identity = r.tmdbId.map(id => s"tmdb:$id")
        .orElse(r.imdbId.map(id => s"imdb:$id"))
        .getOrElse(s"raw:${r.rawId}")
      (identity, MovieService.normalize(r.title))
    }

    val dupeGroups = grouped.filter { case (_, rs) => rs.size > 1 }
    println(s"@@ ${dupeGroups.size} duplicate group(s) (identity, normalised-title) with > 1 doc")
    println()

    var totalDeleted = 0
    var groupsShown  = 0
    val SampleSize   = 20
    dupeGroups.toSeq.sortBy { case ((id, t), _) => (id, t) }.foreach { case ((identity, _), groupRows) =>
      // Survivor preference: matches-current-docId > year=Some > longest title.
      // A clean docId-match guarantees future `repo.upsert` lands on this
      // very doc; the others would otherwise leak again on the next write.
      val survivor = groupRows.maxBy { r =>
        (r.matchesCurrentDocId, r.year.isDefined, r.cleanTitleLen, r.rawId)
      }
      val losers = groupRows.filterNot(_.rawId == survivor.rawId)
      if (losers.nonEmpty) {
        if (groupsShown < SampleSize) {
          groupsShown += 1
          println(s"── $identity  → keeping '${survivor.title}' (${survivor.year.getOrElse("—")})  _id=${survivor.rawId}")
          losers.foreach { l =>
            println(s"    delete _id=${l.rawId}  title='${l.title}' year=${l.year.getOrElse("—")}")
          }
          println()
        }
        val ids = losers.map(_.rawId)
        Try {
          val res = Await.result(coll.deleteMany(Filters.in("_id", ids: _*)).toFuture(), 30.seconds)
          totalDeleted += res.getDeletedCount.toInt
        }.recover {
          case ex: Throwable => println(s"@@ delete failed for $identity: ${ex.getMessage}")
        }
      }
    }
    if (dupeGroups.size > SampleSize)
      println(s"@@   (+ ${dupeGroups.size - SampleSize} more group(s) processed — same pattern)")

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    val afterCount = Await.result(coll.countDocuments().toFuture(), 30.seconds)
    println()
    println("════ Summary ════")
    println(s"  docs before:        ${all.size}")
    println(s"  duplicate groups:   ${dupeGroups.size}")
    println(s"  orphan docs deleted: $totalDeleted")
    println(s"  docs after:         $afterCount")
    println(f"  done in $elapsedSec%.1fs")

    client.close()
  }
}
