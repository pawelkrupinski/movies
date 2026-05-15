package scripts

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.collection.immutable.Document
import services.movies.MovieService
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Diagnostic: list every `(title, year)` in `movies` that has more than one
 * underlying `_id`. Confirms whether `MovieRepo.delete` is silently matching
 * zero docs because the `_id` formula at delete-time doesn't match the `_id`
 * legacy docs were inserted with.
 *
 * Also prints, per duplicate pair, what `MovieRepo.docId` would compute for
 * the row right now — so we can see at a glance whether either of the
 * existing `_id`s matches the formula. If both diverge, the cleanup needs to
 * happen via a title+year filter instead.
 *
 * Run: sbt "Test/runMain scripts.DuplicateDocIdAudit"
 */
object DuplicateDocIdAudit {
  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    val coll   = client.getDatabase(dbName).getCollection[Document]("movies")

    println(s"@@ scanning $dbName.movies")
    val all = Await.result(coll.find().toFuture(), 60.seconds)
    println(s"@@ ${all.size} doc(s) total")

    // ── (title, year) duplicates — would collapse on hydrate (same CacheKey) ──
    val groupedByTitleYear = all.groupBy { d =>
      val title = Try(d.get("title").get.asString().getValue).toOption.getOrElse("?")
      val year  = Option(d.get("year").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
      (title, year)
    }

    val titleYearDupes = groupedByTitleYear.filter { case (_, docs) => docs.size > 1 }
    println(s"══════ (title, year) duplicates ══════")
    println(s"@@ ${titleYearDupes.size} (title, year) group(s) with more than one doc")
    println(s"   (these collapse on hydrate; not what the merger is catching)")
    println()
    titleYearDupes.toSeq
      .sortBy { case ((t, _), _) => t.toLowerCase }
      .foreach { case ((title, year), docs) =>
        val expectedId = s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"
        println(s"── '$title' (${year.getOrElse("—")})  → docId-now-would-be: $expectedId")
        docs.foreach { d =>
          val id     = Try(d.get("_id").get.asString().getValue).toOption.getOrElse("?")
          val tmdbId = Option(d.get("tmdbId").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
          val matches = if (id == expectedId) "  (matches docId formula)" else "  (MISMATCH — delete by _id wouldn't find this)"
          println(s"    _id=$id  tmdb=${tmdbId.getOrElse("—")}$matches")
        }
        println()
      }

    // ── tmdbId duplicates — what IdentityMerger.mergeAll actually catches ──
    println()
    println(s"══════ tmdbId duplicates (different (title, year), same tmdbId) ══════")
    val byTmdb = all
      .filter(d => Option(d.get("tmdbId").orNull).flatMap(v => Try(v.asInt32().getValue).toOption).isDefined)
      .groupBy(d => d.get("tmdbId").get.asInt32().getValue)
    val tmdbDupes = byTmdb.filter { case (_, docs) =>
      docs.map { d =>
        val t = Try(d.get("title").get.asString().getValue).toOption.getOrElse("?")
        val y = Option(d.get("year").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
        (t, y)
      }.toSet.size > 1
    }
    println(s"@@ ${tmdbDupes.size} tmdbId group(s) spanning more than one (title, year)")
    println()
    tmdbDupes.toSeq.sortBy(_._1).foreach { case (tmdbId, docs) =>
      println(s"── tmdb=$tmdbId  (${docs.size} doc(s))")
      docs.foreach { d =>
        val id    = Try(d.get("_id").get.asString().getValue).toOption.getOrElse("?")
        val title = Try(d.get("title").get.asString().getValue).toOption.getOrElse("?")
        val year  = Option(d.get("year").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
        val expectedId = s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"
        val matches = if (id == expectedId) "  (matches docId formula)" else "  (MISMATCH — delete by _id wouldn't find this)"
        println(s"    _id=$id  title='$title' year=${year.getOrElse("—")}  expected=$expectedId$matches")
      }
      println()
    }

    client.close()
  }
}
