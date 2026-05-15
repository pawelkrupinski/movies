package scripts

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * One-shot: copy every doc from the legacy `enrichments` collection into
 * `movies`, SKIPPING any docId already present in `movies` (so production
 * writes that have already migrated are never overwritten by stale legacy
 * versions). The legacy collection is left in place — the deploy's dual-read
 * still reads it until a follow-up commit drops it; this script just front-
 * loads the migration so we don't wait on natural re-enrichment cycles to
 * eventually copy each row.
 *
 * Idempotent: re-running on a fully-migrated cluster prints "0 copied".
 *
 * Run: sbt "Test/runMain scripts.MoviesCollectionMigrate"
 */
object MoviesCollectionMigrate {
  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName    = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client    = MongoClient(uri)
    val db        = client.getDatabase(dbName)
    val legacy    = db.getCollection[Document]("enrichments")
    val primary   = db.getCollection[Document]("movies")
    val startedAt = System.currentTimeMillis()

    println(s"@@ source:      $dbName.enrichments")
    println(s"@@ destination: $dbName.movies")

    val primaryDocs = Await.result(primary.find().toFuture(), 60.seconds)
    val primaryIds: Set[String] = primaryDocs.iterator
      .flatMap(_.get("_id"))
      .flatMap(v => Try(v.asString().getValue).toOption)
      .toSet
    println(s"@@ ${primaryIds.size} doc(s) already in 'movies'")

    val legacyDocs = Await.result(legacy.find().toFuture(), 60.seconds)
    println(s"@@ ${legacyDocs.size} doc(s) in 'enrichments'")

    val toCopy = legacyDocs.filterNot { d =>
      d.get("_id").flatMap(v => Try(v.asString().getValue).toOption).exists(primaryIds.contains)
    }

    var copied      = 0
    var sampleShown = 0
    val SampleSize  = 15
    toCopy.foreach { d =>
      val id = d.get("_id").flatMap(v => Try(v.asString().getValue).toOption).getOrElse {
        println(s"@@ skipping legacy doc with non-string _id: ${d.get("_id")}")
        return
      }
      val title = d.get("title").flatMap(v => Try(v.asString().getValue).toOption).getOrElse("?")
      val year  = d.get("year").flatMap(v => Try(v.asInt32().getValue).toOption)
        .map(_.toString).getOrElse("—")
      Try {
        Await.result(
          primary.replaceOne(Filters.eq("_id", id), d, new ReplaceOptions().upsert(true)).toFuture(),
          10.seconds
        )
        copied += 1
        if (sampleShown < SampleSize) {
          sampleShown += 1
          println(s"@@ copied '$title' ($year)")
        }
      }.recover {
        case ex: Throwable => println(s"@@ FAILED to copy '$title' ($year): ${ex.getMessage}")
      }
    }
    if (copied > SampleSize) println(s"@@   (+ ${copied - SampleSize} more rows copied — same pattern)")

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    println()
    println("════ Summary ════")
    println(s"  legacy docs scanned:  ${legacyDocs.size}")
    println(s"  already in 'movies':  ${legacyDocs.size - toCopy.size}")
    println(s"  copied to 'movies':   $copied")
    println(f"  done in $elapsedSec%.1fs")

    client.close()
  }
}
