package scripts

import org.mongodb.scala.model.{Filters, Updates}
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import services.movies.{MongoSlotsRepository, MovieCodecs, StoredMovieDto}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Finish the slot migration: drop the embedded `sourceData` from every film whose
 * slots are already safely in `movie_slots`.
 *
 * `BackfillMovieSlots` filled the new collection but changed nothing in `movies` —
 * the embedded copy only goes when a film is next written, so a film nobody re-scrapes
 * keeps a full slot map indefinitely and the documents the change stream re-decodes
 * never shrink. This closes that out in one pass.
 *
 * THE SAFETY RULE, and the reason this is not a bare `$unset`: a film is only stripped
 * when `movie_slots` holds EVERY slot key its embedded map has. If the stored rows are
 * missing or fall short, the film is SKIPPED and reported — stripping it would leave it
 * with no cinemas anywhere, which is the one unrecoverable state this migration can
 * reach. Losing the copy is fine; losing the copy while the replacement is incomplete
 * is not.
 *
 * PACED on purpose. Each `$unset` is a change event, so an unpaced pass re-projects the
 * whole corpus as fast as Mongo will take it — thousands of films at once, into a worker
 * that is already the memory-tightest thing here. `--batch` / `--pause-ms` bound that.
 *
 *   flyctl proxy 27017:27017 --app kinowo-mongo &
 *   MONGODB_DB=kinowo_uk sbt "worker/Test/runMain scripts.RetireEmbeddedSlots --dry-run"
 *   MONGODB_DB=kinowo_uk sbt "worker/Test/runMain scripts.RetireEmbeddedSlots"
 */
object RetireEmbeddedSlots {

  case class Outcome(scanned: Int, stripped: Int, alreadyBare: Int, skipped: Seq[String])

  /** Decide, per film, whether the stored rows fully cover the embedded map. Pure, so the
   *  spec can drive every branch without Mongo. */
  def coversEmbedded(embeddedKeys: Set[String], storedKeys: Set[String]): Boolean =
    embeddedKeys.nonEmpty && embeddedKeys.subsetOf(storedKeys)

  def main(args: Array[String]): Unit = {
    val dryRun  = args.contains("--dry-run")
    val batch   = argInt(args, "--batch", 100)
    val pauseMs = argInt(args, "--pause-ms", 500)
    val uri     = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client  = MongoClient(uri)
    try {
      val db    = client.getDatabase(dbName)
      val coll  = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredMovieDto]("movies")
      val slots = new MongoSlotsRepository(Some(db))
      val stored = slots.findAllChecked() match {
        case (m, true)  => m
        case (_, false) =>
          println("  ABORT: could not read movie_slots completely — refusing to strip anything on a partial view.")
          sys.exit(2)
      }
      println(s"RetireEmbeddedSlots: $dbName${if (dryRun) " (dry run)" else ""} " +
        s"[batch=$batch pause=${pauseMs}ms]  movie_slots covers ${stored.size} film(s)")

      val docs = Await.result(coll.find().toFuture(), 300.seconds)
      var stripped = 0; var bare = 0; var n = 0
      val skipped = Seq.newBuilder[String]
      docs.foreach { dto =>
        n += 1
        // An already-retired row has no `sourceData` field at all (None) — same
        // "nothing embedded left to strip" case as an empty map, so the pass is
        // re-runnable over a partly-migrated corpus.
        val embedded = dto.sourceData.getOrElse(Map.empty).keySet
        if (embedded.isEmpty) bare += 1
        else if (!coversEmbedded(embedded, stored.getOrElse(dto._id, Map.empty).keySet)) skipped += dto._id
        else {
          if (!dryRun) {
            Try(Await.result(coll.updateOne(Filters.eq("_id", dto._id),
              Updates.combine(Updates.unset("sourceData"),
                Updates.set("slotsUpdatedAt", org.mongodb.scala.bson.BsonDateTime(java.time.Instant.now().toEpochMilli)))
            ).toFuture(), 30.seconds))
          }
          stripped += 1
          if (stripped % batch == 0) {
            println(s"  … $stripped stripped")
            if (!dryRun && pauseMs > 0) Thread.sleep(pauseMs.toLong)
          }
        }
      }
      val out = Outcome(n, stripped, bare, skipped.result())
      println(s"  scanned ${out.scanned}  ${if (dryRun) "would strip" else "stripped"} ${out.stripped}  " +
        s"already-bare ${out.alreadyBare}  skipped ${out.skipped.size}")
      if (out.skipped.nonEmpty) {
        println("  SKIPPED (movie_slots does not cover the embedded map — re-run BackfillMovieSlots first):")
        out.skipped.take(10).foreach(id => println(s"    $id"))
      }
    } finally client.close()
  }

  private def argInt(args: Array[String], flag: String, default: Int): Int =
    args.sliding(2).collectFirst { case Array(`flag`, v) => v.toIntOption }.flatten.getOrElse(default)
}
