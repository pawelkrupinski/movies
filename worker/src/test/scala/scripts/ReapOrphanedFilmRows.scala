package scripts

import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, ObservableFuture, SingleObservableFuture}
import services.movies.{KeysetScan, MovieCodecs, MovieRepository, ScreeningsRepository, SlotsRepository, StoredMovieDto}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Reap `movie_slots` / `screenings` rows whose `movies` film is gone.
 *
 * Both side collections are keyed by `filmId` and are supposed to be emptied by
 * `MovieRepository.delete` / `deleteById` when their film leaves the corpus. One writer
 * never did: `MongoStagingFolder` deletes a group-merge loser with a direct in-transaction
 * `deleteOne` on `movies`, so its cinemas were left behind. Measured on prod PL
 * 2026-07-27: 888 `movie_slots` rows across 19 vanished films, plus 61 orphaned
 * `screenings` films from the same bypass predating the slot split.
 *
 * The fold now takes the side rows with it, so this closes out the rows already stranded
 * rather than a leak that is still running.
 *
 * SAFETY. Orphan status is decided against the live film-id set, and building that set is
 * the whole risk: a short read makes live films look deleted and would reap the cinemas of
 * films that are perfectly fine. So the corpus scan is keyset-paged and its completeness is
 * checked, and ANY shortfall aborts before a single delete. Deletes are also capped
 * (`--max`) so a surprise can only ever cost that many rows.
 *
 * Dry run by DEFAULT — pass `--apply` to delete.
 *
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   MONGODB_DB=kinowo    sbt "worker/Test/runMain scripts.ReapOrphanedFilmRows"
 *   MONGODB_DB=kinowo    sbt "worker/Test/runMain scripts.ReapOrphanedFilmRows --apply"
 */
object ReapOrphanedFilmRows {

  case class Orphans(collection: String, films: Seq[String], rows: Int)

  /** The `filmId`s in `sideFilmIds` that `liveFilmIds` does not contain. Pure, so the
   *  spec pins the one decision that matters without Mongo. */
  def orphanFilmIds(liveFilmIds: Set[String], sideFilmIds: Seq[String]): Seq[String] =
    sideFilmIds.filterNot(liveFilmIds.contains).sorted

  def main(args: Array[String]): Unit = {
    val apply  = args.contains("--apply")
    val max    = argInt(args, "--max", 5000)
    val uri    = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    try {
      val db     = client.getDatabase(dbName)
      val movies = db.withCodecRegistry(MovieCodecs.registry)
        .getCollection[StoredMovieDto](MovieRepository.Collection)

      // The live set, keyset-paged. `_id` only — the payload is irrelevant here and this
      // must not depend on a row decoding (the corpus this reaps from is the same one a
      // required-field DTO once made undecodable wholesale).
      val live      = Set.newBuilder[String]
      var liveCount = 0
      val complete  = KeysetScan.scan[StoredMovieDto](
        label          = "ReapOrphanedFilmRows corpus batch",
        batchSize      = 200,
        maxAttempts    = 4,
        initialBackoff = 500.millis,
        keyOf          = _._id,
        fetchPage      = (afterId, limit) => Await.result(
          movies.find(afterId.fold(Filters.empty())(Filters.gt("_id", _)))
            .sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds),
        onIncomplete   = e => println(s"  corpus scan failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
      )(batch => { live ++= batch.map(_._id); liveCount += batch.size })

      if (!complete) {
        println("  ABORT: the corpus scan fell short, so the live-film set is partial and every film it")
        println("  missed would look orphaned. Refusing to delete anything on a partial view.")
        sys.exit(2)
      }
      val liveIds = live.result()
      println(s"ReapOrphanedFilmRows: $dbName${if (apply) "" else " (dry run — pass --apply to delete)"}")
      println(s"  live films: $liveCount")

      val found = Seq(SlotsRepository.Collection, ScreeningsRepository.Collection).map { name =>
        val side    = db.getCollection(name)
        val orphans = orphanFilmIds(liveIds, Await.result(side.distinct[String]("filmId").toFuture(), 60.seconds))
        val rows    = if (orphans.isEmpty) 0
          else Await.result(side.countDocuments(Filters.in("filmId", orphans*)).toFuture(), 60.seconds).toInt
        println(s"  $name: ${orphans.size} orphaned film(s), $rows row(s)")
        orphans.take(5).foreach(f => println(s"    $f"))
        Orphans(name, orphans, rows)
      }

      val total = found.map(_.rows).sum
      if (total == 0) println("  nothing to reap.")
      else if (total > max) {
        println(s"  ABORT: $total row(s) exceeds --max $max. That is more than a merge-loser leak should")
        println("  produce — check the live set is right before raising the cap.")
        sys.exit(2)
      } else if (!apply) println(s"  would delete $total row(s). Re-run with --apply.")
      else {
        found.foreach { o =>
          if (o.films.nonEmpty) {
            val side: MongoCollection[Document] = db.getCollection(o.collection)
            val deleted = Await.result(
              side.deleteMany(Filters.in("filmId", o.films*)).toFuture(), 120.seconds).getDeletedCount
            println(s"  ${o.collection}: deleted $deleted row(s)")
          }
        }
      }
    } finally client.close()
  }

  private def argInt(args: Array[String], flag: String, default: Int): Int =
    args.sliding(2).collectFirst { case Array(`flag`, v) => v.toIntOption }.flatten.getOrElse(default)
}
