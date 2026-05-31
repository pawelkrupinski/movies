package clients.tools

import org.mongodb.scala.SingleObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import services.MongoConnection

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 *  One-shot: unset the `Cinema City Kinepolis` slot from every movie row
 *  in Mongo. The next `ShowtimeCache` tick refills with fresh, normalised
 *  showtimes from the now-fixed `CinemaCityClient`.
 *
 *  Reads `MONGODB_URI` / `MONGODB_DB` from env — source `.env.local`
 *  before running:
 *
 *  ```
 *  set -a; . ./.env.local; set +a
 *  sbt 'Test/runMain clients.tools.DropKinepolisSlots'
 *  ```
 */
object DropKinepolisSlots {
  def main(args: Array[String]): Unit = {
    val conn = MongoConnection.fromEnv(required = false)
    try {
      val db = conn.database.getOrElse {
        println("MONGODB_URI not set — nothing to do.")
        sys.exit(1)
      }
      val movies = db.getCollection("movies")
      val res = Await.result(
        movies.updateMany(
          Document(),
          Document("$unset" -> Document("sourceData.Cinema City Kinepolis" -> ""))
        ).toFuture(),
        30.seconds
      )
      println(s"matched=${res.getMatchedCount} modified=${res.getModifiedCount}")
    } finally conn.close()
  }
}
