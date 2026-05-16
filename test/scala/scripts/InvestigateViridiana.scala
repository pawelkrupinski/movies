package scripts

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.collection.immutable.Document
import services.movies.MovieService
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Diagnostic: list every Mongo doc whose title contains "viridiana" (case-
 * and diacritic-insensitive). User reports 3 rows for what should be one
 * film — we want to see which `_id`s exist, how `(title, year)` differs,
 * which cinemas reported each one, and whether any normalisation /
 * docId-formula divergence is keeping them apart.
 *
 * Run: sbt "Test/runMain scripts.InvestigateViridiana"
 */
object InvestigateViridiana {
  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    val coll   = client.getDatabase(dbName).getCollection[Document]("movies")

    println(s"@@ scanning $dbName.movies for 'viridiana'")
    val all = Await.result(coll.find().toFuture(), 60.seconds)

    val matches = all.filter { d =>
      val title = Try(d.get("title").get.asString().getValue).toOption.getOrElse("")
      MovieService.normalize(title).contains("viridiana")
    }
    println(s"@@ ${matches.size} doc(s) match")
    println()

    matches.foreach { d =>
      val id           = Try(d.get("_id").get.asString().getValue).toOption.getOrElse("?")
      val title        = Try(d.get("title").get.asString().getValue).toOption.getOrElse("?")
      val year         = Option(d.get("year").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
      val tmdbId       = Option(d.get("tmdbId").orNull).flatMap(v => Try(v.asInt32().getValue).toOption)
      val imdbId       = Option(d.get("imdbId").orNull).flatMap(v => Try(v.asString().getValue).toOption)
      val imdbRating   = Option(d.get("imdbRating").orNull).flatMap(v => Try(v.asDouble().getValue).toOption)
      val originalTitle = Option(d.get("originalTitle").orNull).flatMap(v => Try(v.asString().getValue).toOption)
      val filmwebUrl   = Option(d.get("filmwebUrl").orNull).flatMap(v => Try(v.asString().getValue).toOption)
      val mcUrl        = Option(d.get("metacriticUrl").orNull).flatMap(v => Try(v.asString().getValue).toOption)
      val rtUrl        = Option(d.get("rottenTomatoesUrl").orNull).flatMap(v => Try(v.asString().getValue).toOption)

      val expectedId = s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"
      val idMatch    = if (id == expectedId) "OK" else s"MISMATCH (expected $expectedId)"

      println(s"── '$title' (${year.getOrElse("—")})")
      println(s"   _id=$id  [$idMatch]")
      println(s"   normalize(title)='${MovieService.normalize(title)}'")
      println(s"   tmdbId=${tmdbId.getOrElse("—")}  imdbId=${imdbId.getOrElse("—")}  imdbRating=${imdbRating.getOrElse("—")}")
      println(s"   originalTitle=${originalTitle.getOrElse("—")}")
      println(s"   filmwebUrl=${filmwebUrl.getOrElse("—")}")
      println(s"   metacriticUrl=${mcUrl.getOrElse("—")}")
      println(s"   rottenTomatoesUrl=${rtUrl.getOrElse("—")}")

      // cinemaScrapes — who reported this row, with what raw title + year
      val scrapes = Option(d.get("cinemaScrapes").orNull).flatMap(v => Try(v.asArray()).toOption)
      scrapes match {
        case Some(arr) if arr.size > 0 =>
          println(s"   cinemaScrapes:")
          import scala.jdk.CollectionConverters._
          arr.getValues.asScala.foreach { v =>
            val sub = v.asDocument()
            val c   = Try(sub.get("cinema").asString().getValue).toOption.getOrElse("?")
            val st  = Try(sub.get("title").asString().getValue).toOption.getOrElse("?")
            val sy  = Option(sub.get("year")).flatMap(v2 => Try(v2.asInt32().getValue).toOption)
            println(s"     - $c reported '$st' (${sy.getOrElse("—")})")
          }
        case _ =>
          println(s"   cinemaScrapes: (none)")
      }

      // cinemaShowings — which cinemas currently have a slot, with showtime count
      val showings = Option(d.get("cinemaShowings").orNull).flatMap(v => Try(v.asDocument()).toOption)
      showings match {
        case Some(doc) if !doc.isEmpty =>
          println(s"   cinemaShowings:")
          import scala.jdk.CollectionConverters._
          doc.entrySet().asScala.foreach { e =>
            val sub = Try(e.getValue.asDocument()).toOption
            val showtimeCount = sub
              .flatMap(s => Option(s.get("showtimes")).flatMap(v => Try(v.asArray()).toOption))
              .map(_.size)
              .getOrElse(0)
            println(s"     - ${e.getKey}: $showtimeCount showtime(s)")
          }
        case _ =>
          println(s"   cinemaShowings: (none)")
      }
      println()
    }

    client.close()
  }
}
