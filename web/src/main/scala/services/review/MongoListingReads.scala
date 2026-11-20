package services.review

import java.time.Instant

import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Aggregates, Filters, Projections, Sorts}

/** The review reads of the listings themselves: their slot rows, their venues' own film pages and their feeds. */
trait MongoListingReads extends MongoReviewReads {
  import ReviewBson._

  def slots(listingKeys: Seq[String]): Map[String, SlotFacts] =
    listingKeys.distinct.grouped(500).flatMap { keys =>
      await(collection(ReviewCollections.Slots).find(Filters.in("listingKey", keys*))
        .projection(Projections.include("listingKey", "slot", "updatedAt")).batchSize(tools.MongoReplies.Default).toFuture())
        .flatMap { d =>
          val b = d.toBsonDocument
          for { key <- string(b, "listingKey"); slot <- doc(b, "slot") }
            yield key -> SlotFacts(facts(slot, poster = "posterUrl"), instant(b, "updatedAt").getOrElse(Instant.EPOCH))
        }
    }.toMap

  def updatedSince(since: Instant): Map[String, Instant] = {
    val out = Map.newBuilder[String, Instant]
    scan(ReviewCollections.Slots, Filters.and(Filters.gte("updatedAt", java.util.Date.from(since)), Filters.exists("listingKey")),
      Projections.include("listingKey", "updatedAt"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d => val b = d.toBsonDocument; for { k <- string(b, "listingKey"); at <- instant(b, "updatedAt") } out += k -> at }
    }
    out.result()
  }

  def venuePages(urls: Seq[String]): Map[String, VenueFacts] =
    urls.distinct.grouped(200).flatMap { batch =>
      await(collection(ReviewCollections.VenuePages).find(Filters.and(Filters.in("page", batch*), Filters.exists("gone", false)))
        .sort(Sorts.ascending("readAt")).batchSize(tools.MongoReplies.Default).toFuture())
        .flatMap { d => val b = d.toBsonDocument; string(b, "page").map(_ -> facts(b, poster = "posterUrl")) }
    }.toMap

  def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed] = {
    val byVenue = listings.distinct.groupMap(_._1)(_._2)
    if (byVenue.isEmpty) Map.empty
    else {
      // Only the listed films, and of their showtimes only the count and the span, leave the server. The films are
      // picked out of each venue's array before it is unwound: a venue's listing is its whole programme (US documents
      // average ~60 KB), and unwinding all of it to keep a few titles was most of a card page's build.
      def raw(film: String) = Document("$ifNull" -> org.mongodb.scala.bson.BsonArray(s"$film.movie.rawTitle", s"$film.movie.title"))
      val raws = org.mongodb.scala.bson.BsonArray.fromIterable(byVenue.values.flatten.toSeq.distinct.map(org.mongodb.scala.bson.BsonString(_)))
      val pipeline = Seq(
        Aggregates.`match`(Filters.in("_id", byVenue.keys.toSeq*)),
        Aggregates.project(Document("films" -> Document("$filter" -> Document("input" -> "$films", "as" -> "film",
          "cond" -> Document("$in" -> org.mongodb.scala.bson.BsonArray(raw("$$film"), raws)))))),
        Aggregates.unwind("$films"),
        Aggregates.project(Document(
          "raw" -> raw("$films"),
          "externalIds" -> "$films.externalIds",
          "poster" -> "$films.posterUrl",
          "screenings" -> Document("$size" -> Document("$ifNull" -> org.mongodb.scala.bson.BsonArray("$films.showtimes", org.mongodb.scala.bson.BsonArray()))),
          "first" -> Document("$min" -> "$films.showtimes.dateTime"),
          "last" -> Document("$max" -> "$films.showtimes.dateTime"))))
      await(collection(ReviewCollections.Listings).aggregate(pipeline).batchSize(tools.MongoReplies.Default).toFuture()).flatMap { d =>
        val b = d.toBsonDocument
        for { venue <- string(b, "_id"); raw <- string(b, "raw") if byVenue.get(venue).exists(_.contains(raw)) } yield
          (venue, raw) -> ListingFeed.of(b.get("externalIds"),
            Option(b.get("screenings")).filter(_.isInt32).fold(0)(_.asInt32.getValue),
            instant(b, "first").map(localTime), instant(b, "last").map(localTime), string(b, "poster"))
      }.toMap
    }
  }
}
