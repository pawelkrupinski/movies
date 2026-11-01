package services.review

import models.Country
import org.mongodb.scala.bson.BsonDocument
import services.identity.{ResolverDecision, ResolverDecisionBson}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * A REAL sample of prod's review inputs, recorded 2026-10-06 (`resources/review/prod-review-sample.json`, Mongo
 * extended JSON): per country database, ~10 `identity_model_families` documents — unmatched (BelowThreshold, Vetoed,
 * NoCandidate) and matched — and the `movie_slots` rows of their members' pages. `identity_listings` (the members'
 * listings, one film per document) and `venue_pages` (the members' pages) were added from the local prod mirror the
 * same day, for the venues' posters, and the families' `identity_traces` (read-only from prod) for a card's "Why".
 */
object ProdReviewSample {
  val Databases: Map[String, Country] = Map("kinowo" -> Country.Poland, "kinowo_uk" -> Country.UnitedKingdom,
    "kinowo_de" -> Country.Germany, "kinowo_us" -> Country.UnitedStates, "kinowo_es" -> Country.Spain)

  lazy val root: BsonDocument = org.bson.BsonDocument.parse(new String(
    Files.readAllBytes(Paths.get(getClass.getResource("/review/prod-review-sample.json").toURI)), StandardCharsets.UTF_8))

  /** `collection`'s documents in `db`. */
  def documents(db: String, collection: String): Seq[BsonDocument] =
    Option(root.getDocument(db).get(collection)).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asDocument))

  /** Every decision of the sample, with its country. */
  lazy val decisions: Seq[(Country, ResolverDecision)] = Databases.toSeq.sortBy(_._1).flatMap { case (db, country) =>
    documents(db, "identity_model_families").flatMap(_.getArray("decisions").getValues.asScala.map(v =>
      country -> ResolverDecisionBson.decode(v.asDocument)))
  }

  /** When the sample's newest slot row was written — a "now" the recently-matched page finds its rows by. */
  lazy val newestSlot: java.time.Instant = Databases.keys.toSeq.flatMap(documents(_, "movie_slots"))
    .flatMap(d => Option(d.get("updatedAt")).filter(_.isDateTime).map(v => java.time.Instant.ofEpochMilli(v.asDateTime.getValue))).max

  /** One country database of the sample as a review source: its decisions, and its slot rows by listing key. */
  def source(db: String): InMemoryReviewSource = {
    val slots = documents(db, "movie_slots").flatMap { d =>
      for {
        key  <- Option(d.get("listingKey")).filter(_.isString).flatMap(v => services.movies.ListingKey.parse(v.asString.getValue))
        slot <- Option(d.get("slot")).filter(_.isDocument).map(_.asDocument)
        at   <- Option(d.get("updatedAt")).filter(_.isDateTime).map(v => java.time.Instant.ofEpochMilli(v.asDateTime.getValue))
      } yield key -> SlotFacts(MongoReviewSource.facts(slot, poster = "posterUrl"), at)
    }.toMap
    val pages = documents(db, "venue_pages").flatMap { d =>
      Option(d.get("page")).filter(_.isString).map(_.asString.getValue -> MongoReviewSource.facts(d, poster = "posterUrl"))
    }.toMap
    new InMemoryReviewSource(decisions.filter(_._1 == Databases(db)).map(_._2), slots, pagesHeld = pages, feedsHeld = feeds(db),
      tracesHeld = traces(db))
  }

  /** The members' traces (`identity_traces`), decoded as the store decodes them. */
  def traces(db: String): Seq[services.identity.ListingTrace] =
    documents(db, "identity_traces").map(services.identity.MongoIdentityTraceStore.decode)

  /** The members' listings as the venues' last scrapes (`identity_listings`, one film per document) hold them. */
  private def feeds(db: String): Map[(String, String), ListingFeed] = {
    def text(d: BsonDocument, name: String) = Option(d.get(name)).filter(_.isString).map(_.asString.getValue)
    def local(v: org.bson.BsonValue) =
      java.time.LocalDateTime.ofInstant(java.time.Instant.ofEpochMilli(v.asDateTime.getValue), java.time.ZoneOffset.UTC).toString.replace("T", " ")
    documents(db, "identity_listings").flatMap { d =>
      val film  = d.getDocument("films")
      val movie = film.getDocument("movie")
      val times = Option(film.get("showtimes")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala)
        .flatMap(s => Option(s.asDocument.get("dateTime")).filter(_.isDateTime)).map(local).sorted
      for { venue <- text(d, "_id"); raw <- text(movie, "rawTitle").orElse(text(movie, "title")) }
        yield (venue, raw) -> ListingFeed(ListingFeed.catalogueIdsOf(film.get("externalIds")), times.size, times.headOption, times.lastOption,
          text(film, "posterUrl"))
    }.toMap
  }
}
