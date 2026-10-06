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
 * NoCandidate) and matched — and the `movie_slots` rows of their members' pages. `venue_pages` came back empty.
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
    new InMemoryReviewSource(decisions.filter(_._1 == Databases(db)).map(_._2), slots)
  }
}
