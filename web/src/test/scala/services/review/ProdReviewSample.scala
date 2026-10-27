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
 * NoCandidate) and matched. The file also holds the members' `movie_slots`, `identity_listings`, `venue_pages` and
 * `identity_traces` documents, which the retired review pages' cards read.
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
}
