package services.review

import scala.jdk.CollectionConverters.*

import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.model.{Filters, Projections}
import services.identity.measures.ResolverDecision
import services.identity.{MongoIdentityModelStore, ResolverDecisionBson}

/** The review reads of the resolver's decisions and the listings' decision traces. */
trait MongoDecisionReads extends MongoReviewReads {
  import ReviewBson.idText

  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = {
    val out = Vector.newBuilder[ResolverDecision]
    scan(MongoIdentityModelStore.FamiliesCollection,
      if (unmatchedOnly) Filters.eq("decisions.film", null) else Filters.empty(),
      Projections.include("decisions"), tools.MongoReplies.Families) { batch =>
      batch.foreach(d => d.get("decisions").filter(_.isArray).foreach(_.asArray.getValues.asScala
        .foreach(v => out += ResolverDecisionBson.decode(v.asDocument))))
    }
    out.result()
  }

  def traces(listingKeys: Seq[String]): Map[String, services.identity.ListingTrace] =
    listingKeys.distinct.grouped(500).flatMap { keys =>
      await(collection(services.identity.MongoIdentityTraceStore.Collection).find(Filters.in("_id", keys*)).batchSize(tools.MongoReplies.Default).toFuture())
        .map(d => idText(d.toBsonDocument.get("_id")) -> services.identity.MongoIdentityTraceStore.decode(d.toBsonDocument))
    }.toMap
}
