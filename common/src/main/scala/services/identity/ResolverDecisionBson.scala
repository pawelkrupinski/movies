package services.identity

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/** A [[ResolverDecision]] as BSON fields — the one encoding every identity collection storing a
 *  decision uses (`identity_shadow_decisions`, `identity_model_families`). */
object ResolverDecisionBson {

  def encode(decision: ResolverDecision): BsonDocument = new BsonDocument()
    .append("members", ListingKeyBson.encodeAll(decision.members))
    .append("film", decision.film.fold[BsonValue](BsonNull())(BsonInt32(_)))
    .append("confidence", BsonDouble(decision.confidence))
    .append("basis", BsonString(decision.basis.toString))
    .append("explanation", BsonArray.fromIterable(decision.explanation.map(BsonString(_))))
    .append("contradictions", BsonArray.fromIterable(decision.contradictions.map(BsonString(_))))

  def decode(d: BsonDocument): ResolverDecision = {
    def strings(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asString.getValue)
    ResolverDecision(ListingKeyBson.decodeAll(d.getArray("members")), Option(d.get("film")).filter(_.isInt32).map(_.asInt32.getValue),
      d.getDouble("confidence").getValue, ResolverDecision.Basis.valueOf(d.getString("basis").getValue), strings("explanation"), strings("contradictions"))
  }
}
