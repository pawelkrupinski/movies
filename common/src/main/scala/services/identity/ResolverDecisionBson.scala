package services.identity

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/** A [[ResolverDecision]] as BSON fields — the one encoding every identity collection storing a
 *  decision uses (`identity_model_families`). */
object ResolverDecisionBson {

  def encode(decision: ResolverDecision): BsonDocument = new BsonDocument()
    .append("members", ListingKeyBson.encodeAll(decision.members))
    .append("film", decision.film.fold[BsonValue](BsonNull())(BsonInt32(_)))
    .append("confidence", BsonDouble(decision.confidence))
    .append("basis", BsonString(decision.basis.toString))
    .append("explanation", BsonArray.fromIterable(decision.explanation.map(BsonString(_))))
    .append("contradictions", BsonArray.fromIterable(decision.contradictions.map(BsonString(_))))
    .append("fallback", decision.fallback.fold[BsonValue](BsonNull())(taken =>
      new BsonDocument("source", BsonString(taken.source)).append("id", BsonString(taken.id)).append("probability", BsonDouble(taken.probability))))
    .append("leaning", decision.leaning.fold[BsonValue](BsonNull())(lean =>
      new BsonDocument("film", BsonInt32(lean.film)).append("imdbNumber", BsonInt32(lean.imdbNumber))))
    .append("unanswered", BsonInt32(decision.unanswered))

  def decode(d: BsonDocument): ResolverDecision = {
    def strings(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asString.getValue)
    ResolverDecision(ListingKeyBson.decodeAll(d.getArray("members")), Option(d.get("film")).filter(_.isInt32).map(_.asInt32.getValue),
      d.getDouble("confidence").getValue, ResolverDecision.Basis.valueOf(d.getString("basis").getValue), strings("explanation"), strings("contradictions"),
      Option(d.get("fallback")).filter(_.isDocument).map(_.asDocument).map(taken =>
        ResolverDecision.Fallback(taken.getString("source").getValue, taken.getString("id").getValue, taken.getDouble("probability").getValue)),
      Option(d.get("leaning")).filter(_.isDocument).map(_.asDocument).map(lean =>
        ResolverDecision.Leaning(lean.getInt32("film").getValue, lean.getInt32("imdbNumber").getValue)),
      Option(d.get("unanswered")).filter(_.isInt32).fold(0)(_.asInt32.getValue))()
  }
}
