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
    .append("fallback", decision.fallback.fold[BsonValue](BsonNull())(taken => {
      val d = new BsonDocument("source", BsonString(taken.source)).append("id", BsonString(taken.id)).append("probability", BsonDouble(taken.probability))
      taken.title.foreach(title => d.append("title", BsonString(title)))
      taken.year.foreach(year => d.append("year", BsonInt32(year)))
      d
    }))
    .append("leaning", decision.leaning.fold[BsonValue](BsonNull())(lean =>
      new BsonDocument("film", BsonInt32(lean.film)).append("imdbNumber", BsonInt32(lean.imdbNumber))))
    .append("candidate", decision.candidate.fold[BsonValue](BsonNull())(best =>
      new BsonDocument("film", BsonInt32(best.film)).append("imdbNumber", BsonInt32(best.imdbNumber))))
    .append("unanswered", BsonInt32(decision.unanswered))
    .append("agreed", new BsonDocument(decision.agreed.toSeq.sorted.map { case (family, id) => new org.bson.BsonElement(family, BsonString(id)) }.asJava))

  def decode(d: BsonDocument): ResolverDecision = {
    def strings(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asString.getValue)
    ResolverDecision(ListingKeyBson.decodeAll(d.getArray("members")), Option(d.get("film")).filter(_.isInt32).map(_.asInt32.getValue),
      d.getDouble("confidence").getValue, ResolverDecision.Basis.valueOf(d.getString("basis").getValue), strings("explanation"), strings("contradictions"),
      Option(d.get("fallback")).filter(_.isDocument).map(_.asDocument).map(taken =>
        ResolverDecision.Fallback(taken.getString("source").getValue, taken.getString("id").getValue, taken.getDouble("probability").getValue,
          Option(taken.get("title")).filter(_.isString).map(_.asString.getValue), Option(taken.get("year")).filter(_.isInt32).map(_.asInt32.getValue))),
      leaningOf(d, "leaning"),
      Option(d.get("unanswered")).filter(_.isInt32).fold(0)(_.asInt32.getValue),
      Option(d.get("agreed")).filter(_.isDocument).fold(Map.empty[String, String])(_.asDocument.asScala.map { case (family, id) => family -> id.asString.getValue }.toMap),
      leaningOf(d, "candidate"))()
  }

  private def leaningOf(d: BsonDocument, name: String): Option[ResolverDecision.Leaning] =
    Option(d.get(name)).filter(_.isDocument).map(_.asDocument).map(lean =>
      ResolverDecision.Leaning(lean.getInt32("film").getValue, lean.getInt32("imdbNumber").getValue))
}
