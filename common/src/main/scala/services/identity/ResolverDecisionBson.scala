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

  def decode(d: BsonDocument): ResolverDecision = read(new org.bson.BsonDocumentReader(d))

  /** The decision the reader is at, read field by field (see [[BsonFields]]). */
  def read(reader: org.bson.BsonReader): ResolverDecision = {
    import org.bson.BsonType
    var members: Seq[services.movies.ListingKey]                = null
    var film: Option[Int]                         = None
    var confidence: java.lang.Double              = null
    var basis: String                             = null
    var explanation, contradictions: Seq[String]  = null
    var fallback: Option[ResolverDecision.Fallback] = None
    var leaning, candidate: Option[ResolverDecision.Leaning] = None
    var unanswered                                = 0
    var agreed                                    = Map.empty[String, String]
    BsonFields.document(reader) {
      case "members"        => members = { val all = Vector.newBuilder[services.movies.ListingKey]; BsonFields.array(reader)(all += ListingKeyBson.read(reader)); all.result() }
      case "film"           => film = BsonFields.when(reader, BsonType.INT32)(reader.readInt32())
      case "confidence"     => confidence = BsonFields.number(reader, "confidence")
      case "basis"          => basis = reader.readString()
      case "explanation"    => explanation = BsonFields.strings(reader)
      case "contradictions" => contradictions = BsonFields.strings(reader)
      case "fallback"       => fallback = BsonFields.when(reader, BsonType.DOCUMENT)(readFallback(reader))
      case "leaning"        => leaning = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))
      case "candidate"      => candidate = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))
      case "unanswered"     => unanswered = BsonFields.when(reader, BsonType.INT32)(reader.readInt32()).getOrElse(0)
      case "agreed"         => agreed = BsonFields.when(reader, BsonType.DOCUMENT) {
                                 val all = Map.newBuilder[String, String]
                                 BsonFields.document(reader)(family => all += family -> reader.readString())
                                 all.result()
                               }.getOrElse(Map.empty)
      case _                => reader.skipValue()
    }
    ResolverDecision(BsonFields.required(members, "members"), film, BsonFields.required(confidence, "confidence").doubleValue,
      ResolverDecision.Basis.valueOf(BsonFields.required(basis, "basis")), BsonFields.required(explanation, "explanation"),
      BsonFields.required(contradictions, "contradictions"), fallback, leaning, unanswered, agreed, candidate)()
  }

  private def readFallback(reader: org.bson.BsonReader): ResolverDecision.Fallback = {
    var source, id: String           = null
    var probability: java.lang.Double = null
    var title: Option[String]        = None
    var year: Option[Int]            = None
    BsonFields.document(reader) {
      case "source"      => source = reader.readString()
      case "id"          => id = reader.readString()
      case "probability" => probability = BsonFields.number(reader, "probability")
      case "title"       => title = BsonFields.when(reader, org.bson.BsonType.STRING)(reader.readString())
      case "year"        => year = BsonFields.when(reader, org.bson.BsonType.INT32)(reader.readInt32())
      case _             => reader.skipValue()
    }
    ResolverDecision.Fallback(BsonFields.required(source, "source"), BsonFields.required(id, "id"),
      BsonFields.required(probability, "probability").doubleValue, title, year)
  }

  private def readLeaning(reader: org.bson.BsonReader): ResolverDecision.Leaning = {
    var film, imdbNumber: java.lang.Integer = null
    BsonFields.document(reader) {
      case "film"       => film = reader.readInt32()
      case "imdbNumber" => imdbNumber = reader.readInt32()
      case _            => reader.skipValue()
    }
    ResolverDecision.Leaning(BsonFields.required(film, "film").intValue, BsonFields.required(imdbNumber, "imdbNumber").intValue)
  }
}
