package services.identity

import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}
import services.identity.measures.{DecisionTrace, ResolverDecision}
import services.identity.vocabulary.{BsonFields, CandidateQuery, ListingKeyBson, Question}

/** A [[ResolverDecision]] as BSON fields — the one encoding every identity collection storing a
 *  decision uses (`identity_model_families`). */
object ResolverDecisionBson {

  def encode(decision: ResolverDecision): BsonDocument = encodeWith(decision, ListingKeyBson.encodeAll)

  /** `members` encodes the decision's members: flat as [[encode]] has them, or in a store's own layout
   *  ([[MongoIdentityModelStore.bucketed]]), which [[read]] reads back in their sorted order. */
  def encodeWith(decision: ResolverDecision, members: Seq[models.ListingKey] => BsonValue): BsonDocument = new BsonDocument()
    .append("members", members(decision.members))
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
    .append("awaits", BsonArray.fromIterable(decision.awaits.toSeq.flatMap(awaitedId).sorted.map(BsonString(_))))
    .append("agreed", new BsonDocument(decision.agreed.toSeq.sorted.map { case (family, id) => new org.bson.BsonElement(family, BsonString(id)) }.asJava))
    .append("trace", traceOf(decision))
    .pipe(d => if (decision.filmTitled.isEmpty) d
      else d.append("filmTitled", BsonArray.fromIterable(decision.filmTitled.toSeq.map(models.ListingKey.serialised).sorted.map(BsonString(_)))))

  /** A question a decision awaits, as stored: the model's own -- a search (`q|` its sort key), a film's record (`f|`), a
   *  venue page (`p|` its listing key) -- else the agreement's own id ([[Question.id]]). */
  private[identity] def awaitedId(question: Question): Option[String] = question match {
    case Question.Candidate(query)   => Some(s"q|${query.sortKey}")
    case Question.FilmRecord(tmdbId) => Some(s"f|$tmdbId")
    case Question.VenuePage(listing) => Some(s"p|${models.ListingKey.serialised(listing)}")
    case other                       => other.id.map("a|" + _)
  }
  private[identity] def awaitedOf(stored: String): Option[Question] = stored.split("\\|", 2) match {
    case Array("q", key)  => CandidateQuery.fromSortKey(key).map(Question.Candidate(_))
    case Array("f", id)   => id.toIntOption.map(Question.FilmRecord(_))
    case Array("p", key)  => models.ListingKey.parse(key).map(Question.VenuePage(_))
    case Array("a", id)   => Question.parse(id)
    case _                => None
  }

  /** What of the decision's trace the model keeps ([[DecisionTrace.kept]]): the pooled rule, and each member node's
   *  accepted rule and candidate, by the member's place among the decision's members sorted. */
  private def traceOf(decision: ResolverDecision): BsonValue = {
    val kept = DecisionTrace.kept(decision)
    if (kept == DecisionTrace.Empty) BsonNull()
    else {
      val sorted = decision.members.sorted(using models.ListingKey.ordering)
      val d = new BsonDocument("nodes", BsonArray.fromIterable(sorted.zipWithIndex.flatMap { case (key, at) =>
        kept.nodes.get(key).map(node => new BsonDocument("at", BsonInt32(at)).append("accepted", BsonString(node.accepted.get))
          .append("candidate", node.candidate.fold[BsonValue](BsonNull())(BsonInt32(_))))
      }))
      kept.pooled.foreach(rule => d.append("pooled", BsonString(rule)))
      d
    }
  }

  /** The kept trace [[traceOf]] wrote, of a decision with `members`. */
  private def keptTrace(d: BsonDocument, members: Seq[models.ListingKey]): DecisionTrace = {
    val sorted = members.sorted(using models.ListingKey.ordering)
    val nodes  = d.getArray("nodes").asScala.map(_.asDocument).flatMap { node =>
      sorted.lift(node.getInt32("at").getValue).map(key => key -> DecisionTrace.Node(Some(node.getString("accepted").getValue), Nil, Nil,
        candidate = Option(node.get("candidate")).filter(_.isInt32).map(_.asInt32.getValue)))
    }.toMap
    DecisionTrace(Option(d.get("pooled")).filter(_.isString).map(_.asString.getValue), None, nodes)
  }

  def decode(d: BsonDocument): ResolverDecision = read(new org.bson.BsonDocumentReader(d))

  /** The decision the reader is at, read field by field (see [[BsonFields]]). */
  def read(reader: org.bson.BsonReader): ResolverDecision = {
    val fields = new DecisionFields
    BsonFields.document(reader)(name => FieldReaders.get(name).fold(reader.skipValue())(_(fields, reader)))
    fields.decision
  }

  /** A decision's fields as [[read]] meets them, then the decision they make. */
  private final class DecisionFields {
    var members: Seq[models.ListingKey]                      = null
    var film: Option[Int]                                    = None
    var confidence: java.lang.Double                         = null
    var basis: String                                        = null
    var explanation, contradictions: Seq[String]             = null
    var fallback: Option[ResolverDecision.Fallback]          = None
    var leaning, candidate: Option[ResolverDecision.Leaning] = None
    var unanswered                                           = 0
    var awaits                                               = Set.empty[Question]
    var agreed                                               = Map.empty[String, String]
    var trace: Option[BsonDocument]                          = None
    var filmTitled                                           = Set.empty[models.ListingKey]

    def decision: ResolverDecision =
      ResolverDecision(BsonFields.required(members, "members"), film, BsonFields.required(confidence, "confidence").doubleValue,
        ResolverDecision.Basis.valueOf(BsonFields.required(basis, "basis")), BsonFields.required(explanation, "explanation"),
        BsonFields.required(contradictions, "contradictions"), fallback, leaning, unanswered, agreed, candidate, awaits, filmTitled)(
        trace.fold(DecisionTrace.Empty)(keptTrace(_, BsonFields.required(members, "members"))))
  }

  /** How each stored field is read into the decision's; any other is skipped. */
  private val FieldReaders: Map[String, (DecisionFields, org.bson.BsonReader) => Unit] = {
    import org.bson.BsonType
    Map(
      "members"        -> ((f, reader) => f.members = readMembers(reader)),
      "film"           -> ((f, reader) => f.film = BsonFields.when(reader, BsonType.INT32)(reader.readInt32())),
      "confidence"     -> ((f, reader) => f.confidence = BsonFields.number(reader, "confidence")),
      "basis"          -> ((f, reader) => f.basis = reader.readString()),
      "explanation"    -> ((f, reader) => f.explanation = BsonFields.strings(reader)),
      "contradictions" -> ((f, reader) => f.contradictions = BsonFields.strings(reader)),
      "fallback"       -> ((f, reader) => f.fallback = BsonFields.when(reader, BsonType.DOCUMENT)(readFallback(reader))),
      "leaning"        -> ((f, reader) => f.leaning = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))),
      "candidate"      -> ((f, reader) => f.candidate = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))),
      "unanswered"     -> ((f, reader) => f.unanswered = BsonFields.when(reader, BsonType.INT32)(reader.readInt32()).getOrElse(0)),
      "awaits"         -> ((f, reader) => f.awaits = BsonFields.when(reader, BsonType.ARRAY)(readAwaits(reader)).getOrElse(Set.empty)),
      "trace"          -> ((f, reader) => f.trace = BsonFields.when(reader, BsonType.DOCUMENT)(
                            new org.bson.codecs.BsonDocumentCodec().decode(reader, org.bson.codecs.DecoderContext.builder().build()))),
      "filmTitled"     -> ((f, reader) => f.filmTitled = BsonFields.strings(reader).flatMap(models.ListingKey.parse).toSet),
      "agreed"         -> ((f, reader) => f.agreed = BsonFields.when(reader, BsonType.DOCUMENT)(readAgreed(reader)).getOrElse(Map.empty)))
  }

  private def readMembers(reader: org.bson.BsonReader): Seq[models.ListingKey] = {
    val all = Vector.newBuilder[models.ListingKey]
    if (BsonFields.bucketed(reader)(all += ListingKeyBson.read(reader))) all.result().sorted(using models.ListingKey.ordering)
    else all.result()
  }
  private def readAwaits(reader: org.bson.BsonReader): Set[Question] = {
    val all = Set.newBuilder[Question]
    reader.readStartArray()
    while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) awaitedOf(reader.readString()).foreach(all += _)
    reader.readEndArray()
    all.result()
  }
  private def readAgreed(reader: org.bson.BsonReader): Map[String, String] = {
    val all = Map.newBuilder[String, String]
    BsonFields.document(reader)(family => all += family -> reader.readString())
    all.result()
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
