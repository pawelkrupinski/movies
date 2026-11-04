package services.identity

import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/** A [[ResolverDecision]] as BSON fields — the one encoding every identity collection storing a
 *  decision uses (`identity_model_families`). */
object ResolverDecisionBson {

  def encode(decision: ResolverDecision): BsonDocument = encodeWith(decision, ListingKeyBson.encodeAll)

  /** `members` encodes the decision's members: flat as [[encode]] has them, or in a store's own layout
   *  ([[MongoIdentityModelStore.bucketed]]), which [[read]] reads back in their sorted order. */
  def encodeWith(decision: ResolverDecision, members: Seq[services.movies.ListingKey] => BsonValue): BsonDocument = new BsonDocument()
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

  /** A question a decision awaits, as stored: the model's own -- a search (`q|` its sort key), a film's record (`f|`), a
   *  venue page (`p|` its listing key) -- else the agreement's own id ([[Question.id]]). */
  private[identity] def awaitedId(question: Question): Option[String] = question match {
    case Question.Candidate(query)   => Some(s"q|${query.sortKey}")
    case Question.FilmRecord(tmdbId) => Some(s"f|$tmdbId")
    case Question.VenuePage(listing) => Some(s"p|${services.movies.ListingKey.serialised(listing)}")
    case other                       => other.id.map("a|" + _)
  }
  private[identity] def awaitedOf(stored: String): Option[Question] = stored.split("\\|", 2) match {
    case Array("q", key)  => CandidateQuery.fromSortKey(key).map(Question.Candidate(_))
    case Array("f", id)   => id.toIntOption.map(Question.FilmRecord(_))
    case Array("p", key)  => services.movies.ListingKey.parse(key).map(Question.VenuePage(_))
    case Array("a", id)   => Question.parse(id)
    case _                => None
  }

  /** What of the decision's trace the model keeps ([[DecisionTrace.kept]]): the pooled rule, and each member node's
   *  accepted rule and candidate, by the member's place among the decision's members sorted. */
  private def traceOf(decision: ResolverDecision): BsonValue = {
    val kept = DecisionTrace.kept(decision)
    if (kept == DecisionTrace.Empty) BsonNull()
    else {
      val sorted = decision.members.sorted(using services.movies.ListingKey.ordering)
      val d = new BsonDocument("nodes", BsonArray.fromIterable(sorted.zipWithIndex.flatMap { case (key, at) =>
        kept.nodes.get(key).map(node => new BsonDocument("at", BsonInt32(at)).append("accepted", BsonString(node.accepted.get))
          .append("candidate", node.candidate.fold[BsonValue](BsonNull())(BsonInt32(_))))
      }))
      kept.pooled.foreach(rule => d.append("pooled", BsonString(rule)))
      if (kept.namesake) d.append("namesake", org.bson.BsonBoolean.TRUE)
      d
    }
  }

  /** The kept trace [[traceOf]] wrote, of a decision with `members`. */
  private def keptTrace(d: BsonDocument, members: Seq[services.movies.ListingKey]): DecisionTrace = {
    val sorted = members.sorted(using services.movies.ListingKey.ordering)
    val nodes  = d.getArray("nodes").asScala.map(_.asDocument).flatMap { node =>
      sorted.lift(node.getInt32("at").getValue).map(key => key -> DecisionTrace.Node(Some(node.getString("accepted").getValue), Nil, Nil,
        candidate = Option(node.get("candidate")).filter(_.isInt32).map(_.asInt32.getValue)))
    }.toMap
    DecisionTrace(Option(d.get("pooled")).filter(_.isString).map(_.asString.getValue), None, nodes,
      namesake = Option(d.get("namesake")).exists(v => v.isBoolean && v.asBoolean.getValue))
  }

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
    var awaits                                    = Set.empty[Question]
    var agreed                                    = Map.empty[String, String]
    var trace: Option[BsonDocument]               = None
    BsonFields.document(reader) {
      case "members"        => members = {
                                 val all = Vector.newBuilder[services.movies.ListingKey]
                                 if (BsonFields.bucketed(reader)(all += ListingKeyBson.read(reader))) all.result().sorted(using services.movies.ListingKey.ordering)
                                 else all.result()
                               }
      case "film"           => film = BsonFields.when(reader, BsonType.INT32)(reader.readInt32())
      case "confidence"     => confidence = BsonFields.number(reader, "confidence")
      case "basis"          => basis = reader.readString()
      case "explanation"    => explanation = BsonFields.strings(reader)
      case "contradictions" => contradictions = BsonFields.strings(reader)
      case "fallback"       => fallback = BsonFields.when(reader, BsonType.DOCUMENT)(readFallback(reader))
      case "leaning"        => leaning = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))
      case "candidate"      => candidate = BsonFields.when(reader, BsonType.DOCUMENT)(readLeaning(reader))
      case "unanswered"     => unanswered = BsonFields.when(reader, BsonType.INT32)(reader.readInt32()).getOrElse(0)
      case "awaits"         => awaits = BsonFields.when(reader, BsonType.ARRAY) {
                                 val all = Set.newBuilder[Question]
                                 reader.readStartArray()
                                 while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) awaitedOf(reader.readString()).foreach(all += _)
                                 reader.readEndArray()
                                 all.result()
                               }.getOrElse(Set.empty)
      case "trace"          => trace = BsonFields.when(reader, BsonType.DOCUMENT)(
                                 new org.bson.codecs.BsonDocumentCodec().decode(reader, org.bson.codecs.DecoderContext.builder().build()))
      case "agreed"         => agreed = BsonFields.when(reader, BsonType.DOCUMENT) {
                                 val all = Map.newBuilder[String, String]
                                 BsonFields.document(reader)(family => all += family -> reader.readString())
                                 all.result()
                               }.getOrElse(Map.empty)
      case _                => reader.skipValue()
    }
    ResolverDecision(BsonFields.required(members, "members"), film, BsonFields.required(confidence, "confidence").doubleValue,
      ResolverDecision.Basis.valueOf(BsonFields.required(basis, "basis")), BsonFields.required(explanation, "explanation"),
      BsonFields.required(contradictions, "contradictions"), fallback, leaning, unanswered, agreed, candidate, awaits)(
      trace.fold(DecisionTrace.Empty)(keptTrace(_, BsonFields.required(members, "members"))))
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
