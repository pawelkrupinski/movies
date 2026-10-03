package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.libs.json.{JsArray, JsValue, Json}
import services.movies.ListingKey

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

/** One stored [[Proposal]]: what a language model said the listings of one title are, and when, by which model. */
final case class StoredProposal(key: String, title: String, proposal: Proposal, model: String, at: Instant)

/** Where proposals are kept: `identity_proposals`, one document per title key. */
trait ProposalStore {
  def all(): Seq[StoredProposal]
  def put(stored: StoredProposal): Unit
}

final class InMemoryProposalStore extends ProposalStore {
  private val held = new ConcurrentHashMap[String, StoredProposal]()
  def all(): Seq[StoredProposal] = held.values.asScala.toSeq
  def put(stored: StoredProposal): Unit = { held.put(stored.key, stored); () }
}

final class MongoProposalStore(db: MongoDatabase) extends ProposalStore {
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](MongoProposalStore.Collection)
  def all(): Seq[StoredProposal] =
    Await.result(collection.find().batchSize(tools.MongoReplies.Default).toFuture(), Timeout).map(d => MongoProposalStore.decode(d.toBsonDocument))
  def put(stored: StoredProposal): Unit =
    Await.result(collection.replaceOne(Filters.equal("_id", stored.key), Document(MongoProposalStore.encode(stored)), ReplaceOptions().upsert(true)).toFuture(), Timeout): Unit
}

object MongoProposalStore {
  val Collection = "identity_proposals"
  private[identity] def encode(s: StoredProposal): BsonDocument = new BsonDocument()
    .append("_id", BsonString(s.key)).append("title", BsonString(s.title)).append("category", BsonString(s.proposal.category))
    .append("originalTitle", s.proposal.originalTitle.fold[BsonValue](BsonNull())(BsonString(_)))
    .append("year", s.proposal.year.fold[BsonValue](BsonNull())(BsonInt32(_)))
    .append("directors", BsonArray.fromIterable(s.proposal.directors.map(BsonString(_))))
    .append("model", BsonString(s.model)).append("at", BsonString(s.at.toString))
  private[identity] def decode(d: BsonDocument): StoredProposal = {
    def text(name: String) = Option(d.get(name)).filter(_.isString).map(_.asString.getValue)
    StoredProposal(d.getString("_id").getValue, text("title").getOrElse(""),
      Proposal(text("category").getOrElse("unclear"), text("originalTitle"), Option(d.get("year")).filter(_.isInt32).map(_.asInt32.getValue),
        Option(d.get("directors")).filter(_.isArray).fold(Seq.empty[String])(_.asArray.getValues.asScala.toSeq.map(_.asString.getValue))),
      text("model").getOrElse(""), text("at").map(Instant.parse).getOrElse(Instant.EPOCH))
  }
}

/** The proposals as the identity model reads them: one per title key ([[ProposalIndex.keyOf]]), each read filed with
 *  the model's `reads` under `proposal:<key>`, so a new or changed proposal re-resolves exactly the listings that read
 *  it (`changed`, the model's `observed`). Loaded once, kept current by [[put]]. */
final class ProposalIndex(store: ProposalStore, changed: String => Unit = _ => ()) {
  @volatile private var loaded: Option[Map[String, Proposal]] = None
  private def held: Map[String, Proposal] = loaded.getOrElse(synchronized {
    loaded.getOrElse { val all = store.all().map(s => s.key -> s.proposal).toMap; loaded = Some(all); all }
  })

  def proposal(listing: Listing, reads: ObservationReads): Option[Proposal] = {
    val key = ProposalIndex.keyOf(listing.rawTitle)
    reads.read(ProposalIndex.readKey(key))
    held.get(key)
  }
  def has(key: String): Boolean = held.contains(key)
  def put(stored: StoredProposal): Unit = {
    store.put(stored)
    synchronized { loaded = Some(held + (stored.key -> stored.proposal)) }
    changed(ProposalIndex.readKey(stored.key))
  }
}

object ProposalIndex {
  /** One proposal per title, whatever venue lists it: its [[IdentityMeasures.key]]. */
  def keyOf(rawTitle: String): String = IdentityMeasures.key(rawTitle)
  def readKey(key: String): String = s"proposal:$key"
}

/** One listing a model is asked about: its title, a venue that lists it, and the year and directors it publishes. */
final case class ProposalAsk(key: String, title: String, venue: String, year: Option[Int], directors: Seq[String])

/** Asks a language model what listings are ([[Proposal]]). */
trait Proposer {
  def model: String
  def propose(asks: Seq[ProposalAsk]): Map[String, Proposal]
}

/** Each round, asks the [[Proposer]] about the title keys of listings left with no film ([[IdentityTraceReads.unresolved]])
 *  that hold no proposal yet — at most `budget` titles, `batch` per request — and files each answer in the index. */
final class ProposalFill(traces: IdentityTraceReads, index: ProposalIndex, proposer: Proposer, clock: Clock,
                         budget: Int = ProposalFill.Budget, batch: Int = ProposalFill.Batch) {
  private val logger = play.api.Logger(getClass)

  def round(): Int = {
    val asks = traces.unresolved(ProposalFill.Scan).map { trace =>
        val (year, directors) = trace.listing match {
          case ListingKey.Published(_, _, year, directors) => (year, directors)
          case _: ListingKey.Native                        => (None, Nil)
        }
        ProposalAsk(ProposalIndex.keyOf(trace.listing.rawTitle), trace.listing.rawTitle, trace.listing.venue, year, directors)
      }
      .filter(ask => ask.key.nonEmpty && !index.has(ask.key)).distinctBy(_.key).take(budget)
    val filed = asks.grouped(batch).map { group =>
      val answers = scala.util.Try(proposer.propose(group)).fold(e => { logger.warn(s"identity proposals: ${group.size} asked, failed: $e"); Map.empty[String, Proposal] }, identity)
      group.flatMap(ask => answers.get(ask.key).map(p => StoredProposal(ask.key, ask.title, p, proposer.model, clock.instant()))).foreach(index.put)
      answers.size
    }.sum
    if (asks.nonEmpty) logger.info(s"identity proposals: ${asks.size} title(s) asked, $filed answered")
    filed
  }
}

object ProposalFill {
  /** Titles asked per round, and per request: a day's new unresolved titles are tens, the first round hundreds. */
  val Budget = 200
  val Batch  = 20
  /** Unresolved traces read per round. */
  val Scan   = 5000
}

/** [[Proposer]] over Anthropic's Messages API: one request per batch, the listings numbered, the answer a JSON array.
 *  Deterministic (temperature 0, the request built from the asks alone), so a recording replays it. A film the model is
 *  not sure of (confidence below 0.7) comes back as "unclear": it adds no search and no rule takes it. */
final class AnthropicProposer(apiKey: settings.AnthropicApiKey, val model: String = AnthropicProposer.Model,
                              timeout: java.time.Duration = java.time.Duration.ofSeconds(90)) extends Proposer {
  private val http = java.net.http.HttpClient.newBuilder().connectTimeout(java.time.Duration.ofSeconds(15)).build()

  def propose(asks: Seq[ProposalAsk]): Map[String, Proposal] = if (asks.isEmpty) Map.empty else {
    val body = Json.obj("model" -> model, "max_tokens" -> 4096, "temperature" -> 0, "system" -> AnthropicProposer.System,
      "messages" -> Json.arr(Json.obj("role" -> "user", "content" -> AnthropicProposer.prompt(asks)))).toString
    val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create("https://api.anthropic.com/v1/messages")).timeout(timeout)
      .header("x-api-key", apiKey.value).header("anthropic-version", "2023-06-01").header("content-type", "application/json")
      .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body)).build()
    val response = http.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
    if (response.statusCode / 100 != 2) throw new RuntimeException(s"Anthropic HTTP ${response.statusCode}")
    AnthropicProposer.parse(response.body, asks)
  }
}

object AnthropicProposer {
  val Model = "claude-haiku-4-5-20251001"
  val Confident = 0.7

  val System: String =
    """You identify cinema listings. For each numbered listing (a venue's title, maybe with the year and directors it
      |publishes) say what it is. category: "film" (one feature, short or documentary with its own film record),
      |"compilation" (a package of several shorts), "event" (not a film: concert, live theatre, stand-up, workshop,
      |meeting, quiz, a marathon of several films), "stage" (an opera, ballet or theatre broadcast), or "unclear".
      |For a film give its ORIGINAL title as the film databases list it (in its original language), its release year
      |and directors, and your confidence 0-1. Strip programme banners and screening notes. Never guess: when unsure,
      |say "unclear". Answer with only a JSON array of {"i", "category", "original_title", "year", "directors",
      |"confidence"}, one per listing.""".stripMargin

  def prompt(asks: Seq[ProposalAsk]): String = asks.zipWithIndex.map { case (ask, i) =>
    s"${i + 1}. ${ask.title} — at ${ask.venue}" + ask.year.fold("")(y => s", year $y") +
      (if (ask.directors.nonEmpty) s", directed by ${ask.directors.mkString(", ")}" else "")
  }.mkString("\n")

  /** The proposals a response's JSON array makes, by each ask's key. */
  def parse(response: String, asks: Seq[ProposalAsk]): Map[String, Proposal] = {
    val text  = (Json.parse(response) \ "content").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(c => (c \ "text").asOpt[String]).mkString
    val array = text.indexOf('[') match { case -1 => "[]"; case start => text.substring(start, text.lastIndexOf(']') + 1) }
    Json.parse(array).asOpt[JsArray].fold(Seq.empty[JsValue])(_.value.toSeq).flatMap { item =>
      for {
        i   <- (item \ "i").asOpt[Int] if i >= 1 && i <= asks.size
        cat <- (item \ "category").asOpt[String]
      } yield {
        val confident = (item \ "confidence").asOpt[Double].exists(_ >= Confident)
        val film      = cat == Proposal.Film && confident
        asks(i - 1).key -> Proposal(if (cat == Proposal.Film && !confident) "unclear" else cat,
          (item \ "original_title").asOpt[String].filter(_ => film).map(_.trim).filter(_.nonEmpty),
          (item \ "year").asOpt[Int].filter(_ => film),
          (item \ "directors").asOpt[Seq[String]].filter(_ => film).getOrElse(Nil))
      }
    }.toMap
  }
}
