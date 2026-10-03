package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, IndexModel, Indexes, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps

/** Which rules decided one listing ([[DecisionTrace]], plus the title rules its title took), filed under its
 *  family: the record `identity_traces` keeps per listing, so a listing's rules and a rule's listings are each
 *  one indexed read. Never read by the resolver, its take-up or the web: written beside the families only.
 *  @param rules   rule ids by kind — `accept:`, `pooled:`, `veto:`, `join:`, `apart:`, `title:`, `format:`
 *  @param vetoedBy the member listing whose own evidence denied the cluster's best film, when one did
 *  @param refusals for a listing no rule took alone, each rule's refusal: its condition, the candidate it weighed
 *                  and what that candidate's evidence said — the detail behind each `refused:` rule id
 *  @param searched for such a listing, each query it asked and how many films it found (or that it went unanswered)
 *  @param candidates its best candidates as scored (five when no rule took it; the runner-up it beat when one did)
 *  @param blocker  for a listing left with no film, what stopped it ([[DecisionTrace.blockerOf]]) — what the
 *                  unresolved listings are ranked by, the next win first */
final case class ListingTrace(listing: ListingKey, family: String, film: Option[Int], basis: String, rules: Seq[String],
                              vetoedBy: Option[String], evidence: Seq[String] = Nil, weighedFilm: Option[Int] = None,
                              refusals: Seq[DecisionTrace.Refusal] = Nil, searched: Seq[String] = Nil,
                              candidates: Seq[String] = Nil, blocker: Option[String] = None)

object ListingTrace {
  /** The traces of `family`'s listings: each decision's rules for its members ([[DecisionTrace.rulesOf]]), and
   *  the title rules each member's title took (`titleRules`, by venue and raw title). */
  def of(familyId: String, family: IdentityResolver.RegionFamily, titleRules: ListingKey => Seq[String],
         calibration: Option[IdentityCalibration] = None): Seq[ListingTrace] =
    family.decisions.flatMap { decision =>
      decision.members.map { key =>
        val node = decision.trace.nodes.get(key)
        ListingTrace(key, familyId, decision.film, decision.basis.toString, decision.trace.rulesOf(key) ++ titleRules(key),
          decision.trace.vetoed.flatMap(_.by),
          calibration.zip(node).fold(Seq.empty[String]) { case (c, n) => c.evidence(IdentityMeasures.ListingFilm, n.measures) },
          node.flatMap(_.candidate), node.fold(Seq.empty[DecisionTrace.Refusal])(_.refusals),
          node.fold(Seq.empty[String])(_.searched), node.fold(Seq.empty[String])(_.candidates),
          if (decision.film.isDefined) None else node.flatMap(_.blocker).orElse(Some("pooled:no-film")))
      }
    }
}

/** Where the traces go: replaced a family at a time, as the families themselves are. `added` is BUILT by the
 *  store, when it keeps them — off the resolver's thread for one that writes them ([[MongoIdentityTraceStore]]). */
trait IdentityTraceStore {
  /** Drop the traces of the families `removed` names, then keep what `added` builds. */
  def replace(removed: Set[String], added: () => Seq[ListingTrace]): Unit
}

/** The trace READ both ways, for `/admin/identity/traces`: a rule's listings, a film's, a title's, and every rule's count. */
trait IdentityTraceReads {
  def byRule(rule: String, limit: Int): Seq[ListingTrace]
  def byFilm(film: Int, limit: Int): Seq[ListingTrace]
  /** The listings whose raw title contains `text`, ignoring case. */
  def byTitle(text: String, limit: Int): Seq[ListingTrace]
  /** Every rule id, with how many listings it decided — most first. */
  def ruleCounts(): Seq[(String, Int)]
  /** The unresolved listings one blocker stopped. */
  def byBlocker(blocker: String, limit: Int): Seq[ListingTrace]
  /** What keeps listings unresolved: each blocker with its listings, its distinct titles and a few of them — most
   *  listings first, so the top row is the next win to investigate. */
  def blockers(): Seq[BlockerCount]
}

/** One blocker's share of the unresolved listings ([[IdentityTraceReads.blockers]]). */
final case class BlockerCount(blocker: String, listings: Int, titles: Int, examples: Seq[String])
object BlockerCount {
  /** The blockers of `traces`, ranked as [[IdentityTraceReads.blockers]] ranks them. */
  def of(traces: Seq[ListingTrace]): Seq[BlockerCount] =
    traces.flatMap(trace => trace.blocker.map(_ -> trace.listing.rawTitle)).groupBy(_._1).map { case (blocker, rows) =>
      val titles = rows.map(_._2).distinct.sorted
      BlockerCount(blocker, rows.size, titles.size, titles.take(Examples))
    }.toSeq.sortBy(count => (-count.listings, count.blocker))
  val Examples = 5
}

object IdentityTraceReads {
  /** Nothing traced: a deployment with no Mongo. */
  val Empty: IdentityTraceReads = new IdentityTraceReads {
    def byRule(rule: String, limit: Int)  = Nil
    def byFilm(film: Int, limit: Int)     = Nil
    def byTitle(text: String, limit: Int) = Nil
    def ruleCounts()                      = Nil
    def blockers()                        = Nil
    def byBlocker(blocker: String, limit: Int) = Nil
  }
}

/** Traces held in memory, written and read as `identity_traces` is — for tests and Mongo-less runs. */
final class InMemoryIdentityTraceStore extends IdentityTraceStore with IdentityTraceReads {
  private val held = scala.collection.mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
  def replace(removed: Set[String], added: () => Seq[ListingTrace]): Unit = synchronized {
    held.filterInPlace((_, trace) => !removed(trace.family)); added().foreach(trace => held(trace.listing) = trace)
  }
  private def all = synchronized(held.values.toSeq)
  def byRule(rule: String, limit: Int)  = all.filter(_.rules.contains(rule)).take(limit)
  def byFilm(film: Int, limit: Int)     = all.filter(_.film.contains(film)).take(limit)
  def byTitle(text: String, limit: Int) = all.filter(_.listing.rawTitle.toLowerCase.contains(text.toLowerCase)).take(limit)
  def blockers()                        = BlockerCount.of(all)
  def byBlocker(blocker: String, limit: Int) = all.filter(_.blocker.contains(blocker)).take(limit)
  def ruleCounts()                      = all.flatMap(_.rules).groupBy(identity).map { case (rule, hits) => rule -> hits.size }.toSeq.sortBy(c => (-c._2, c._1))
}

object IdentityTraceStore {
  /** Keeps nothing, and builds nothing: a model whose decisions no one reads the rules of. */
  val Discard: IdentityTraceStore = (_: Set[String], _: () => Seq[ListingTrace]) => ()
}

/** The traces in `identity_traces`: one document per listing, `_id` its serialised key, indexed on its rule ids
 *  (a rule's listings), its film (a film's listings' rules) and its family (what a family's replace drops). */
final class MongoIdentityTraceStore(db: MongoDatabase) extends IdentityTraceStore {
  import MongoIdentityTraceStore._
  private val Timeout = 60.seconds
  private val logger  = play.api.Logger(getClass)
  // One thread, so each family's drop and write land in the order the model replaced it; a resolve only
  // hands its families over. A trace is diagnostics: a failed write is logged and never fails a projection.
  private val writer = java.util.concurrent.Executors.newSingleThreadExecutor(Thread.ofPlatform().daemon().name("identity-traces").factory())

  def replace(removed: Set[String], added: () => Seq[ListingTrace]): Unit = {
    writer.execute(() =>
      try write(removed, added())
      catch { case scala.util.control.NonFatal(e) => logger.warn(s"identity traces: ${removed.size} family drop(s) and their re-resolved traces not written: $e") })
  }

  /** Waits for every handed-over write: for a test, or a shutdown that wants them in. */
  def flush(): Unit = { writer.submit(new Runnable { def run(): Unit = () }).get(); () }
  private lazy val collection: MongoCollection[Document] = {
    val c = db.getCollection[Document](Collection)
    Await.result(c.createIndexes(Seq(IndexModel(Indexes.ascending("rules")), IndexModel(Indexes.ascending("film")),
      IndexModel(Indexes.ascending("family")),
      IndexModel(Indexes.ascending("blocker"), org.mongodb.scala.model.IndexOptions().sparse(true)))).toFuture(), Timeout)
    c
  }

  private def write(removed: Set[String], added: Seq[ListingTrace]): Unit = {
    if (removed.nonEmpty) Await.result(collection.deleteMany(Filters.in("family", removed.toSeq*)).toFuture(), Timeout)
    if (added.nonEmpty)
      Await.result(collection.bulkWrite(added.map(trace =>
        ReplaceOneModel(Filters.equal("_id", ListingKey.serialised(trace.listing)), Document(encode(trace)), ReplaceOptions().upsert(true)))).toFuture(), Timeout)
    ()
  }
}

/** `identity_traces` read for the admin page — through its indexes for a rule and a film; a title is a scan, asked by hand. */
final class MongoIdentityTraceReads(db: MongoDatabase) extends IdentityTraceReads {
  import MongoIdentityTraceStore._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](Collection)
  private def find(filter: org.bson.conversions.Bson, limit: Int): Seq[ListingTrace] =
    Await.result(collection.find(filter).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), Timeout).map(d => decode(d.toBsonDocument))
  def byRule(rule: String, limit: Int)  = find(Filters.equal("rules", rule), limit)
  def byFilm(film: Int, limit: Int)     = find(Filters.equal("film", film), limit)
  def byBlocker(blocker: String, limit: Int) = find(Filters.equal("blocker", blocker), limit)
  def byTitle(text: String, limit: Int) =
    find(Filters.regex("listing.rawTitle", java.util.regex.Pattern.quote(text), "i"), limit)
  def ruleCounts(): Seq[(String, Int)] =
    Await.result(collection.aggregate(Seq(
      org.mongodb.scala.model.Aggregates.unwind("$rules"),
      org.mongodb.scala.model.Aggregates.group("$rules", org.mongodb.scala.model.Accumulators.sum("n", 1)),
      org.mongodb.scala.model.Aggregates.sort(org.mongodb.scala.model.Sorts.descending("n")))).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
      .map(d => d.toBsonDocument).map(d => d.getString("_id").getValue -> d.getInt32("n").getValue)
  /** Through the `blocker` index: only unresolved listings carry one. */
  def blockers(): Seq[BlockerCount] = {
    import org.mongodb.scala.model.{Accumulators, Aggregates, Filters as F, Sorts}
    Await.result(collection.aggregate(Seq(
      Aggregates.`match`(F.exists("blocker")),
      Aggregates.group("$blocker", Accumulators.sum("n", 1), Accumulators.addToSet("titles", "$listing.rawTitle")),
      Aggregates.sort(Sorts.orderBy(Sorts.descending("n"), Sorts.ascending("_id"))))).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
      .map(_.toBsonDocument).map { d =>
        val titles = d.getArray("titles").getValues.asScala.toSeq.map(_.asString.getValue).sorted
        BlockerCount(d.getString("_id").getValue, d.getInt32("n").getValue, titles.size, titles.take(BlockerCount.Examples))
      }
  }
}

object MongoIdentityTraceStore {
  val Collection = "identity_traces"

  private[identity] def decode(d: BsonDocument): ListingTrace = {
    def strings(name: String) = Option(d.get(name)).filter(_.isArray).fold(Seq.empty[String])(_.asArray.getValues.asScala.toSeq.map(_.asString.getValue))
    def int(name: String)     = Option(d.get(name)).filter(_.isInt32).map(_.asInt32.getValue)
    ListingTrace(ListingKeyBson.decode(d.getDocument("listing")), d.getString("family").getValue, int("film"),
      d.getString("basis").getValue, strings("rules"), Option(d.get("vetoedBy")).filter(_.isString).map(_.asString.getValue),
      strings("evidence"), int("weighedFilm"),
      Option(d.get("refusals")).filter(_.isArray).fold(Seq.empty[DecisionTrace.Refusal])(_.asArray.getValues.asScala.toSeq.map { value =>
        val r = value.asDocument
        DecisionTrace.Refusal(r.getString("rule").getValue, r.getString("why").getValue,
          Option(r.get("film")).filter(_.isInt32).map(_.asInt32.getValue), Option(r.get("detail")).filter(_.isString).fold("")(_.asString.getValue))
      }), strings("searched"), strings("candidates"), Option(d.get("blocker")).filter(_.isString).map(_.asString.getValue))
  }

  private[identity] def encode(trace: ListingTrace): BsonDocument = new BsonDocument()
    .append("_id", BsonString(ListingKey.serialised(trace.listing)))
    .append("listing", ListingKeyBson.encode(trace.listing))
    .append("family", BsonString(trace.family))
    .append("film", trace.film.fold[BsonValue](BsonNull())(BsonInt32(_)))
    .append("basis", BsonString(trace.basis))
    .append("rules", BsonArray.fromIterable(trace.rules.map(BsonString(_))))
    .append("vetoedBy", trace.vetoedBy.fold[BsonValue](BsonNull())(BsonString(_)))
    // why: each measure of the listing against `weighedFilm` (its decision's film, else its best candidate), with its weight
    .append("weighedFilm", trace.weighedFilm.fold[BsonValue](BsonNull())(BsonInt32(_)))
    .append("evidence", BsonArray.fromIterable(trace.evidence.map(BsonString(_))))
    // why not: each rule's refusal of a listing no rule took — its condition, the candidate it weighed, what that said
    .append("refusals", BsonArray.fromIterable(trace.refusals.map(r => new BsonDocument()
      .append("rule", BsonString(r.rule)).append("why", BsonString(r.why))
      .append("film", r.film.fold[BsonValue](BsonNull())(BsonInt32(_))).append("detail", BsonString(r.detail)))))
    // what it searched and weighed, and what stopped it when it was left with no film
    .append("searched", BsonArray.fromIterable(trace.searched.map(BsonString(_))))
    .append("candidates", BsonArray.fromIterable(trace.candidates.map(BsonString(_))))
    // only an unresolved listing carries a blocker, so the sparse `blocker` index holds just those
    .tap(document => trace.blocker.foreach(blocker => document.append("blocker", BsonString(blocker))))
}
