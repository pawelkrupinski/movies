package services.identity

import java.util.Locale

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, IndexModel, Indexes, Projections, ReplaceOneModel, ReplaceOptions}
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

/** One family's traces as a resolve hands them over: its id, and how to BUILD them — lazily, by the store, off the
 *  resolver's thread for one that writes them ([[MongoIdentityTraceStore]]), so a store writing in batches holds one
 *  batch, not a restore's whole corpus. */
final case class FamilyTraces(family: String, build: () => IterableOnce[ListingTrace])

object FamilyTraces {
  /** Traces already built, filed by their family. */
  def of(traces: Seq[ListingTrace]): Seq[FamilyTraces] =
    traces.groupBy(_.family).toSeq.sortBy(_._1).map { case (family, own) => FamilyTraces(family, () => own) }
}

/** Where the traces go: replaced a family at a time, as the families themselves are. */
trait IdentityTraceStore {
  /** Drop the traces of the families `removed` names, then keep what `added` builds. A family handed over again is
   *  removed in the same call or an earlier one (the model re-resolved it), so its earlier traces never stand beside. */
  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit
  /** Stop writing: what is still queued is dropped, and a later [[replace]] keeps nothing. */
  def close(): Unit = ()
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
  /** Up to `limit` `wanted` listings left with no film that no proposal judged no film — what a model is asked
   *  about. Read past every unwanted one, so listings already answered can never fill the limit ahead of new ones. */
  def unresolved(limit: Int, wanted: ListingTrace => Boolean): Seq[ListingTrace]
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
  /** The blocker prefix of a listing a model judged no film (`ResolverDecisions`). */
  val NotAFilm = "not-a-film:"
  /** Nothing traced: a deployment with no Mongo. */
  val Empty: IdentityTraceReads = new IdentityTraceReads {
    def byRule(rule: String, limit: Int)  = Nil
    def byFilm(film: Int, limit: Int)     = Nil
    def byTitle(text: String, limit: Int) = Nil
    def ruleCounts()                      = Nil
    def blockers()                        = Nil
    def unresolved(limit: Int, wanted: ListingTrace => Boolean) = Nil
    def byBlocker(blocker: String, limit: Int) = Nil
  }
}

/** Traces held in memory, written and read as `identity_traces` is — for tests and Mongo-less runs. */
final class InMemoryIdentityTraceStore extends IdentityTraceStore with IdentityTraceReads {
  private val held = scala.collection.mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = synchronized {
    held.filterInPlace((_, trace) => !removed(trace.family)); added.iterator.flatMap(_.build()).foreach(trace => held(trace.listing) = trace)
  }
  private def all = synchronized(held.values.toSeq)
  def byRule(rule: String, limit: Int)  = all.filter(_.rules.contains(rule)).take(limit)
  def byFilm(film: Int, limit: Int)     = all.filter(_.film.contains(film)).take(limit)
  def byTitle(text: String, limit: Int) = all.filter(_.listing.rawTitle.toLowerCase(Locale.ROOT).contains(text.toLowerCase(Locale.ROOT))).take(limit)
  def blockers()                        = BlockerCount.of(all)
  def unresolved(limit: Int, wanted: ListingTrace => Boolean) =
    all.filter(trace => trace.blocker.exists(!_.startsWith(IdentityTraceReads.NotAFilm)) && wanted(trace)).take(limit)
  def byBlocker(blocker: String, limit: Int) = all.filter(_.blocker.contains(blocker)).take(limit)
  def ruleCounts()                      = all.flatMap(_.rules).groupBy(identity).map { case (rule, hits) => rule -> hits.size }.toSeq.sortBy(c => (-c._2, c._1))
}

object IdentityTraceStore {
  /** Keeps nothing, and builds nothing: a model whose decisions no one reads the rules of. */
  val Discard: IdentityTraceStore = (_: Set[String], _: Seq[FamilyTraces]) => ()
}

/** The traces in `identity_traces`: one document per listing, `_id` its serialised key, indexed on its rule ids
 *  (a rule's listings), its film (a film's listings' rules) and its family (what a family's replace drops). */
final class MongoIdentityTraceStore(db: MongoDatabase) extends IdentityTraceStore {
  import MongoIdentityTraceStore._
  private val Timeout = 60.seconds
  private val logger  = play.api.Logger(getClass)
  // One writer thread, each family's drop and write landing in the order the model replaced it; a resolve only
  // hands its families over. A trace is diagnostics: a failed write is logged and never fails a projection.
  private val writes = new CoalescedTraceWrites(write, (removed, e) =>
    logger.warn(s"identity traces: ${removed.size} family drop(s) and their re-resolved traces not written: $e"))

  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = writes.replace(removed, added)

  /** Its thread stopped, before the Mongo connection it writes through closes: what waits is dropped unbuilt. */
  override def close(): Unit = writes.close()

  /** Waits for every handed-over write: for a test, or a shutdown that wants them in. */
  def flush(): Unit = writes.flush()

  private val documentsWritten = new java.util.concurrent.atomic.AtomicLong
  /** How many trace documents this store has replaced or deleted — those whose content moved. */
  def written: Long = documentsWritten.get
  private lazy val collection: MongoCollection[Document] = {
    val c = db.getCollection[Document](Collection)
    Await.result(c.createIndexes(Seq(IndexModel(Indexes.ascending("rules")), IndexModel(Indexes.ascending("film")),
      IndexModel(Indexes.ascending("family")),
      IndexModel(Indexes.ascending("blocker"), org.mongodb.scala.model.IndexOptions().sparse(true)))).toFuture(), Timeout)
    c
  }

  // A batch at a time: a restore hands over every listing's trace (~100k on worker-us), and one bulk write of them all
  // held every document at once and ran into its own timeout.
  //
  // Only what moved is written: a trace whose content digest matches the stored one is left alone, and a removed
  // family's documents are deleted only where no new trace names them. A rules change re-resolves every family on
  // the next boot, and rewrote all ~165k traces as a delete and an upsert each, 500-1,100 Mongo writes a second for
  // minutes, for decisions that had not changed (`IncrementalResolver.restore` drops only what it did not decide again).
  private def write(removed: Set[String], added: IterableOnce[ListingTrace]): Unit = {
    val before = if (removed.isEmpty) Set.empty[String] else
      Await.result(collection.find(Filters.in("family", removed.toSeq*)).projection(Projections.include("_id"))
        .batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
        .flatMap(_.get("_id").collect { case id if id.isString => id.asString.getValue }).toSet
    val kept = scala.collection.mutable.HashSet.empty[String]
    added.iterator.grouped(WriteBatch).foreach { batch =>
      val docs   = batch.map(trace => ListingKey.serialised(trace.listing) -> digested(encode(trace)))
      kept ++= docs.map(_._1)
      val stored = Await.result(collection.find(Filters.in("_id", docs.map(_._1)*)).projection(Projections.include(DigestField))
        .batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
        .flatMap(d => d.get("_id").map(_.asString.getValue -> d.get(DigestField).filter(_.isInt64).map(_.asInt64.getValue))).toMap
      val moved  = docs.filterNot { case (id, doc) => stored.get(id).flatten.contains(doc.getInt64(DigestField).getValue) }
      if (moved.nonEmpty) {
        Await.result(collection.bulkWrite(moved.map { case (id, doc) =>
          ReplaceOneModel(Filters.equal("_id", id), Document(doc), ReplaceOptions().upsert(true)) }).toFuture(), Timeout)
        documentsWritten.addAndGet(moved.size.toLong)
      }
    }
    (before -- kept).grouped(WriteBatch).foreach { gone =>
      Await.result(collection.deleteMany(Filters.in("_id", gone.toSeq*)).toFuture(), Timeout)
      documentsWritten.addAndGet(gone.size.toLong)
    }
  }
}

/**
 * The hand-overs a trace store writes on ONE thread, coalesced while that thread is busy: a family removed after it
 * was handed over is never built (its traces would be dropped by the removal anyway), and a family handed over
 * again replaces its earlier hand-over. What waits is at most one hand-over per family and the ids removed — never
 * one queued task per model update: each held its families' decisions with their full traces, and a slow store
 * under a take-up's or a fill's event stream queued them without bound.
 *
 * Writing the coalesced removals first, then every pending family's traces, ends in what writing each hand-over in
 * turn would: a family's traces handed over after a removal are written after it, and one removed after its
 * hand-over is dropped from what waits.
 */
private[identity] final class CoalescedTraceWrites(write: (Set[String], Iterator[ListingTrace]) => Unit,
                                                   failed: (Set[String], Throwable) => Unit) {
  private val removed = scala.collection.mutable.HashSet.empty[String]
  private val pending = scala.collection.mutable.LinkedHashMap.empty[String, () => IterableOnce[ListingTrace]]
  private var writing = false

  def replace(gone: Set[String], added: Seq[FamilyTraces]): Unit = synchronized {
    // Closed: a drain racing the stop keeps nothing, and builds nothing.
    if (!closed) {
      gone.foreach { family => removed += family; pending.remove(family) }
      added.foreach(family => pending(family.family) = family.build)
      notifyAll()
    }
  }

  /** How many families' traces wait to be built and written. */
  def waiting: Int = synchronized(pending.size)

  /** Waits until everything handed over is written (or dropped by [[close]]). */
  def flush(): Unit = synchronized { while (writing || removed.nonEmpty || pending.nonEmpty) wait() }

  /** Drops what waits, keeps nothing handed over later, and stops the writer thread — interrupting a write under way. */
  def close(): Unit = {
    synchronized { closed = true; removed.clear(); pending.clear(); notifyAll() }
    writer.interrupt()
  }
  private var closed = false

  private val writer = Thread.ofPlatform().daemon().name("identity-traces").start(() =>
    try while (true) {
      val (gone, builds) = synchronized {
        while (!closed && removed.isEmpty && pending.isEmpty) wait()
        if (closed) throw new InterruptedException("closed")
        val taken = (removed.toSet, pending.values.toList)
        removed.clear(); pending.clear(); writing = true
        taken
      }
      try write(gone, builds.iterator.flatMap(_())) catch { case scala.util.control.NonFatal(e) => failed(gone, e) }
      finally synchronized { writing = false; notifyAll() }
    } catch { case _: InterruptedException => () })
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
  /** Through the sparse `blocker` index (only unresolved listings carry one), a page at a time in `_id` order until
   *  `limit` wanted listings are found or the unresolved run out. */
  def unresolved(limit: Int, wanted: ListingTrace => Boolean) = {
    val unresolvedFilter = Filters.and(Filters.exists("blocker"), Filters.not(Filters.regex("blocker", s"^${IdentityTraceReads.NotAFilm}")))
    val kept = Seq.newBuilder[ListingTrace]
    var keptCount = 0
    var after: Option[String] = None
    var more = true
    while (more && keptCount < limit) {
      val filter = after.fold(unresolvedFilter)(id => Filters.and(unresolvedFilter, Filters.gt("_id", id)))
      val page = Await.result(collection.find(filter).sort(org.mongodb.scala.model.Sorts.ascending("_id"))
        .limit(UnresolvedPage).batchSize(tools.MongoReplies.Default).toFuture(), Timeout).map(_.toBsonDocument)
      page.iterator.map(decode).filter(wanted).take(limit - keptCount).foreach { trace => kept += trace; keptCount += 1 }
      after = page.lastOption.map(_.getString("_id").getValue)
      more = page.size == UnresolvedPage
    }
    kept.result()
  }
  private val UnresolvedPage = 1000
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
  /** A trace document's content digest, which a rewrite of an unchanged trace is skipped by. */
  val DigestField = "digest"

  /** `doc` with its content digest under `field`: 64 bits from its canonical JSON, two MurmurHash3 seeds. */
  private[identity] def digested(doc: BsonDocument, field: String = DigestField): BsonDocument = {
    val json = doc.toJson
    val high = scala.util.hashing.MurmurHash3.stringHash(json, 0x2f1d7a3b).toLong
    val low  = scala.util.hashing.MurmurHash3.stringHash(json, 0x6c8e9cf5).toLong & 0xffffffffL
    doc.clone().append(field, BsonInt64((high << 32) | low))
  }
  /** How many traces one bulk write carries. */
  val WriteBatch = 1000

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
