package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, IndexModel, Indexes, Projections, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
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

/** One family's traces as a store receives them (from a settled family, [[IdentityTraceStore.settle]]): its id, and
 *  how to BUILD them — lazily, by the store, off the resolver's thread for one that writes them
 *  ([[MongoIdentityTraceStore]]), so a store writing in batches holds one batch, not a restore's whole corpus. */
final case class FamilyTraces(family: String, build: () => IterableOnce[ListingTrace])

object FamilyTraces {
  /** Traces already built, filed by their family. */
  def of(traces: Seq[ListingTrace]): Seq[FamilyTraces] =
    traces.groupBy(_.family).toSeq.sortBy(_._1).map { case (family, own) => FamilyTraces(family, () => own) }
}

/** Where the traces go: replaced a family at a time, as the families themselves are. */
trait IdentityTraceStore extends IdentityTraceSink {
  /** The settled families' traces, each built by [[ListingTrace.of]] when the store asks for it. */
  final def settle(removed: Set[String], settled: Seq[SettledFamily]): Unit =
    replace(removed, settled.map(s => FamilyTraces(s.id, () => ListingTrace.of(s.id, s.family, s.titleRules, s.calibration))))

  /** Drop the traces of the families `removed` names, then keep what `added` builds. A family handed over again is
   *  removed in the same call or an earlier one (the model re-resolved it), so its earlier traces never stand beside. */
  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit
  /** Stop writing: what is still queued is dropped, and a later [[replace]] keeps nothing. */
  def close(): Unit = ()
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
  private val documentsWritten = new java.util.concurrent.atomic.AtomicLong
  // One writer thread, each family's drop and write landing in the order the model replaced it; a resolve only
  // hands its families over. A trace is diagnostics: a failed write is logged and never fails a projection.
  private val writes = new CoalescedTraceWrites(write, (removed, e) =>
    logger.warn(s"identity traces: ${removed.size} family drop(s) and their re-resolved traces not written: $e"))

  def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = writes.replace(removed, added)

  /** Its thread stopped, before the Mongo connection it writes through closes: what waits is dropped unbuilt. */
  override def close(): Unit = writes.close()

  /** Waits for every handed-over write: for a test, or a shutdown that wants them in. */
  def flush(): Unit = writes.flush()

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
      val docs = batch.map(trace => ListingKey.serialised(trace.listing) -> DocumentDigest.of(encode(trace), DigestField))
      kept ++= docs.map(_._1)
      val written = DocumentDigest.writeMoved(collection, docs.toSeq, DigestField, Timeout)((id, doc) =>
        ReplaceOneModel(Filters.equal("_id", id), Document(doc), ReplaceOptions().upsert(true)))
      documentsWritten.addAndGet(written.toLong)
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

object MongoIdentityTraceStore {
  val Collection = "identity_traces"
  /** A trace document's content digest, which a rewrite of an unchanged trace is skipped by. */
  val DigestField = "digest"

  /** How many traces one bulk write carries. */
  val WriteBatch = 1000

  def decode(d: BsonDocument): ListingTrace = {
    def strings(name: String) = BsonFields.stringsOf(d, name)
    def int(name: String)     = Option(d.get(name)).filter(_.isInt32).map(_.asInt32.getValue)
    ListingTrace(ListingKeyBson.decode(d.getDocument("listing")), d.getString("family").getValue, int("film"),
      d.getString("basis").getValue, strings("rules"), Option(d.get("vetoedBy")).filter(_.isString).map(_.asString.getValue),
      strings("evidence"), int("weighedFilm"),
      BsonFields.elementsOf(d, "refusals").map { value =>
        val r = value.asDocument
        DecisionTrace.Refusal(r.getString("rule").getValue, r.getString("why").getValue,
          Option(r.get("film")).filter(_.isInt32).map(_.asInt32.getValue), Option(r.get("detail")).filter(_.isString).fold("")(_.asString.getValue))
      }, strings("searched"), strings("candidates"), Option(d.get("blocker")).filter(_.isString).map(_.asString.getValue))
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
