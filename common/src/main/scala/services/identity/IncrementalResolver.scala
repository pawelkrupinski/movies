package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.mutable

/** Which lookups gained an answer since the model last read them — candidate queries, film
 *  records, and the listings whose detail page did. */
final case class AnswersChanged(queries: Set[CandidateQuery], films: Set[Int], details: Set[ListingKey] = Set.empty) {
  def isEmpty: Boolean = queries.isEmpty && films.isEmpty && details.isEmpty
}
object AnswersChanged { val Empty: AnswersChanged = AnswersChanged(Set.empty, Set.empty) }

/**
 * The identity model kept current EVENT BY EVENT — listings a scrape saw, listings it no longer
 * sees, answers the fill filed — instead of re-resolved from scratch on a schedule. Its contract
 * is [[IdentityResolver.resolve]]'s: after every event, its [[decisions]] are what a resolve of the
 * listings it holds would decide over `lookups` as they are then (P1: a resolve is a function of
 * the set, so the order events arrive in is not an input).
 *
 * It keeps the model FAMILY BY FAMILY and re-resolves only the families an event can move (A3: a
 * family decides alone, against the corpus's [[CorpusContext]], as the whole resolve does):
 *
 *  - a family whose listings changed, or whose questions or records gained an answer;
 *  - a family whose slice of the corpus context changed value ([[CorpusContext.Reads]]);
 *  - and every family those, re-resolved, now share a block key with — a match or a title that
 *    merges them — until no re-resolved family shares a key with one left alone.
 */
final class IncrementalResolver(lookups: IdentityLookups, normalizer: TitleNormalizer, calibration: IdentityCalibration,
                                pins: PinConstraints = PinConstraints(Nil),
                                decorations: TitleDecorations = TitleDecorations.resolver,
                                store: IdentityModelStore = new InMemoryIdentityModelStore,
                                rules: String = "",
                                regionBatch: Int = IncrementalResolver.RegionBatch,
                                mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None) {
  import IdentityResolver.RegionFamily
  import IncrementalResolver.Mutation

  // A family keeps the DIGEST of the corpus facts it read, not the facts: comparing digests tells
  // whether it still reads the same, and it holds no copy of another family's records.
  private final case class Family(resolved: RegionFamily, digest: Long) {
    val storeId: String = StoredFamily.idOf(resolved.listings)
    lazy val nodes: Int = resolved.nodeKeys.values.toSet.size
  }

  private val held          = mutable.HashMap.empty[ListingKey, Listing]
  private val atVenue       = mutable.HashMap.empty[String, Set[ListingKey]]
  private val corpus        = new LiveCorpus(lookups, normalizer, pins, decorations)
  private val families      = mutable.HashMap.empty[Int, Family]
  private val readersOf     = mutable.HashMap.empty[String, Set[Int]]
  // Which families asked each question and read each record: what an answer event re-resolves.
  private val askersOf      = mutable.HashMap.empty[CandidateQuery, Set[Int]]
  private val filmReadersOf = mutable.HashMap.empty[Int, Set[Int]]
  private val familyOfKey   = mutable.HashMap.empty[ListingKey, Int]
  private val familiesOfKey = mutable.HashMap.empty[String, Set[Int]]
  private var nextFamily    = 0
  private var reResolved    = 0
  private val clock         = new IncrementalResolver.Timings

  def listingsSeen(listings: Seq[Listing]): Unit       = batch(listings, Nil, AnswersChanged.Empty)
  def listingsGone(keys: Seq[ListingKey]): Unit        = batch(Nil, keys, AnswersChanged.Empty)

  /** The listings the model holds at `venue`: what a venue's next scrape is diffed against. */
  def heldAt(venue: String): Set[ListingKey] = atVenue.getOrElse(venue, Set.empty)
  /** Every listing the model holds. */
  def listings: Seq[Listing] = held.values.toSeq
  def answersChanged(changed: AnswersChanged): Unit    = batch(Nil, Nil, changed)

  /** Several events at once — scrapes and answers that landed together — resolving a family they
   *  all touch once, not once per event. */
  def batch(seen: Seq[Listing], gone: Seq[ListingKey], answered: AnswersChanged): Unit = {
    val left    = gone.filter(held.contains)
    left.foreach(release)
    // A listing whose detail page was answered anew is seen again: its evidence may have moved.
    val redetailed = answered.details.filterNot(left.contains).flatMap(held.get)
    val arrived = seen.filterNot(listing => held.get(listing.key).contains(listing)) ++
      redetailed.filterNot(listing => seen.exists(_.key == listing.key))
    arrived.foreach(hold)
    val moved   = clock.context(corpus.gone(left) ++ corpus.seen(arrived) ++ corpus.answered(answered))
    val asked   = answered.queries.flatMap(askersOf.getOrElse(_, Set.empty)) ++ answered.films.flatMap(filmReadersOf.getOrElse(_, Set.empty))
    update((left ++ arrived.map(_.key)).flatMap(familyOfKey.get).toSet ++ asked, arrived.map(_.key).toSet, moved)
  }

  /** Take up the model `store` kept, over the listings held now: every stored family whose listings
   *  are all still held and still the same nodes, claimed by no other stored family, and whose
   *  corpus facts read the same (its slice's digest) stands as it was; every other listing — one
   *  whose family changed while the model was not running, or that no stored family holds — is
   *  resolved anew, a title-key component at a time. What moved while it was down is exactly what
   *  those checks find, so a restart needs no replay of what it missed. */
  def restore(listings: Seq[Listing]): Unit = {
    listings.foreach(hold)
    clock.context(corpus.seen(listings))
    // Families decided under other rules stand for nothing: every one is re-resolved.
    val stored  = if (store.rulesVersion.contains(rules)) store.families() else { store.replace(store.families().map(_.id).toSet, Nil); Nil }
    val claimed = stored.flatMap(_.family.listings).groupBy(identity).collect { case (key, claims) if claims.sizeIs > 1 => key }.toSet
    val (standing, fallen) = stored.partition { family =>
      family.family.listings.forall(key => held.contains(key) && !claimed(key)) &&
        family.family.nodeKeys.forall { case (key, node) => corpus.nodeKeyOf(key).contains(node) } &&
        (mutation == Mutation.TrustStored || clock.slices(corpus.slice(family.family.reads).digest) == family.digest)
    }
    // A standing family's slice digests to what it stored — the check above just computed it — so it
    // is kept, not digested again (half a UK take-up's hashing when it was).
    standing.foreach(family => remember(family.family, family.digest))
    store.replace(fallen.map(_.id).toSet, Nil)
    update(Set.empty, held.keys.filterNot(familyOfKey.contains).toSet, CorpusContext.Changed.None)
    ruled()
  }

  /** Take `listings` in whole — a new country, or a rebuild after the rules changed — resolved a
   *  bounded batch at a time, so the build holds one batch's answers and records, never the corpus's. */
  def seed(listings: Seq[Listing]): Unit = {
    val arrived = listings.filterNot(listing => held.get(listing.key).contains(listing))
    arrived.foreach(hold)
    val moved = clock.context(corpus.seen(arrived))
    update(arrived.flatMap(listing => familyOfKey.get(listing.key)).toSet, arrived.map(_.key).toSet, moved)
  }

  /** The model as a [[Resolution]] — what the shadow diff and the projection read: its decisions,
   *  its families, and the records of the films it decided. It counts no edges or lookups: those
   *  are a whole resolve's. */
  def resolution: Resolution = {
    val decided = decisions
    Resolution(decided, corpus.nodeCount, familyOf, Nil, Nil, 0, 0, 0, 0, 0,
      decided.flatMap(_.film).distinct.flatMap(id => corpus.candidate(id).map(id -> _.film)).toMap)
  }

  /** The questions and records the model's lookups do not know yet: what a fill should ask. */
  def gaps: AnswersChanged = corpus.gaps

  /** Every decision, in the whole resolve's order. */
  def decisions: Seq[ResolverDecision] = families.values.flatMap(_.resolved.decisions).toSeq.sortBy(_.members.head)(using ListingKey.ordering)
  /** Each listing's family. */
  def familyOf: Map[ListingKey, Int] = familyOfKey.toMap
  /** Each family's candidate questions, with its decisions: what the fill picks aged questions from. */
  def familyQuestions: Seq[(Set[CandidateQuery], Seq[ResolverDecision])] =
    families.values.toSeq.map(f => (f.resolved.queries, f.resolved.decisions))

  /** How many families the model holds, and how many listings. */
  def familyCount: Int = families.size
  def heldCount: Int   = held.size
  /** How large the model's families are: a family grown past a region resolves slowly every time
   *  anything in it moves, so its size is what a slow model shows first. */
  def sizes: IncrementalResolver.FamilySizes = {
    val largest = families.values.toSeq.sortBy(family => (-family.resolved.listings.size, family.storeId)).take(IncrementalResolver.Largest)
    IncrementalResolver.FamilySizes(
      largest.map { family =>
        val perNode = family.resolved.nodeKeys.values.groupMapReduce(identity)(_ => 1)(_ + _)
        IncrementalResolver.FamilySize(family.resolved.listings.size, family.nodes, family.resolved.blockKeys.size,
          perNode.toSeq.sortBy { case (node, count) => (-count, node) }.take(IncrementalResolver.Largest))
      },
      largestNodes = families.values.map(_.nodes).maxOption.getOrElse(0),
      large        = families.values.count(_.resolved.listings.size > IncrementalResolver.RegionBatch))
  }
  /** How many families the model has re-resolved since it was made: the work its events cost. */
  def familiesResolved: Int = reResolved
  /** Where the model's time went since it was made: keeping the corpus context, resolving regions,
   *  and comparing families' context slices. */
  def timings: IncrementalResolver.Timings = clock

  private def update(touched: Set[Int], arrived: Set[ListingKey], moved: CorpusContext.Changed): Unit = {
    val context: CorpusContext = corpus
    // A family reading none of the keys the event moved reads what it read before; one that does
    // is stale only if the values it reads changed.
    val stale = if (mutation == Mutation.IgnoreContext) Set.empty[Int]
                else clock.slices(moved.keys.flatMap(readersOf.getOrElse(_, Set.empty)).filter(id => context.slice(families(id).resolved.reads).digest != families(id).digest))
    val replaced = mutable.HashSet.empty[Int] ++ touched ++ stale
    val settled  = mutable.LinkedHashMap.empty[Set[ListingKey], RegionFamily]
    val loose    = arrived -- (touched ++ stale).flatMap(id => families(id).resolved.listings)
    // What is resolved together: each family whole, and the listings no family holds a title
    // component (or a batch's worth of one) at a time.
    var units: Seq[Set[ListingKey]] = (touched ++ stale).toSeq.sorted.map(id => families(id).resolved.listings) ++
      clock.context(corpus.titleComponents(loose)).sortBy(_.size).flatMap(_.grouped(regionBatch)).map(_.toSet)
    // Resolve the units, packed into batches of at most `regionBatch` listings; a family a result now
    // shares a block key with — one left alone, or one this update already settled — is pulled in
    // and resolved again beside it, in the next round. Every other result is final. A region is so
    // as large as the families it holds, never as every family an event touched.
    while (units.nonEmpty) {
      val next = mutable.ArrayBuffer.empty[Set[ListingKey]]
      IncrementalResolver.pack(units, regionBatch).foreach { batch =>
        val listings = batch.toSeq.flatMap(held.get)
        if (listings.nonEmpty) {
          val result = clock.resolves(listings.size)(IdentityResolver.resolveRegion(listings, lookups, normalizer, calibration, pins, decorations, context))
          reResolved += result.size
          result.foreach { family =>
            val joined = if (mutation == Mutation.NoExpansion) Set.empty[Int]
                         else family.blockKeys.flatMap(key => familiesOfKey.getOrElse(key, Set.empty)) -- replaced
            val back   = if (mutation == Mutation.NoExpansion) Seq.empty[RegionFamily]
                         else settled.values.filter(_.blockKeys.exists(family.blockKeys)).toSeq
            if (joined.isEmpty && back.isEmpty) settled(family.listings) = family
            else {
              back.foreach(again => settled.remove(again.listings))
              replaced ++= joined
              next += family.listings ++ joined.flatMap(id => families(id).resolved.listings) ++ back.flatMap(_.listings)
            }
          }
        }
      }
      units = next.toSeq
    }
    val removed = replaced.flatMap(id => families.get(id).map(_.storeId)).toSet
    replaced.foreach(forget)
    clock.slices(settled.values.foreach(family => remember(family, context.slice(family.reads).digest)))
    val added   = settled.values.toSeq.flatMap(family => familyOfKey.get(family.listings.head).flatMap(families.get))
      .map(family => StoredFamily(family.storeId, family.resolved, family.digest))
    store.replace(removed -- added.map(_.id), added)
    ruled()
  }

  /** The store's families are this model's: record the rules they are decided under, once. */
  private var rulesRecorded = false
  private def ruled(): Unit = if (!rulesRecorded) { store.recordRulesVersion(rules); rulesRecorded = true }

  private def hold(listing: Listing): Unit = {
    held(listing.key) = listing
    atVenue.updateWith(listing.venue)(keys => Some(keys.getOrElse(Set.empty) + listing.key))
  }
  private def release(key: ListingKey): Unit = held.remove(key).foreach(listing =>
    atVenue.updateWith(listing.venue)(_.map(_ - key).filter(_.nonEmpty)))

  private def forget(id: Int): Unit = families.remove(id).foreach { family =>
    family.resolved.listings.foreach(key => if (familyOfKey.get(key).contains(id)) familyOfKey.remove(key))
    family.resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.reads.keys.foreach(key => readersOf.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.queries.foreach(query => askersOf.updateWith(query)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.films.foreach(film => filmReadersOf.updateWith(film)(_.map(_ - id).filter(_.nonEmpty)))
  }

  private def remember(resolved: RegionFamily, digest: Long): Unit = {
    val id = nextFamily; nextFamily += 1
    families(id) = Family(resolved, digest)
    resolved.listings.foreach(key => familyOfKey(key) = id)
    resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.reads.keys.foreach(key => readersOf.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.queries.foreach(query => askersOf.updateWith(query)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.films.foreach(film => filmReadersOf.updateWith(film)(ids => Some(ids.getOrElse(Set.empty) + id)))
  }
}

object IncrementalResolver {
  /** `units` — each a set of listings resolved together, never split — merged where they overlap
   *  and packed, smallest first, into batches of at most `limit` listings (a unit larger alone). */
  def pack(units: Seq[Set[ListingKey]], limit: Int): Seq[Set[ListingKey]] = {
    // Overlapping units merged by a union-find over their indices, each listing naming the first unit holding it.
    val parent = Array.tabulate(units.size)(identity)
    def root(i: Int): Int = { var r = i; while (parent(r) != r) r = parent(r); var j = i
                              while (parent(j) != r) { val up = parent(j); parent(j) = r; j = up }; r }
    val firstUnit = mutable.HashMap.empty[ListingKey, Int]
    units.zipWithIndex.foreach { case (unit, i) =>
      unit.foreach(key => firstUnit.get(key).fold(firstUnit(key) = i)(other => parent(root(i)) = root(other)))
    }
    val merged = units.indices.groupMapReduce(root)(units(_))(_ ++ _).values.toSeq
    // Smallest first, ties by smallest listing: the packing is a function of the units, not of their order.
    val byFirst = ListingKey.ordering
    merged.map(unit => (unit.size, unit.min(using byFirst), unit)).sortWith { case ((sa, fa, _), (sb, fb, _)) =>
      sa < sb || (sa == sb && byFirst.lt(fa, fb)) }.map(_._3)
      .foldLeft(Vector.empty[Set[ListingKey]]) { (batches, unit) =>
        batches.lastOption match {
          case Some(open) if open.size + unit.size <= limit => batches.init :+ (open ++ unit)
          case _                                            => batches :+ unit
        }
      }
  }

  /** The most listings resolved together — in a full build or an event's update alike. A region is
   *  bounded by it unless one family is larger; 2,000 took over a minute a region in production. */
  val RegionBatch = 500

  /** The version of the rules a model's families are decided under: the resolver's code
   *  ([[IdentityRules.codeVersion]]), the calibration and decorations it runs with, and the pins.
   *  Stored families decided under another version are re-resolved on restore. */
  def rulesVersion(code: String, calibration: IdentityCalibration, decorations: TitleDecorations, pins: PinConstraints): String =
    Seq(code, calibration.hashCode, decorations.hashCode, pins.hashCode).mkString(":")

  /** How many of the largest families, and of each one's busiest nodes, the model reports. */
  val Largest = 3

  /** One family's size: its listings, evidence nodes and block keys, and the nodes most of its
   *  listings share (node key → listings). */
  final case class FamilySize(listings: Int, nodes: Int, blockKeys: Int, busiest: Seq[(String, Int)]) {
    // A node key joins its evidence fields with NULs and repeats its title across them: the first
    // field, the title as published, is what a log reader needs.
    def render: String = s"$listings listings / $nodes nodes / $blockKeys keys (${busiest.map { case (node, n) => s"${node.takeWhile(_ != '\u0000')}×$n" }.mkString(", ")})"
  }

  /** The model's largest families, the most nodes any family holds, and how many families are
   *  larger than one region. */
  final case class FamilySizes(largest: Seq[FamilySize], largestNodes: Int, large: Int) {
    def largestListings: Int = largest.headOption.fold(0)(_.listings)
    def render: String = s"$large families over $RegionBatch listings; largest ${if (largest.isEmpty) "none" else largest.map(_.render).mkString("; ")}"
  }

  /** Seconds spent in each part of the model's work. */
  final class Timings {
    private var contextNanos, resolveNanos, sliceNanos = 0L
    // The slowest region resolves: (seconds, listings), the costliest few kept.
    private var slowest = List.empty[(Double, Int)]
    private def timed[A](add: Long => Unit)(body: => A): A = { val start = System.nanoTime(); try body finally add(System.nanoTime() - start) }
    def context[A](body: => A): A  = timed(contextNanos += _)(body)
    def resolves[A](listings: Int)(body: => A): A = timed { nanos =>
      resolveNanos += nanos
      slowest = ((nanos / 1e9, listings) :: slowest).sortBy(-_._1).take(5)
    }(body)
    def slices[A](body: => A): A   = timed(sliceNanos += _)(body)
    def render: String = f"context ${contextNanos / 1e9}%.1fs, resolves ${resolveNanos / 1e9}%.1fs, slices ${sliceNanos / 1e9}%.1fs; " +
      s"slowest regions ${slowest.map { case (s, n) => f"$s%.1fs/$n" }.mkString(" ")}"
  }

  /** The teeth tests' mutants: each drops one reason a family is re-resolved, and the
   *  event-sequence spec must catch it. */
  enum Mutation {
    case None
    /** A family whose slice of the corpus context changed is left as it was. */
    case IgnoreContext
    /** A re-resolved family never pulls in a family it now shares a block key with. */
    case NoExpansion
    /** A restart takes every stored family as it was, whatever its corpus facts now read. */
    case TrustStored
  }
}
