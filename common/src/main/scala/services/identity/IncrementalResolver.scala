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
                                mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None) {
  import IdentityResolver.RegionFamily
  import IncrementalResolver.Mutation

  private final case class Family(resolved: RegionFamily, slice: CorpusContext.Slice) {
    val storeId: String = StoredFamily.idOf(resolved.listings)
  }

  private val held          = mutable.HashMap.empty[ListingKey, Listing]
  private val atVenue       = mutable.HashMap.empty[String, Set[ListingKey]]
  private val corpus        = new LiveCorpus(lookups, normalizer, pins, decorations)
  private val families      = mutable.HashMap.empty[Int, Family]
  private val readersOf     = mutable.HashMap.empty[String, Set[Int]]
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
    val asked   = families.collect { case (id, family) if family.resolved.queries.exists(answered.queries) || family.resolved.films.exists(answered.films) => id }
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
    standing.foreach(family => remember(family.family, corpus))
    store.replace(fallen.map(_.id).toSet, Nil)
    val loose = held.keys.filterNot(familyOfKey.contains).toSeq
    clock.context(corpus.titleComponents(loose)).foreach(component => update(Set.empty, component.toSet, CorpusContext.Changed.None))
    ruled()
  }

  /** Take `listings` in whole — a new country, or a rebuild after the rules changed — one title-key
   *  component at a time, so the build holds one component's answers and records, never the corpus's. */
  def seed(listings: Seq[Listing]): Unit = {
    val arrived = listings.filterNot(listing => held.get(listing.key).contains(listing))
    arrived.foreach(hold)
    val moved = clock.context(corpus.seen(arrived))
    clock.context(corpus.titleComponents(arrived.map(_.key))).zipWithIndex.foreach { case (component, index) =>
      update(component.flatMap(familyOfKey.get).toSet, component.toSet, if (index == 0) moved else CorpusContext.Changed.None)
    }
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
                else clock.slices(moved.keys.flatMap(readersOf.getOrElse(_, Set.empty)).filter(id => context.slice(families(id).resolved.reads) != families(id).slice))
    val replaced = mutable.HashSet.empty[Int] ++ touched ++ stale
    val settled  = mutable.LinkedHashMap.empty[Set[ListingKey], RegionFamily]
    var region   = (touched ++ stale).flatMap(id => families(id).resolved.listings) ++ arrived
    // Resolve the region; a family it produced that now shares a block key with a family left
    // alone — or with one this update already settled — pulls that one in and is resolved again
    // beside it. Every other result is final.
    while (region.nonEmpty) {
      val result = clock.resolves(IdentityResolver.resolveRegion(region.flatMap(held.get), lookups, normalizer, calibration, pins, decorations, context))
      reResolved += result.size
      val joins  = result.map { family =>
        if (mutation == Mutation.NoExpansion) (family, Set.empty[Int], Seq.empty[RegionFamily])
        else (family, family.blockKeys.flatMap(key => familiesOfKey.getOrElse(key, Set.empty)) -- replaced,
              settled.values.filter(_.blockKeys.exists(family.blockKeys)).toSeq)
      }
      joins.collect { case (family, joined, back) if joined.isEmpty && back.isEmpty => settled(family.listings) = family }
      val joined = joins.flatMap(_._2).toSet
      val back   = joins.flatMap(_._3).distinct
      back.foreach(family => settled.remove(family.listings))
      replaced ++= joined
      region = joins.collect { case (family, pulled, again) if pulled.nonEmpty || again.nonEmpty => family.listings }.flatten.toSet ++
        joined.flatMap(id => families(id).resolved.listings) ++ back.flatMap(_.listings)
    }
    val removed = replaced.flatMap(id => families.get(id).map(_.storeId)).toSet
    replaced.foreach(forget)
    clock.slices(settled.values.foreach(remember(_, context)))
    val added   = settled.values.toSeq.flatMap(family => familyOfKey.get(family.listings.head).flatMap(families.get))
      .map(family => StoredFamily(family.storeId, family.resolved, family.slice.digest))
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
  }

  private def remember(resolved: RegionFamily, context: CorpusContext): Unit = {
    val id = nextFamily; nextFamily += 1
    families(id) = Family(resolved, context.slice(resolved.reads))
    resolved.listings.foreach(key => familyOfKey(key) = id)
    resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.reads.keys.foreach(key => readersOf.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
  }
}

object IncrementalResolver {
  /** The version of the rules a model's families are decided under: the deployment (its commit —
   *  the resolver's code and the calibration and decorations it ships), and the pins. Stored
   *  families decided under another version are re-resolved on restore. */
  def rulesVersion(commit: String, calibration: IdentityCalibration, decorations: TitleDecorations, pins: PinConstraints): String =
    Seq(commit, calibration.hashCode, decorations.hashCode, pins.hashCode).mkString(":")

  /** Seconds spent in each part of the model's work. */
  final class Timings {
    private var contextNanos, resolveNanos, sliceNanos = 0L
    private def timed[A](add: Long => Unit)(body: => A): A = { val start = System.nanoTime(); try body finally add(System.nanoTime() - start) }
    def context[A](body: => A): A  = timed(contextNanos += _)(body)
    def resolves[A](body: => A): A = timed(resolveNanos += _)(body)
    def slices[A](body: => A): A   = timed(sliceNanos += _)(body)
    def render: String = f"context ${contextNanos / 1e9}%.1fs, resolves ${resolveNanos / 1e9}%.1fs, slices ${sliceNanos / 1e9}%.1fs"
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
