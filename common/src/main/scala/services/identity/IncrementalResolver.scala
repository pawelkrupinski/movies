package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.mutable

/** Which lookups gained an answer since the model last read them: the fill filed them. */
final case class AnswersChanged(queries: Set[CandidateQuery], films: Set[Int])

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
                                mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None) {
  import IdentityResolver.RegionFamily
  import IncrementalResolver.Mutation

  private final case class Family(resolved: RegionFamily, slice: CorpusContext.Slice)

  private val held          = mutable.HashMap.empty[ListingKey, Listing]
  private val families      = mutable.HashMap.empty[Int, Family]
  private val familyOfKey   = mutable.HashMap.empty[ListingKey, Int]
  private val familiesOfKey = mutable.HashMap.empty[String, Set[Int]]
  private var nextFamily    = 0
  private var reResolved    = 0

  def listingsSeen(listings: Seq[Listing]): Unit = {
    val changed = listings.filterNot(listing => held.get(listing.key).contains(listing))
    changed.foreach(listing => held(listing.key) = listing)
    update(changed.flatMap(listing => familyOfKey.get(listing.key)).toSet, changed.map(_.key).toSet)
  }

  def listingsGone(keys: Seq[ListingKey]): Unit = {
    val gone = keys.filter(held.contains)
    held --= gone
    update(gone.flatMap(familyOfKey.get).toSet, Set.empty)
  }

  def answersChanged(changed: AnswersChanged): Unit =
    update(families.collect { case (id, family) if family.resolved.queries.exists(changed.queries) || family.resolved.films.exists(changed.films) => id }.toSet,
      Set.empty)

  /** Every decision, in the whole resolve's order. */
  def decisions: Seq[ResolverDecision] = families.values.flatMap(_.resolved.decisions).toSeq.sortBy(_.members.head)(using ListingKey.ordering)
  /** Each listing's family. */
  def familyOf: Map[ListingKey, Int] = familyOfKey.toMap
  /** How many families the model has re-resolved since it was made: the work its events cost. */
  def familiesResolved: Int = reResolved

  private def update(touched: Set[Int], arrived: Set[ListingKey]): Unit = {
    val context = IdentityResolver.contextOf(held.values, lookups, normalizer, pins, decorations)
    val stale   = if (mutation == Mutation.IgnoreContext) Set.empty[Int]
                  else families.collect { case (id, family) if context.slice(family.resolved.reads) != family.slice => id }.toSet
    var region  = touched ++ stale
    var loose   = arrived
    var result  = Seq.empty[RegionFamily]
    var stable  = region.isEmpty && loose.isEmpty
    while (!stable) {
      val listings = (region.flatMap(id => families(id).resolved.listings) ++ loose).flatMap(held.get)
      result = if (listings.isEmpty) Nil
               else IdentityResolver.resolveRegion(listings, lookups, normalizer, calibration, pins, decorations, context)
      val joined = if (mutation == Mutation.NoExpansion) Set.empty[Int]
                   else result.flatMap(_.blockKeys).flatMap(key => familiesOfKey.getOrElse(key, Set.empty)).toSet -- region
      stable = joined.isEmpty
      region ++= joined
    }
    region.foreach(forget)
    result.foreach(remember(_, context))
    reResolved += result.size
  }

  private def forget(id: Int): Unit = families.remove(id).foreach { family =>
    family.resolved.listings.foreach(key => if (familyOfKey.get(key).contains(id)) familyOfKey.remove(key))
    family.resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)))
  }

  private def remember(resolved: RegionFamily, context: CorpusContext): Unit = {
    val id = nextFamily; nextFamily += 1
    families(id) = Family(resolved, context.slice(resolved.reads))
    resolved.listings.foreach(key => familyOfKey(key) = id)
    resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
  }
}

object IncrementalResolver {
  /** The teeth tests' mutants: each drops one reason a family is re-resolved, and the
   *  event-sequence spec must catch it. */
  enum Mutation {
    case None
    /** A family whose slice of the corpus context changed is left as it was. */
    case IgnoreContext
    /** A re-resolved family never pulls in a family it now shares a block key with. */
    case NoExpansion
  }
}
