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
                                mutation: IncrementalResolver.Mutation = IncrementalResolver.Mutation.None,
                                traces: IdentityTraceSink = IdentityTraceSink.Discard) {
  import IdentityResolver.RegionFamily
  import IncrementalResolver.Mutation

  // A family keeps the DIGEST of the corpus facts it read, not the facts: comparing digests tells
  // whether it still reads the same, and it holds no copy of another family's records.
  private final case class Family(resolved: RegionFamily, digest: Long) {
    val storeId: String = StoredFamily.idOf(resolved.listings)
    lazy val nodes: Int = resolved.nodeKeys.values.toSet.size
  }

  // What the model knows per listing key — the listing it holds (null once released) and the family
  // it is in (-1 for none) — in ONE entry: two maps over the same keys kept a hash node each per
  // listing and boxed every family id (~12 MB on worker-us). A key goes when both are gone: a released
  // listing's family still claims it until that family is forgotten.
  private val byKey         = mutable.HashMap.empty[ListingKey, IncrementalResolver.Keyed]
  private val atVenue       = mutable.HashMap.empty[String, Set[ListingKey]]
  private val corpus        = new LiveCorpus(lookups, normalizer, pins, decorations)
  private val families      = mutable.HashMap.empty[Int, Family]
  private val readersOf     = mutable.HashMap.empty[String, Set[Int]]
  // Which families asked each question and read each record: what an answer event re-resolves.
  private val askersOf      = mutable.HashMap.empty[CandidateQuery, Set[Int]]
  private val filmReadersOf = mutable.LongMap.empty[Set[Int]]
  private val familiesOfKey = mutable.HashMap.empty[String, Set[Int]]
  // Family ids number the LIVE families: a freed id is taken again, smallest first, so ids stay
  // below the family count and inside the worker's raised small-integer cache
  // (`-XX:AutoBoxCacheMax`, infra/jvm/worker.options) — boxed in five maps and sets, a counter that
  // only grew made each box a fresh Integer (~10 MB on worker-us). Smallest-first keeps the ids a
  // pure function of the event sequence.
  private val freedIds      = mutable.BitSet.empty
  private var nextFamily    = 0
  private var reResolved    = 0
  private val clock         = new IncrementalResolver.Timings

  def listingsSeen(listings: Seq[Listing]): Unit       = batch(listings, Nil, AnswersChanged.Empty)
  def listingsGone(keys: Seq[ListingKey]): Unit        = batch(Nil, keys, AnswersChanged.Empty)

  /** The listings the model holds at `venue`: what a venue's next scrape is diffed against. */
  def heldAt(venue: String): Set[ListingKey] = atVenue.getOrElse(venue, Set.empty)
  /** Every listing the model holds. */
  // An array, not a list: every projection reads it (~100k on worker-us), and a cell per listing was ~2.4 MB a projection.
  def listings: Seq[Listing] = byKey.valuesIterator.flatMap(k => Option(k.listing)).to(scala.collection.immutable.ArraySeq)
  def answersChanged(changed: AnswersChanged): Unit    = batch(Nil, Nil, changed)

  /** Several events at once — scrapes and answers that landed together — resolving a family they
   *  all touch once, not once per event. */
  def batch(seen: Seq[Listing], gone: Seq[ListingKey], answered: AnswersChanged): Unit = {
    val left    = gone.filter(isHeld)
    left.foreach(release)
    // A listing whose detail page was answered anew is seen again: its evidence may have moved —
    // also when its venue re-published it unchanged in the same batch, which alone would not.
    // (A listing that left in this batch is released already, so it is held no more.)
    val redetailed = answered.details.flatMap(heldListing)
    val fresh      = seen.filterNot(listing => heldListing(listing.key).contains(listing))
    // An equal listing re-published with another poster, other screening days or names resolves nothing again, but is
    // held in its place, under the held key object its families name: the stages after the model read those fields
    // off the model's listings (`listings`, which the projection's own listings adopt).
    seen.foreach(listing => byKey.get(listing.key).foreach { keyed =>
      val held = keyed.listing
      if (held != null && (held ne listing) && held == listing && Listing.movedOutsideEquality(held, listing))
        keyed.listing = listing.copy(key = held.key)
    })
    val freshKeys  = fresh.iterator.map(_.key).toSet
    val arrived    = fresh ++ redetailed.filterNot(listing => freshKeys(listing.key))
    arrived.foreach(hold)
    val moved   = clock.context(corpus.gone(left) ++ corpus.seen(arrived) ++ corpus.answered(answered))
    val asked   = answered.queries.flatMap(askersOf.getOrElse(_, Set.empty)) ++ answered.films.flatMap(filmReadersOf.getOrElse(_, Set.empty))
    update((left ++ arrived.map(_.key)).flatMap(familyOf).toSet ++ asked, arrived.map(_.key).toSet, moved)
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
    val (stored, outruled) =
      if (store.rulesVersion.contains(rules)) (store.families(), Set.empty[String]) else (Nil, store.families().map(_.id).toSet)
    // The keys two stored families claim, found in one pass — not by grouping every stored key.
    val claimed = { val once = mutable.HashSet.empty[ListingKey]
                    stored.iterator.flatMap(_.family.listings).filterNot(once.add).toSet }
    val (standing, fallen) = stored.partition { family =>
      family.family.listings.forall(key => isHeld(key) && !claimed(key)) &&
        family.family.nodeKeys.forall { case (key, node) => corpus.nodeKeyOf(key).contains(node) } &&
        (mutation == Mutation.TrustStored || clock.slices(corpus.slice(family.family.reads).digest) == family.digest)
    }
    // A standing family's slice digests to what it stored — the check above just computed it — so it
    // is kept, not digested again (half a UK take-up's hashing when it was).
    standing.foreach(family => remember(family.family, family.digest))
    // Dropped only AFTER the re-resolve, and only where it decided no family under the same id: most
    // re-resolved families decide what they did before, and their stored family and traces are then
    // compared and kept rather than deleted and written again (all ~165k US traces, every boot after
    // a rules change, when they were dropped first). In a `finally`, so a failed resolve still drops them.
    val dropped = outruled ++ fallen.map(_.id)
    try update(Set.empty, byKey.collect { case (key, k) if k.listing != null && k.family < 0 => key }.toSet, CorpusContext.Changed.None)
    finally {
      val gone = dropped -- families.valuesIterator.map(_.storeId)
      store.replace(gone, Nil)
      traces.settle(gone, Nil)
    }
    ruled()
  }

  /** Take `listings` in whole — a new country, or a rebuild after the rules changed — resolved a
   *  bounded batch at a time, so the build holds one batch's answers and records, never the corpus's. */
  def seed(listings: Seq[Listing]): Unit = {
    val arrived = listings.filterNot(listing => heldListing(listing.key).contains(listing))
    arrived.foreach(hold)
    val moved = clock.context(corpus.seen(arrived))
    update(arrived.flatMap(listing => familyOf(listing.key)).toSet, arrived.map(_.key).toSet, moved)
  }

  /** The model as a [[Resolution]] — what the projection reads: its decisions and the records of the films it decided.
   *  It carries no families (an empty `familyOf`: read [[familyOf]] on the model's thread) and counts no edges or
   *  lookups, which are a whole resolve's. Every projection reads one, and a map of every listing's family was a third
   *  of worker-us's model thread (2026-10-05) for a reader that never looked at it. */
  def resolution: Resolution = {
    val decided = decisions
    Resolution(decided, corpus.nodeCount, Map.empty, Nil, Nil, 0, 0, 0, 0, 0,
      decided.flatMap(_.film).distinct.flatMap(id => corpus.candidate(id).map(id -> _.film)).toMap)
  }

  /** The questions and records the model's lookups do not know yet: what a fill should ask. */
  def gaps: AnswersChanged = corpus.gaps

  /** Every decision, in the whole resolve's order. */
  def decisions: Seq[ResolverDecision] = families.values.flatMap(_.resolved.decisions).toSeq.sortBy(_.members.head)(using ListingKey.ordering)
  /** Each listing's family. */
  def familyOf: Map[ListingKey, Int] = byKey.collect { case (key, k) if k.family >= 0 => key -> k.family }.toMap
  /** Each family's candidate questions, with its decisions: what the fill picks aged questions from. */
  def familyQuestions: Seq[(Set[CandidateQuery], Seq[ResolverDecision])] =
    families.values.toSeq.map(f => (f.resolved.queries, f.resolved.decisions))

  /** Every listing key the families name — their listings, node maps and decisions' members — and each node text:
   *  what the one-object-per-listing checks read. */
  private[identity] def familyKeys: Iterator[ListingKey] = families.valuesIterator.flatMap(family =>
    family.resolved.listings.iterator ++ family.resolved.nodeKeys.keysIterator ++ family.resolved.decisions.iterator.flatMap(_.members))
  private[identity] def familyNodeTexts: Iterator[(ListingKey, String)] = families.valuesIterator.flatMap(_.resolved.nodeKeys)
  private[identity] def nodeTextOf(key: ListingKey): Option[String] = corpus.nodeKeyOf(key)

  /** How many families the model holds, and how many listings. */
  def familyCount: Int = families.size
  def heldCount: Int   = heldListings
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
    // The settled families by block key: what a result looks its settled neighbours up in, not a scan
    // of every family settled so far (a seed settles them all, so the scan was quadratic in families).
    val settledOfKey = mutable.HashMap.empty[String, mutable.Set[Set[ListingKey]]]
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
        val listings = batch.toSeq.flatMap(heldListing)
        if (listings.nonEmpty) {
          val result = clock.resolves(listings.size)(IdentityResolver.resolveRegion(listings, lookups, normalizer, calibration, pins, decorations, context))
          reResolved += result.size
          result.foreach { family =>
            val joined = if (mutation == Mutation.NoExpansion) Set.empty[Int]
                         else family.blockKeys.flatMap(key => familiesOfKey.getOrElse(key, Set.empty)) -- replaced
            val back   = if (mutation == Mutation.NoExpansion) Seq.empty[RegionFamily]
                         else family.blockKeys.iterator.flatMap(settledOfKey.getOrElse(_, Nil)).distinct.flatMap(settled.get)
                           .filter(_.blockKeys.exists(family.blockKeys)).toSeq
            if (joined.isEmpty && back.isEmpty) {
              settled(family.listings) = family
              family.blockKeys.foreach(key => settledOfKey.getOrElseUpdate(key, mutable.HashSet.empty) += family.listings)
            } else {
              back.foreach { again =>
                settled.remove(again.listings)
                again.blockKeys.foreach(key => settledOfKey.get(key).foreach(_ -= again.listings))
              }
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
    // The rules behind each settled family's decisions go to the trace store — built there, off this thread, family
    // by family, from the family as resolved and each of its listings' venue and title — and the model keeps the
    // decisions without them. One family's hand-over holds only its own: a store may drop it unbuilt, once a later
    // update re-resolves the family. The canonical and search tiers are read once per cleaned title across the batch.
    val byClean = mutable.HashMap.empty[String, Seq[String]]
    val traced = settled.values.toSeq.map { family =>
      val id     = StoredFamily.idOf(family.listings)
      val titled = family.listings.iterator.flatMap(key => heldListing(key).map(listing => key -> (listing.cinema, listing.rawTitle))).toMap
      SettledFamily(id, family, key =>
        titled.get(key).fold(Seq.empty[String]) { case (cinema, raw) =>
          byClean.synchronized(normalizer.firedRules(cinema, raw, byClean)) }, Some(calibration))
    }
    clock.slices(settled.values.foreach(family => remember(family.copy(decisions = family.decisions.map(_.copy()(DecisionTrace.Empty))),
      context.slice(family.reads).digest)))
    val added   = settled.values.toSeq.flatMap(family => familyOf(family.listings.head).flatMap(families.get))
      .map(family => StoredFamily(family.storeId, family.resolved, family.digest))
    store.replace(removed -- added.map(_.id), added)
    traces.settle(removed, traced)
    ruled()
  }

  /** The store's families are this model's: record the rules they are decided under, once. */
  private var rulesRecorded = false
  private def ruled(): Unit = if (!rulesRecorded) { store.recordRulesVersion(rules); rulesRecorded = true }

  // A listing is filed under its OWN key object — the one its families, decisions and node maps are made to name
  // ([[remember]]), and the corpus files it under: a listing re-published with other fields comes with a new one,
  // and the model's maps would otherwise keep the old beside it.
  private def hold(listing: Listing): Unit = {
    val keyed = byKey.get(listing.key) match {
      case Some(was) if was.listing != null && (was.listing.key eq listing.key) => was
      case was                                                                  =>
        byKey.remove(listing.key)
        val entry = was.getOrElse(new IncrementalResolver.Keyed)
        byKey(listing.key) = entry
        entry
    }
    if (keyed.listing == null) heldListings += 1
    keyed.listing = listing
    atVenue.updateWith(listing.venue)(keys => Some(keys.getOrElse(Set.empty) - listing.key + listing.key))
  }
  private def release(key: ListingKey): Unit = heldListing(key).foreach { listing =>
    val keyed = byKey(key)
    keyed.listing = null; heldListings -= 1
    if (keyed.family < 0) byKey.remove(key)
    atVenue.updateWith(listing.venue)(_.map(_ - key).filter(_.nonEmpty))
  }

  private var heldListings = 0
  private def isHeld(key: ListingKey): Boolean = byKey.get(key).exists(_.listing != null)
  private def heldListing(key: ListingKey): Option[Listing] = byKey.get(key).flatMap(k => Option(k.listing))
  private def familyOf(key: ListingKey): Option[Int] = byKey.get(key).collect { case k if k.family >= 0 => k.family }

  private def forget(id: Int): Unit = families.remove(id).foreach { family =>
    freedIds += id
    family.resolved.listings.foreach(key => byKey.get(key).filter(_.family == id).foreach { keyed =>
      keyed.family = -1
      if (keyed.listing == null) byKey.remove(key)
    })
    family.resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.reads.keys.foreach(key => readersOf.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.queries.foreach(query => askersOf.updateWith(query)(_.map(_ - id).filter(_.nonEmpty)))
    family.resolved.films.foreach(film => filmReadersOf.updateWith(film)(_.map(_ - id).filter(_.nonEmpty)))
  }

  /** The held listing's own key object for `key` (`key` itself for a listing no longer held). */
  private def heldKey(key: ListingKey): ListingKey = heldListing(key).fold(key)(_.key)
  /** The corpus's object for a node's `text` when it is the node `key` is one of now. */
  private def heldText(key: ListingKey, text: String): String = corpus.nodeKeyOf(key).filter(_ == text).getOrElse(text)

  private def remember(family: RegionFamily, digest: Long): Unit = {
    val resolved = family.sharing(heldKey, heldText)
    val id = freedIds.headOption.fold { val fresh = nextFamily; nextFamily += 1; fresh } { free => freedIds -= free; free }
    families(id) = Family(resolved, digest)
    resolved.listings.foreach(key => byKey.getOrElseUpdate(key, new IncrementalResolver.Keyed).family = id)
    resolved.blockKeys.foreach(key => familiesOfKey.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.reads.keys.foreach(key => readersOf.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.queries.foreach(query => askersOf.updateWith(query)(ids => Some(ids.getOrElse(Set.empty) + id)))
    resolved.films.foreach(film => filmReadersOf.updateWith(film)(ids => Some(ids.getOrElse(Set.empty) + id)))
  }
}

object IncrementalResolver {
  /** One listing key's entry: the listing held (null once released) and its family (-1 for none). */
  private final class Keyed { var listing: Listing = null; var family: Int = -1 }

  /** `units` — each a set of listings resolved together, never split — merged where they overlap
   *  and packed, smallest first, into batches of at most `limit` listings (a unit larger alone). */
  def pack(handed: Seq[Set[ListingKey]], limit: Int): Seq[Set[ListingKey]] = {
    // Indexed: the merge reads units by index, and an update hands them over as a list.
    val units = handed.toIndexedSeq
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
    private val contexts, resolving, slicing = tools.Stopwatch.total()
    // The slowest region resolves: (seconds, listings), the costliest few kept.
    private var slowest = List.empty[(Double, Int)]
    def context[A](body: => A): A  = contexts(body)
    def resolves[A](listings: Int)(body: => A): A = {
      val started = tools.Stopwatch.start()
      try resolving(body) finally slowest = ((started.seconds, listings) :: slowest).sortBy(-_._1).take(5)
    }
    def slices[A](body: => A): A   = slicing(body)
    def render: String = f"context ${contexts.seconds}%.1fs, resolves ${resolving.seconds}%.1fs, slices ${slicing.seconds}%.1fs; " +
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
