package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.util.Random
import scala.util.hashing.MurmurHash3

/**
 * THE POPULATION PROPERTY (docs/investigations/2026-11-01-flip-unification.md, stage 1): as a title's listings only
 * ARRIVE, with every answer filed, no listing's decided take goes back to a value it left (A -> B -> A). The 10-30 plan's
 * P2 ("as inputs only move from Pending to Known, a take only refines") holds for answers by the typed awaits; nothing
 * held it for the population. Addams, Keswick, Prima Facie and Torrington flipped while listings only arrived.
 *
 * A population is walked along sampled ARRIVAL CHAINS: an order of its listings cut into nested prefixes (at most `steps`
 * of them), each prefix resolved whole. A take read while it is Pending is no take (P1). It is a measure, not a rule:
 * nothing here moves a take.
 *
 *  - [[flips]]: one population, its resolve handed in (a spec's corpus, a slice);
 *  - [[report]]: every multi-listing family of a whole corpus, each resolved alone against the corpus's context (A3) with
 *    the family's absent listings taken out of it ([[Without]]): one family's population varies, the rest stay fixed.
 */
object PopulationMonotonicity {

  /** A take as the property reads it: the film (`tmdb:<id>`, a fallback's id, or `none`), or `None` while Pending. */
  def takeOf(decision: ResolverDecision): Option[String] = decision.outcome match {
    case TakeOutcome.Pending(_)  => None
    case TakeOutcome.Taken(film) => Some(film)
    case TakeOutcome.NoFilm(_)   => Some(TakeFlips.NoFilm)
  }

  /** `count` arrival chains of `listings`: each a seeded order cut into at most `steps` nested prefixes, the last whole. */
  def chains(listings: Seq[Listing], seed: Long, count: Int, steps: Int): Seq[Seq[Seq[Listing]]] = {
    val sorted = listings.sorted.distinctBy(_.key)
    (0 until count).map { c =>
      val order = new Random(seed * 31 + c).shuffle(sorted)
      val size  = math.max(1, math.ceil(order.size.toDouble / math.max(1, steps)).toInt)
      ((size until order.size by size).map(order.take) :+ order).distinctBy(_.size)
    }
  }

  /** Does a take sequence go back to a value it left? Pending steps (`None`) are skipped; repeats collapse. */
  def returns(takes: Seq[Option[String]]): Boolean = {
    val decided = collapsed(takes.flatten)
    decided.indices.exists(i => decided.indexOf(decided(i)) < i)
  }

  private def collapsed(takes: Seq[String]): Vector[String] =
    takes.foldLeft(Vector.empty[String])((seen, take) => if (seen.lastOption.contains(take)) seen else seen :+ take)

  /** A listing whose take went back to a value it left along arrival chain `chain`: (prefix size, take) at each step. */
  final case class Flip(key: ListingKey, chain: Int, takes: Seq[(Int, Option[String])]) {
    /** The decided takes, each with the prefix size it was first taken at. */
    def path: String = takes.collect { case (n, Some(take)) => (n, take) }
      .foldLeft(Vector.empty[(Int, String)])((seen, step) => if (seen.lastOption.exists(_._2 == step._2)) seen else seen :+ step)
      .map { case (n, take) => s"$take@$n" }.mkString(" -> ")
    /** Its shape: between two films, or a film dropped to none and given back, or none taken a film and dropped again. */
    def shape: String = {
      val decided = collapsed(takes.flatMap(_._2))
      val back    = decided.indices.find(i => decided.indexOf(decided(i)) < i).map(decided).getOrElse("")
      if (back == TakeFlips.NoFilm) "none-film-none"
      else if (decided.contains(TakeFlips.NoFilm)) "film-none-film"
      else "film-film-film"
    }
  }

  /** The flips of one population along `count` chains, `resolve` a whole resolve of a subset, its takes by key. */
  def flips(listings: Seq[Listing], resolve: Seq[Listing] => Map[ListingKey, Option[String]], seed: Long, count: Int, steps: Int): Seq[Flip] =
    chains(listings, seed, count, steps).zipWithIndex.flatMap { case (chain, c) =>
      val takes = chain.map(prefix => prefix.size -> resolve(prefix))
      listings.map(_.key).distinct.flatMap { key =>
        val sequence = takes.flatMap { case (n, taken) => taken.get(key).map(n -> _) }
        Option.when(returns(sequence.map(_._2)))(Flip(key, c, sequence))
      }
    }

  /** A whole resolve of `listings` (no corpus beyond them), every listing's take as [[takeOf]] reads it. */
  def wholeTakes(lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs)(listings: Seq[Listing]): Map[ListingKey, Option[String]] = {
    val r = IdentityResolver.resolve(listings, lookups, normalizer, inputs)
    listings.flatMap(l => r.decisionOf.get(l.key).map(d => l.key -> takeOf(d))).toMap
  }

  // ── the corpus report ──────────────────────────────────────────────────────────────────────

  /** One family's flips: its smallest listing's title, how many listings it holds, its flips and which of them are of a
   *  BARE listing (one publishing no fact of its own). */
  final case class FamilyFlips(title: String, listings: Int, flips: Seq[Flip], bare: Set[ListingKey])

  /** A corpus's report: the multi-listing families, how many were walked (the rest past `maxFamily` or the time budget),
   *  the families that flipped, how many listings' whole-family take differed from the corpus resolve's (A3: 0 expected),
   *  and the CONTESTED bare listings of the corpus resolve ([[Resolution.contested]]), each with its take there. */
  final case class Report(families: Int, walked: Int, tooLarge: Int, outOfTime: Int, seconds: Double, flipped: Seq[FamilyFlips],
                          apart: Int, contested: Seq[(ListingKey, Option[String], Map[Int, Set[String]])]) {
    def flips: Seq[(FamilyFlips, Flip)] = flipped.flatMap(f => f.flips.map(f -> _))
    def listingsFlipped: Int = flipped.map(_.flips.map(_.key).distinct.size).sum
    def bareFlipped: Int     = flipped.map(f => f.flips.map(_.key).distinct.count(f.bare)).sum
    def byShape: Map[String, Int] = flips.map(_._2).groupBy(_.key).values.map(_.head.shape).groupMapReduce(identity)(_ => 1)(_ + _)
    /** The contested bare listings taking a film now: what stage 5 would move to none. */
    def contestedTaking: Int = contested.count(_._2.exists(_ != TakeFlips.NoFilm))
    def line(country: String): String =
      f"kind=identity.population_flips country=$country families=$families walked=$walked too_large=$tooLarge out_of_time=$outOfTime " +
        f"families_flipped=${flipped.size} listings_flipped=$listingsFlipped bare_flipped=$bareFlipped " +
        byShape.toSeq.sorted.map { case (shape, n) => s"shape_$shape=$n" }.mkString(" ") +
        f" a3_apart=$apart contested_bare=${contested.size} contested_bare_taking=$contestedTaking seconds=$seconds%.0f"
  }

  /** Every family of `whole` (the corpus resolve of `listings`) holding two listings or more, one of them `focus`ed, at most `maxFamily`, walked
   *  along `count` arrival chains of at most `steps` steps, smallest family id first, until `budgetSeconds` is spent on
   *  `stopwatch` (the caller's: a tool's composition root hands down its own). On `threads` threads: each family's walk
   *  reads the corpus's context and answers and writes nothing they hold (its resolves build their own generation and
   *  scope), so families walk side by side and their findings are gathered in family order -- the same report as one
   *  thread's while the budget is not spent (a family starts, on any thread, only while it is not). */
  def report(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs, whole: Resolution,
             stopwatch: tools.Stopwatch, count: Int = 4, steps: Int = 6, budgetSeconds: Double = 900, maxFamily: Int = 300,
             focus: ListingKey => Boolean = _ => true, threads: Int = 1): Report = {
    val started    = stopwatch.start()
    def elapsed    = started.seconds
    val all        = listings.sorted.distinctBy(_.key)
    val generation = new CandidateGeneration(all, lookups, normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val context    = generation.context
    val nodeOf     = generation.nodes.flatMap(node => node.listings.map(_.key -> node)).toMap
    val without    = new Without.Index(generation.nodes)
    val families   = all.groupBy(l => whole.familyOf.getOrElse(l.key, -1)).toSeq.filter { case (id, ls) => id >= 0 && ls.sizeIs >= 2 && ls.exists(l => focus(l.key)) }.sortBy(_._1)
    val (fits, large) = families.partition(_._2.sizeIs <= maxFamily)
    // one family walked: its flips, if any, and how many of its listings' whole-family take differed from the corpus's
    def walk(members: Seq[Listing]): (Option[FamilyFlips], Int) = {
      var apart = 0
      val keys  = members.map(_.key).toSet
      def resolve(prefix: Seq[Listing]): Map[ListingKey, Option[String]] = {
        val present = prefix.map(_.key).toSet
        val r = IdentityResolver.resolveWith(prefix, lookups, normalizer, inputs.calibration, IdentityResolver.Mutation.None, inputs.pins,
          inputs.decorations, Some(without(context, keys -- present)))
        val taken = prefix.flatMap(l => r.decisionOf.get(l.key).map(d => l.key -> takeOf(d))).toMap
        if (prefix.sizeIs == members.size) apart += members.count(l => whole.decisionOf.get(l.key).map(takeOf) != taken.get(l.key))
        taken
      }
      val seed  = MurmurHash3.stringHash(members.head.sortKey).toLong
      val found = flips(members, resolve, seed, count, steps)
      (Option.when(found.nonEmpty)(FamilyFlips(members.head.rawTitle, members.size, found,
        keys.filter(key => nodeOf.get(key).exists(!_.evidence.measured.publishesAFact)))), apart)
    }
    // a family is walked only when it starts inside the budget; none when it does not
    def walkedInTime(members: Seq[Listing]): Option[(Option[FamilyFlips], Int)] = Option.when(elapsed < budgetSeconds)(walk(members))
    val walks: Seq[Option[(Option[FamilyFlips], Int)]] =
      if (threads <= 1 || fits.sizeIs < 2) fits.map { case (_, members) => walkedInTime(members) }
      else {
        // the caller's switched-off rules, on every thread the walk runs on (RuleAblation holds per thread)
        val off  = RuleAblation.switchedOff
        val pool = tools.DaemonExecutors.boundedEC("population-walk", threads)
        try fits.map { case (_, members) => pool.submit(() => RuleAblation.disabling(off)(walkedInTime(members))) }.map(_.get)
        finally { val _ = pool.shutdownNow() }
      }
    val walked    = walks.count(_.isDefined)
    val apart     = walks.flatten.map(_._2).sum
    val flipped   = walks.flatten.flatMap(_._1)
    val contested = whole.contested.toSeq.sortBy(_._1)(using ListingKey.ordering).map { case (key, backed) =>
      (key, whole.decisionOf.get(key).flatMap(takeOf), backed) }
    Report(families.size, walked, large.size, fits.size - walked, elapsed, flipped, apart, contested)
  }

  /**
   * The corpus's context with some listings ABSENT: their title-group entries ([[CorpusContext.titleGroup]], what a film's
   * backing reads) and their nodes' reached candidates ([[CorpusContext.reachedBy]]) gone, a node that keeps a present
   * listing named by its smallest present one, as a resolve of the present listings names it. The corpus's learned
   * facts (venue titles, houses, whole titles and pieces) stay as the whole corpus learned them: a family's own few
   * listings move them rarely, and the report says it reads them so.
   */
  object Without {
    /** The corpus's nodes, indexed for [[Without]]: each title key's entries by listing, and each listing's node. */
    final class Index(nodes: Seq[EvidenceNode]) {
      private[PopulationMonotonicity] val groups: Map[String, Seq[(ListingKey, (String, IdentityMeasures.Listing))]] =
        nodes.flatMap(node => node.listings.map(l => CorpusContext.titleOf(node) -> (l.key, Backer.of(l) -> node.evidence.measured))).groupMap(_._1)(_._2)
      private[PopulationMonotonicity] val nodeOf: Map[ListingKey, EvidenceNode] = nodes.flatMap(node => node.listings.map(_.key -> node)).toMap
      def apply(context: CorpusContext, absent: Set[ListingKey]): CorpusContext =
        if (absent.isEmpty) context else new Without(context, absent, this)
    }
  }

  private final class Without(context: CorpusContext, absent: Set[ListingKey], index: Without.Index) extends CorpusContext {
    private val touched: Map[String, EvidenceNode] = absent.flatMap(index.nodeOf.get).map(node => node.id -> node).toMap
    private val titles: Set[String]               = touched.values.map(CorpusContext.titleOf).toSet
    // a touched node's present name: its smallest present listing's sort key, none when every listing is absent
    private def renamed(id: String): Option[String] = touched.get(id).fold(Option(id))(node =>
      node.listings.filterNot(l => absent(l.key)).map(_.sortKey).minOption)
    def candidate(id: Int): Option[Candidate] = context.candidate(id)
    def houses: StageRelays.Houses = context.houses
    def houseRanking: Map[String, Seq[StageRelays.Houses.Contender]] = context.houseRanking
    def titleGroup(key: String): Seq[(String, IdentityMeasures.Listing)] =
      if (!titles(key)) context.titleGroup(key) else index.groups.getOrElse(key, Nil).collect { case (k, entry) if !absent(k) => entry }
    def wholeTitle(sanitised: String): Boolean = context.wholeTitle(sanitised)
    def segmentSpread(sanitised: String): Int = context.segmentSpread(sanitised)
    def reachedBy(key: String): Seq[(String, Set[Int])] =
      if (!titles(key)) context.reachedBy(key) else context.reachedBy(key).flatMap { case (id, reached) => renamed(id).map(_ -> reached) }
    def ranked(query: CandidateQuery): Option[Seq[Int]] = context.ranked(query)
    override def evidenceOf(listing: Listing): Option[Evidence] = context.evidenceOf(listing)
  }
}
