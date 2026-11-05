package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.concurrent.TrieMap

/**
 * THE FAMILY DEPENDENCY INDEX (docs/design/identity-no-drift.md, "One family index"): for every family of a corpus resolve, everything
 * its decisions read — its members, each by the digest of its whole input; the corpus-learned facts it read through
 * [[CorpusContext]], each by the digest of its VALUE (a banner's house, a piece's spread, a record with its venue titles,
 * a title group, a reached set, a search's ranking); every lookup it made ([[Dependency]], digested as the receipts
 * digest them, an unanswered one too); the pins on its listings; the keys it blocks under — and what it decided: each
 * member's take and the rules that fired (the [[DecisionTrace]] ids, `rulesOf` and the stage's, the ids the rule
 * registry is keyed by).
 *
 * Recorded by resolving each family ALONE against the corpus's context (A3) through recording wrappers of the context and
 * the lookups: what is recorded is what was READ, not a superset of keys. A measure, not a rule: nothing here moves a
 * take; the resolve it records is the model's own.
 *
 * The property it exists for ([[FamilyIndex.Entry.stale]], FamilyIndexCompletenessSpec over real slices): a family whose
 * every recorded read digests as recorded against ANOTHER corpus — and none of whose block keys a listing outside it
 * carries there — takes exactly what it took. That is what lets (a) a per-family check re-walk only families whose index
 * moved and (b) a recompute re-resolve only the families a change reaches.
 */
object FamilyIndex {

  /** One read of the corpus's learned context, by key: what [[CorpusContext]] answered it. */
  enum ContextRead {
    case Record(film: Int)
    case House(banner: String)
    /** Every banner's house at once: a reader that walked the whole map. */
    case Houses
    case HouseRanking
    case TitleGroup(key: String)
    case WholeTitle(sanitised: String)
    case SegmentSpread(sanitised: String)
    case ReachedBy(key: String)
    case Ranked(query: CandidateQuery)

    /** What `context` answers for this read: the one function the recorder and a re-read both digest. */
    private[identity] def valueIn(context: CorpusContext): Any = this match {
      case Record(film)             => context.candidate(film)
      case House(banner)            => context.houses.of.get(banner)
      case Houses                   => context.houses.of
      case HouseRanking             => context.houseRanking
      case TitleGroup(key)          => context.titleGroup(key)
      case WholeTitle(sanitised)    => context.wholeTitle(sanitised)
      case SegmentSpread(sanitised) => context.segmentSpread(sanitised)
      case ReachedBy(key)           => context.reachedBy(key)
      case Ranked(query)            => context.ranked(query)
    }
    private[identity] def digestIn(context: CorpusContext): Long = ContentHash.of(valueIn(context))
    /** A readable id, as a report or a stored index names it. */
    def id: String = this match {
      case Record(film)             => s"record|$film"
      case House(banner)            => s"house|$banner"
      case Houses                   => "houses"
      case HouseRanking             => "house-ranking"
      case TitleGroup(key)          => s"group|$key"
      case WholeTitle(sanitised)    => s"whole|$sanitised"
      case SegmentSpread(sanitised) => s"spread|$sanitised"
      case ReachedBy(key)           => s"reached|$key"
      case Ranked(query)            => s"ranked|${Dependency.queryId(query)}"
    }
  }

  /** A context answering each [[ContextRead]] through one function, [[read]]: the recorder and the replay below differ
   *  only in where a read's value comes from, never in which read a method makes. */
  private[identity] abstract class ReadingContext extends CorpusContext {
    protected def read(what: ContextRead): Any
    def candidate(id: Int): Option[Candidate] = read(ContextRead.Record(id)).asInstanceOf[Option[Candidate]]
    def houses: StageRelays.Houses = StageRelays.Houses(HousesView)
    def houseRanking: Map[String, Seq[StageRelays.Houses.Contender]] =
      read(ContextRead.HouseRanking).asInstanceOf[Map[String, Seq[StageRelays.Houses.Contender]]]
    def titleGroup(key: String): Seq[(String, IdentityMeasures.Listing)] =
      read(ContextRead.TitleGroup(key)).asInstanceOf[Seq[(String, IdentityMeasures.Listing)]]
    def wholeTitle(sanitised: String): Boolean = read(ContextRead.WholeTitle(sanitised)).asInstanceOf[Boolean]
    def segmentSpread(sanitised: String): Int = read(ContextRead.SegmentSpread(sanitised)).asInstanceOf[Int]
    def reachedBy(key: String): Seq[(String, Set[Int])] = read(ContextRead.ReachedBy(key)).asInstanceOf[Seq[(String, Set[Int])]]
    def ranked(query: CandidateQuery): Option[Seq[Int]] = read(ContextRead.Ranked(query)).asInstanceOf[Option[Seq[Int]]]

    /** The houses' map, each banner read on its own; walked whole, read as [[ContextRead.Houses]]. */
    private object HousesView extends scala.collection.immutable.AbstractMap[String, String] {
      private def whole = read(ContextRead.Houses).asInstanceOf[Map[String, String]]
      def get(banner: String): Option[String] = read(ContextRead.House(banner)).asInstanceOf[Option[String]]
      def iterator: Iterator[(String, String)] = whole.iterator
      def removed(key: String): Map[String, String] = whole.removed(key)
      def updated[V1 >: String](key: String, value: V1): Map[String, V1] = whole.updated(key, value)
    }
  }

  object ContextRead {
    /** The read an [[ContextRead.id]] names; `None` for one it does not. */
    def parse(id: String): Option[ContextRead] = id.split("\\|", 2) match {
      case Array("houses")                => Some(Houses)
      case Array("house-ranking")         => Some(HouseRanking)
      case Array("record", film)          => film.toIntOption.map(Record(_))
      case Array("house", banner)         => Some(House(banner))
      case Array("group", key)            => Some(TitleGroup(key))
      case Array("whole", sanitised)      => Some(WholeTitle(sanitised))
      case Array("spread", sanitised)     => Some(SegmentSpread(sanitised))
      case Array("reached", key)          => Some(ReachedBy(key))
      case Array("ranked", query)         => Dependency.queryOf(query).map(Ranked(_))
      case _                              => None
    }
  }

  /** A corpus's context recording every read a resolve makes of it: the value it answered, and its digest. */
  private[identity] final class RecordingContext(underlying: CorpusContext) extends ReadingContext {
    val values: TrieMap[ContextRead, Any] = TrieMap.empty
    protected def read(what: ContextRead): Any = { val value = what.valueIn(underlying); values.update(what, value); value }
    def reads: Map[ContextRead, Long] = values.toMap.map { case (what, value) => what -> ContentHash.of(value) }
    // the listing's own evidence, cached: derived from the listing alone, which the entry's member digest holds
    override def evidenceOf(listing: Listing): Option[Evidence] = underlying.evidenceOf(listing)
  }

  /** A read the index does not hold: what a replay from the index alone met ([[replayAlone]]). */
  final case class Unindexed(read: String) extends RuntimeException(s"a read the family index does not hold: $read")

  /** The context a family's recorded reads answer, and nothing else ([[Unindexed]]). */
  private final class ReplayContext(values: Map[ContextRead, Any]) extends ReadingContext {
    protected def read(what: ContextRead): Any = values.getOrElse(what, throw Unindexed(what.id))
  }

  /** Lookups answering each read through one function, [[read]], as [[ReadingContext]] does the context's. */
  private[identity] abstract class ReadingLookups extends IdentityLookups {
    protected def read[A](dependency: Dependency, ask: => Answer[A]): Answer[A]
    def detail(listing: Listing): Answer[Option[DetailFacts]] = read(Lookups.detail(listing.key), detailOf(listing))
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = read(Lookups.search(query), searchOf(query))
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = read(Lookups.record(tmdbId, Dependency.Aspect.Answer), recordOf(tmdbId))
    override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = read(Lookups.record(tmdbId, Dependency.Aspect.Release), releaseOf(tmdbId))
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = read(Lookups.record(tmdbId, Dependency.Aspect.Cast), castOf(tmdbId))
    override def overview(tmdbId: Int): Answer[Option[ProseWords]] = read(Lookups.record(tmdbId, Dependency.Aspect.Overview), overviewOf(tmdbId))
    protected def detailOf(listing: Listing): Answer[Option[DetailFacts]]
    protected def searchOf(query: CandidateQuery): Answer[Seq[Hit]]
    protected def recordOf(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]
    protected def releaseOf(tmdbId: Int): Answer[Option[java.time.LocalDate]]
    protected def castOf(tmdbId: Int): Answer[Option[Seq[String]]]
    protected def overviewOf(tmdbId: Int): Answer[Option[ProseWords]]
  }

  /** Lookups recording every read as its [[Dependency]]: the answer, and its digest as a receipt digests it
   *  ([[Dependency.digest]]); and which listings' venues publish a detail page. */
  private[identity] final class RecordingLookups(underlying: IdentityLookups) extends ReadingLookups {
    val answers: TrieMap[Dependency, Answer[?]] = TrieMap.empty
    val detailed: TrieMap[ListingKey, Boolean] = TrieMap.empty
    protected def read[A](dependency: Dependency, ask: => Answer[A]): Answer[A] = { val answer = ask; answers.update(dependency, answer); answer }
    def reads: Map[Dependency, Long] = answers.toMap.map { case (dependency, answer) => dependency -> Dependency.digest(answer) }
    def hasDetail(listing: Listing): Boolean = { val has = underlying.hasDetail(listing); detailed.update(listing.key, has); has }
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit =
      underlying.prefetch(queries, films, details)
    override def prefetchAnswered(): Unit = underlying.prefetchAnswered()
    override def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[ListingKey]): Unit =
      underlying.released(queries, films, details)
    override def numbersFilmsByTmdbId: Boolean = underlying.numbersFilmsByTmdbId
    protected def detailOf(listing: Listing): Answer[Option[DetailFacts]] = underlying.detail(listing)
    protected def searchOf(query: CandidateQuery): Answer[Seq[Hit]] = underlying.candidates(query)
    protected def recordOf(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = underlying.film(tmdbId)
    protected def releaseOf(tmdbId: Int): Answer[Option[java.time.LocalDate]] = underlying.releaseDay(tmdbId)
    protected def castOf(tmdbId: Int): Answer[Option[Seq[String]]] = underlying.cast(tmdbId)
    protected def overviewOf(tmdbId: Int): Answer[Option[ProseWords]] = underlying.overview(tmdbId)
  }

  /** The lookups a family's recorded answers answer, and nothing else ([[Unindexed]]). */
  private final class ReplayLookups(recorded: Recorded) extends ReadingLookups {
    protected def read[A](dependency: Dependency, ask: => Answer[A]): Answer[A] =
      recorded.answers.getOrElse(dependency, throw Unindexed(Dependency.encode(dependency))).asInstanceOf[Answer[A]]
    def hasDetail(listing: Listing): Boolean = recorded.detailed.getOrElse(listing.key, throw Unindexed(s"has-detail|${listing.key}"))
    private def never = throw new IllegalStateException("a replay reads its recorded answers only")
    protected def detailOf(listing: Listing): Answer[Option[DetailFacts]] = never
    protected def searchOf(query: CandidateQuery): Answer[Seq[Hit]] = never
    protected def recordOf(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = never
    protected def releaseOf(tmdbId: Int): Answer[Option[java.time.LocalDate]] = never
    protected def castOf(tmdbId: Int): Answer[Option[Seq[String]]] = never
    protected def overviewOf(tmdbId: Int): Answer[Option[ProseWords]] = never
  }

  /** What a family read, as VALUES: what [[replayAlone]] answers it from. Kept by [[build]] only when asked (`values`):
   *  the index itself holds digests. */
  final case class Recorded(members: Seq[Listing], context: Map[ContextRead, Any], answers: Map[Dependency, Answer[?]],
                            detailed: Map[ListingKey, Boolean])

  /** The family `recorded` holds resolved ALONE from its recorded reads — no corpus, no lookups beyond them: each member's
   *  take. Throws [[Unindexed]] on a read the index does not hold. */
  def replayAlone(recorded: Recorded, normalizer: TitleNormalizer, inputs: ResolveInputs): Map[ListingKey, Option[String]] = {
    val regions = IdentityResolver.resolveRegion(recorded.members, new ReplayLookups(recorded), normalizer, inputs.calibration, inputs.pins,
      inputs.decorations, new ReplayContext(recorded.context))
    val decided = regions.flatMap(_.decisions).flatMap(decision => decision.members.map(_ -> decision)).toMap
    recorded.members.map(l => l.key -> decided.get(l.key).flatMap(PopulationMonotonicity.takeOf)).toMap
  }

  /** The lookup [[Dependency]] kinds, and a re-read of one against other lookups: the digest a read gives now. */
  object Lookups {
    def detail(key: ListingKey): Dependency = Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Answer, Question.VenuePage(key))
    def search(query: CandidateQuery): Dependency = Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.Candidate(query))
    def record(film: Int, aspect: Dependency.Aspect): Dependency = Dependency.Lookup(Dependency.Via.Tmdb, aspect, Question.FilmRecord(film))

    /** `dependency` read again through `lookups` (`listingOf` naming a venue page's listing); `None`: not a lookup's, or a
     *  page whose listing is gone — read as moved. */
    def digestIn(dependency: Dependency, lookups: IdentityLookups, listingOf: ListingKey => Option[Listing]): Option[Long] = dependency match {
      case Dependency.Lookup(_, _, Question.VenuePage(key)) => listingOf(key).map(listing => Dependency.digest(lookups.detail(listing)))
      case Dependency.Lookup(_, _, Question.Candidate(query)) => Some(Dependency.digest(lookups.candidates(query)))
      case Dependency.Lookup(_, aspect, Question.FilmRecord(film)) => Some(Dependency.digest(aspect match {
        case Dependency.Aspect.Answer   => lookups.film(film)
        case Dependency.Aspect.Release  => lookups.releaseDay(film)
        case Dependency.Aspect.Cast     => lookups.cast(film)
        case Dependency.Aspect.Overview => lookups.overview(film)
      }))
      case _ => None
    }
  }

  /** What a corpus answers a re-read with: its context and lookups, its listings, its pins, and the block keys its
   *  listings OUTSIDE a family carry ([[outsideBlockKeys]], what a merge with another family would share). */
  private[identity] final case class Now(context: CorpusContext, lookups: IdentityLookups, listingOf: ListingKey => Option[Listing], pins: PinConstraints,
                       blockKeyHolders: String => Set[ListingKey]) {
    def outsideBlockKeys(entry: Entry): Set[String] = entry.blockKeys.filter(key => (blockKeyHolders(key) -- entry.members.keySet).nonEmpty)
  }

  /** One family's entry. `members`: each listing's input digest as the model reads it ([[modelDigest]]); `blockKeys`: the title and
   *  film keys it merges under; `context`/`answers`: each read with the digest of what it read; `pins`: the ids of the
   *  pins naming a member; `takes`/`rules`: each member's take ([[PopulationMonotonicity.takeOf]], `None` pending) and
   *  the rule ids that decided it. */
  final case class Entry(members: Map[ListingKey, Long], blockKeys: Set[String], context: Map[ContextRead, Long],
                         answers: Map[Dependency, Long], pins: Set[String], takes: Map[ListingKey, Option[String]],
                         rules: Map[ListingKey, Seq[String]]) {
    /** The content digest of everything it read — equal digests under one rules version, equal decisions. */
    lazy val digest: Long = ContentHash.of((members, context.map { case (read, d) => read.id -> d },
      answers.map { case (dependency, d) => Dependency.encode(dependency) -> d }, pins, blockKeys))
    def name: String = members.keys.minBy(_.toString).toString

    /** Why this entry no longer stands against `now`, each moved input named; empty: every read digests as recorded,
     *  no member moved, and no listing outside it shares a block key — it decides as it decided. */
    private[identity] def stale(now: Now): Seq[String] = {
      val membersMoved = members.toSeq.collect { case (key, d) if !now.listingOf(key).map(modelDigest).contains(d) => s"member|$key" }
      val contextMoved = context.toSeq.collect { case (read, d) if read.digestIn(now.context) != d => read.id }
      val answersMoved = answers.toSeq.collect { case (dependency, d) if !Lookups.digestIn(dependency, now.lookups, now.listingOf).contains(d) =>
        Dependency.encode(dependency) }
      val pinsMoved    = Option.when(pinsOf(now.pins, members.keySet) != pins)("pins").toSeq
      membersMoved ++ contextMoved ++ answersMoved ++ pinsMoved ++ now.outsideBlockKeys(this).toSeq.map("block|" + _)
    }
  }

  /** A listing's input as the MODEL reads it: every field inside its equality ([[Listing.equals]]: the screening days by
   *  the season they place a relay in), none of the stage's alone (its poster, days, names, words) -- a venue adding a
   *  showtime moves no family's model entry. The final take's reads digest the whole listing. */
  def modelDigest(listing: Listing): Long =
    ContentHash.of((listing.copy(poster = None, screenings = ScreeningDays.None, names = VenueNames.None, words = ProseWords.None,
      pageFacts = false), listing.screenedSeason))

  private def pinsOf(pins: PinConstraints, members: Set[ListingKey]): Set[String] = pins.pins.filter(_.listings.exists(members)).map(_.id).toSet

  /** An index's build: its entries, the corpus resolve they were read off, and the listings whose take resolved alone
   *  differed from the corpus resolve's (A3: none expected). */
  final case class Built(index: FamilyIndex, whole: Resolution, apart: Seq[ListingKey], recorded: Seq[Recorded])

  /** Every family of `listings`' resolve, each resolved alone against the corpus's context and recorded; `values`: each
   *  family's reads kept as values too ([[Recorded]], in entry order), for [[replayAlone]]. */
  def build(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs, threads: Int = 1,
            values: Boolean = false): Built = {
    val all        = listings.sorted.distinctBy(_.key)
    val whole      = IdentityResolver.resolve(all, lookups, normalizer, inputs)
    val generation = new CandidateGeneration(all, lookups, normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val families   = all.groupBy(listing => whole.familyOf.getOrElse(listing.key, -1)).toSeq.sortBy(_._1).map(_._2)
    def entryOf(members: Seq[Listing]): (Entry, Seq[ListingKey], Option[Recorded]) = {
      val context  = new RecordingContext(generation.context)
      val recorded = new RecordingLookups(lookups)
      val regions  = IdentityResolver.resolveRegion(members, recorded, normalizer, inputs.calibration, inputs.pins, inputs.decorations, context)
      val decided  = regions.flatMap(_.decisions).flatMap(decision => decision.members.map(_ -> decision)).toMap
      val keys     = members.map(_.key)
      val takes    = keys.map(key => key -> decided.get(key).flatMap(PopulationMonotonicity.takeOf)).toMap
      val rules    = keys.map(key => key -> decided.get(key).toSeq.flatMap(d => d.trace.rulesOf(key) ++ d.trace.stage)).toMap
      val apart    = keys.filter(key => whole.decisionOf.get(key).flatMap(PopulationMonotonicity.takeOf) != takes(key))
      (Entry(members.map(l => l.key -> modelDigest(l)).toMap, regions.flatMap(_.blockKeys).toSet, context.reads,
        recorded.reads, pinsOf(inputs.pins, keys.toSet), takes, rules), apart,
        Option.when(values)(Recorded(members, context.values.toMap, recorded.answers.toMap, recorded.detailed.toMap)))
    }
    val built =
      if (threads <= 1 || families.sizeIs < 2) families.map(entryOf)
      else {
        val pool = tools.DaemonExecutors.boundedEC("family-index", threads)
        try families.map(members => pool.submit(() => entryOf(members))).map(_.get)
        finally { val _ = pool.shutdownNow() }
      }
    Built(new FamilyIndex(built.map(_._1)), whole, built.flatMap(_._2), built.flatMap(_._3))
  }

  /** What `listings` answer a re-read with ([[Now]]): their corpus's context, `lookups`, and each block key's holders by
   *  the corpus resolve's families (`whole`, [[blockKeyHolders]]). */
  private[identity] def now(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs, whole: Built): Now = {
    val all        = listings.sorted.distinctBy(_.key)
    val generation = new CandidateGeneration(all, lookups, normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val byKey      = all.map(l => l.key -> l).toMap
    Now(generation.context, lookups, byKey.get, inputs.pins, blockKeyHolders(whole.index))
  }

  /** Each block key's listings, by the families of `index` carrying it. */
  def blockKeyHolders(index: FamilyIndex): String => Set[ListingKey] = {
    val holders = index.entries.flatMap(entry => entry.blockKeys.map(_ -> entry.members.keySet)).groupMapReduce(_._1)(_._2)(_ ++ _)
    key => holders.getOrElse(key, Set.empty)
  }
}

/** The index's query side: which family holds a listing, which families read a context fact, a lookup or a block key,
 *  in which a rule fired, and which no longer stand against another corpus. */
final class FamilyIndex(val entries: Seq[FamilyIndex.Entry]) {
  import FamilyIndex.{ContextRead, Entry, Now}

  private lazy val byListing: Map[ListingKey, Entry] = entries.flatMap(entry => entry.members.keys.map(_ -> entry)).toMap
  private lazy val byContext: Map[ContextRead, Seq[Entry]] = entries.flatMap(entry => entry.context.keys.map(_ -> entry)).groupMap(_._1)(_._2)
  private lazy val byAnswer: Map[Dependency, Seq[Entry]] = entries.flatMap(entry => entry.answers.keys.map(_ -> entry)).groupMap(_._1)(_._2)
  private lazy val byBlockKey: Map[String, Seq[Entry]] = entries.flatMap(entry => entry.blockKeys.map(_ -> entry)).groupMap(_._1)(_._2)
  private lazy val byRule: Map[String, Seq[Entry]] =
    entries.flatMap(entry => entry.rules.values.flatten.toSeq.distinct.map(_ -> entry)).groupMap(_._1)(_._2)

  def familyOf(key: ListingKey): Option[Entry] = byListing.get(key)
  def readersOf(read: ContextRead): Seq[Entry] = byContext.getOrElse(read, Nil)
  def readersOf(dependency: Dependency): Seq[Entry] = byAnswer.getOrElse(dependency, Nil)
  def holdersOf(blockKey: String): Seq[Entry] = byBlockKey.getOrElse(blockKey, Nil)
  /** The families a rule id ([[DecisionTrace.rulesOf]], a stage's) decided a member of. */
  def firing(ruleId: String): Seq[Entry] = byRule.getOrElse(ruleId, Nil)
  def rules: Set[String] = byRule.keySet
  /** Each entry that no longer stands against `now`, with why ([[Entry.stale]]). */
  private[identity] def stale(now: Now): Seq[(Entry, Seq[String])] = entries.map(entry => entry -> entry.stale(now)).filter(_._2.nonEmpty)
}
