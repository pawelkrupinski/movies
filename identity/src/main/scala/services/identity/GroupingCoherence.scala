package services.identity

import services.identity.IdentityMeasures.{Category, Number}
import services.movies.{ListingKey, RelayBanner}

/**
 * GROUPING COHERENCE: does a cluster hold listings of ONE film? A reader of the takes after the fact, over the whole
 * corpus, never a decision: it changes no take and no grouping. [[SuspectTakes]] asks whether a take fits its listing; this
 * asks whether a cluster's members fit EACH OTHER and the film their cluster took — the wrong groupings (a cluster
 * merging listings of different films), which no member's own take shows on its own: UK Chichester's bare "Prima Facie"
 * (the 2026 film, its own evidence took it) grouped with Power Station's "NT Live: Prima Facie" under the National
 * Theatre's 2022 recording (drift uk seed 17, docs/investigations/2026-11-01-relay-apart-from-film.md).
 *
 * Two signals, each in words a judge checks against the venues' pages without reading code:
 *  - [[Signal.Facts]]: the members' OWN stated facts conflict — years more than a year apart, disjoint credited directors,
 *    running times far apart — read through the listing-listing measures ([[IdentityMeasures.listingListing]]:
 *    `year.delta`, `director`, `runtime.delta`), with a dated re-release, an anniversary, a title-year marker, a relay
 *    and an event's slot no conflict;
 *  - [[Signal.Alone]]: a member's own evidence alone, scored as the resolver scores a lone node ([[FamilyScope.takenAlone]],
 *    kept in its trace, [[DecisionTrace.Alone]]), confidently takes another film than the cluster's, or denies the
 *    cluster's film ([[DecisionTrace.Denied]]).
 *
 * Pure: a decision, its members as their venues state them, the film record it took, the year it screens in.
 */
object GroupingCoherence {

  enum Signal(val label: String) {
    case Facts extends Signal("facts")
    case Alone extends Signal("alone")
  }

  /** What one finding is about, and the signal it is of. */
  enum Kind(val label: String, val signal: Signal) {
    case Year     extends Kind("year", Signal.Facts)
    case Director extends Kind("director", Signal.Facts)
    case Runtime  extends Kind("runtime", Signal.Facts)
    case Prefers  extends Kind("prefers", Signal.Alone)
    case Denies   extends Kind("denies", Signal.Alone)
  }

  /** One reason the cluster is suspect: the members it is about, why, and how much it weighs ([[Coherence.score]]). */
  final case class Finding(kind: Kind, members: Seq[ListingKey], why: String, weight: Double) {
    def render: String = s"${kind.label}: $why"
  }

  /** One cluster's coherence: the film it took (`tmdb:<id>`, a fallback's `imdb:<tt>`), its record's title, its members
   *  and what argues they are not one film. */
  final case class Coherence(decision: ResolverDecision, film: String, title: String, members: Seq[Listing], findings: Seq[Finding]) {
    /** The heaviest finding of each kind, summed: two years apart and a disjoint director outweigh one of them; ten
     *  members repeating one conflict weigh no more than one. */
    lazy val score: Double = findings.groupBy(_.kind).values.map(_.map(_.weight).max).sum
    def signals: Seq[Signal] = findings.map(_.kind.signal).distinct
    /** The members a finding names. */
    def involved: Seq[ListingKey] = findings.flatMap(_.members).distinct
    /** Is it suspect enough to rank ([[Flagged]])? */
    def flagged: Boolean = score >= Flagged
  }

  // ── the thresholds, calibrated in GroupingCoherenceCalibrationSpec ──────────────────────────────────────────────

  /** The least score a cluster is flagged at: one strong finding (a disjoint director, a confident own take of another
   *  film, a denial by the member's own evidence), or two weaker ones together. */
  val Flagged = 1.0
  /** How many years two stated years may stand apart and still date one film: a release year beside its premiere's. */
  val YearSlack = 1
  /** Years apart from which a year conflict is a strong one: no premiere, festival or print dates a film this far off. */
  val FarYears = 5
  /** How far a running time may sit from a record's and still be that cut ("rounding and trailers"). */
  val RuntimeFit = 10
  /** The confidence a member's own rules must take another film at for it to count. */
  val AloneConfident = 0.6
  /** How much more a member's own take of another film must weigh than its own score of the cluster's film. Its finding
   *  weighs 0.4 + 0.7 x its confidence, at most 1: flagged alone from 86%. */
  val AloneMargin = 0.2

  /** The coherence of the cluster `decision` took `film` (`record`) for, its members `members` as their venues state them,
   *  screening in `screeningYear`; `country`: the venues' ISO-3166-1 code, whose release dates a stated year reads. */
  def of(decision: ResolverDecision, film: String, record: IdentityMeasures.Film, members: Seq[Listing], screeningYear: Int,
         country: Option[String] = None): Coherence = {
    val stated = members.map(l => l -> Evidence.of(l, None, TitleDecorations.resolver).measured)
    Coherence(decision, film, record.title, members, facts(stated, record, screeningYear, country) ++ alone(decision, members))
  }

  /** Every cluster among `takes` ([[SuspectTakes.takes]]: one per member listing, as its venue states it) with what argues
   *  it is no one film, the most suspect first; clusters nothing argues against are left out. */
  def corpus(takes: Seq[SuspectTakes.Take], screeningYear: Int, country: Option[String] = None): Seq[Coherence] =
    takes.groupBy(_.decision.members).valuesIterator.map { members =>
      val head = members.head
      of(head.decision, head.film, head.record, members.map(_.listing).sortBy(_.key), screeningYear, country)
    }.filter(_.findings.nonEmpty).toSeq.sortBy(c => (-c.score, -c.involved.size, -c.members.size, c.film))

  // ── (1) the members' own facts ────────────────────────────────────────────────────────────────────────────────

  /** A member as its facts compare: a chain billing one title alike at a hundred venues is one statement. */
  private final case class Statement(listing: Listing, measured: IdentityMeasures.Listing, keys: Seq[ListingKey])

  private def statements(members: Seq[(Listing, IdentityMeasures.Listing)]): Seq[Statement] =
    members.groupBy { case (l, m) => (m.statedYear, l.directors.map(_.trim.toLowerCase).sorted, m.statedRuntime, l.rawTitle) }
      .valuesIterator.map(same => Statement(same.head._1, same.head._2, same.map(_._1.key))).toSeq.sortBy(_.listing.key)

  private def named(s: Statement): String = s"${s.listing.venue} '${s.listing.rawTitle}'${if (s.keys.sizeIs > 1) s" (+${s.keys.size - 1})" else ""}"

  /** Does the listing bill an event-cinema relay or a stage work — whose credited director is the staging's, whose
   *  running time is the broadcast's? */
  private def relay(s: Statement): Boolean =
    RelayBanner.billed(Seq(s.listing.rawTitle, s.listing.title)) || ListingShape.stagesAWork(s.listing)

  private[identity] def facts(members: Seq[(Listing, IdentityMeasures.Listing)], record: IdentityMeasures.Film, screeningYear: Int,
                              country: Option[String]): Seq[Finding] = {
    val said = statements(members)
    if (said.sizeIs < 2) Nil
    else {
      val pairs = said.combinations(2).map { case Seq(a, b) =>
        (a, b, IdentityMeasures.listingListing(a.measured.copy(year = a.measured.statedYear), b.measured.copy(year = b.measured.statedYear),
          sameVenue = a.listing.venue == b.listing.venue, sharedChainId = None)) }.toSeq
      // the strongest pair of each kind: one finding per kind, naming its two statements
      Seq(pairs.flatMap(year(_, record, screeningYear, country)), pairs.flatMap(director(_, record)), pairs.flatMap(runtime(_, record)))
        .flatMap(_.maxByOption(_.weight))
    }
  }

  private type Pair = (Statement, Statement, Map[String, IdentityMeasures.Measure])

  /** Years more than [[YearSlack]] apart, neither explained by the film: a stated year a dated release of the record
   *  carries (a re-release, an edition: "Apocalypse Now" billed 2019), the screening year (or the one before) beside the
   *  record's own year -- the venue dating its local release or re-release ("Everytime" billed 2026, its 2024 record) --
   *  a title-year marker agreeing where the feed's year does not ("Film Club: East Of Eden (1955)" fed 1995), or an
   *  edition's billing ("(Extended Edition)" 2011). Two relays
   *  state their productions' and seasons' years, no film's. An anniversary's year is no statement of the film's
   *  ([[IdentityMeasures.listingListing]] leaves it out). Weighs 1.0 at [[FarYears]] apart unless one year is the
   *  screening's (a re-release record's original beside it: "Doctor Who: The Satan Pit" 2006 and 2026), else 0.6. */
  private def year(pair: Pair, record: IdentityMeasures.Film, screeningYear: Int, country: Option[String]): Option[Finding] = {
    val (a, b, m) = pair
    m.get("year.delta").collect { case Number(d) if d > YearSlack => d.toInt }.filterNot(_ => relay(a) && relay(b)).flatMap { gap =>
      val (ya, yb) = (a.measured.statedYear.get, b.measured.statedYear.get)
      def releasedIn(y: Int) = record.closestYear(y, country).exists(r => math.abs(r - y) <= YearSlack)
      def current(y: Int) = y >= screeningYear - 1
      def released(now: Int, other: Int) = current(now) && record.year.exists(r => math.abs(r - other) <= YearSlack)
      def years(s: Statement) = (s.measured.year.toSeq ++ s.measured.titleYear).distinct
      val marked = years(a).exists(x => years(b).exists(y => math.abs(x - y) <= YearSlack))
      // an edition's billing dates the edition ("The Return of the King (Extended Edition)" 2011)
      def edition(s: Statement) = services.billing.DecorationWords.billsAnEdition(s.measured.ownTitles, record.titles)
      Option.unless((releasedIn(ya) && releasedIn(yb)) || released(ya, yb) || released(yb, ya) || marked || edition(a) || edition(b))(
        Finding(Kind.Year, a.keys ++ b.keys, s"${named(a)} states $ya, ${named(b)} $yb",
          if (gap >= FarYears && !current(ya) && !current(yb)) 1.0 else 0.6))
    }
  }

  /** Credited directors with no person in common, anchored on the record: one member credits the film's director and
   *  the other someone else entirely (0.6) -- or, the record crediting nobody either names, both apart (0.3). Never
   *  enough alone: a venue's credit field names producers and writers too (UK The Ritz Workington's "Forgotten Island"
   *  credits its three producers), so a director conflict is flagged beside another finding. A credit in
   *  another script, a name shared in part (a typo, a transliteration: "Favn Veerasunthorn"), two of the record's own
   *  credits (a co-direction each venue credits half of) and a relay's or stage work's credit are no conflict. */
  private def director(pair: Pair, record: IdentityMeasures.Film): Option[Finding] = {
    val (a, b, m) = pair
    val credited = record.directors.filter(_.exists(_.trim.nonEmpty))
    def toFilm(s: Statement) = credited.map(d => IdentityMeasures.directorRelation(s.listing.directors, d))
    def films(s: Statement) = toFilm(s).contains(IdentityMeasures.SamePersonMeasure)
    def apart(s: Statement) = toFilm(s).forall(_ == Category("different"))
    m.get("director").collect { case Category("different") => () }.filterNot(_ => relay(a) || relay(b)).flatMap { _ =>
      val weight = if ((films(a) && apart(b)) || (films(b) && apart(a))) Some(0.6) else Option.when(apart(a) && apart(b))(0.3)
      weight.map(w => Finding(Kind.Director, a.keys ++ b.keys,
        s"${named(a)} credits ${a.listing.directors.mkString(", ")}, ${named(b)} ${b.listing.directors.mkString(", ")}" +
          credited.fold("")(d => s"; the film ${d.mkString(", ")}"), w))
    }
  }

  /** A member running far SHORTER than every cut of the record (at least [[IdentityMeasures.RuntimeContradiction]]
   *  minutes and a quarter of the record's) beside one running a cut of it -- or, the record stating none, two members at
   *  least that far apart. A longer listing is the venue's slot (the film with its wine, its Q&A, its adverts: "Psycho
   *  with wine", 170 minutes); a short's programme, a relay's broadcast and an event's billing are no conflict either. */
  private def runtime(pair: Pair, record: IdentityMeasures.Film): Option[Finding] = {
    val (a, b, m) = pair
    m.get("runtime.delta").collect { case Number(d) => d.toInt }.flatMap { _ =>
      val Seq((short, lo), (long, hi)) = Seq(a -> a.measured.statedRuntime.get, b -> b.measured.statedRuntime.get).sortBy(_._2)
      def event(s: Statement) = relay(s) || FilmNess.eventWords(s.measured).nonEmpty
      def far(x: Int, y: Int) = y - x >= math.max(IdentityMeasures.RuntimeContradiction.toDouble, y * 0.25)
      val runs = record.runtimes
      val apart =
        if (runs.isEmpty) far(lo, hi)
        else runs.forall(r => far(lo, r)) && runs.exists(r => math.abs(hi - r) <= RuntimeFit)
      Option.when(lo > ShortFilms.MaxMinutes && apart && !event(a) && !event(b))(
        Finding(Kind.Runtime, short.keys ++ long.keys, s"${named(short)} runs $lo minutes, ${named(long)} $hi" +
          (if (runs.isEmpty) "" else s"; the film ${runs.sorted.mkString("/")}"), 0.5))
    }
  }

  // ── (2) a member on its own ───────────────────────────────────────────────────────────────────────────────────

  /** Members whose own rules, alone, confidently take another film than the cluster's (at [[AloneConfident]] or more,
   *  [[AloneMargin]] over their own score of the cluster's film), or whose own evidence denies the cluster's film — read
   *  off the decision's trace ([[DecisionTrace.Node]]). Never of a pinned cluster: a curator's pin is the judgement. */
  private[identity] def alone(decision: ResolverDecision, members: Seq[Listing]): Seq[Finding] =
    if (decision.basis == ResolverDecision.Basis.Pinned) Nil
    else decision.film.toSeq.flatMap { film =>
      val byKey = members.map(l => l.key -> l).toMap
      // a node's listings share its trace: one finding per node
      val nodes = decision.trace.nodes.toSeq.filter { case (key, _) => byKey.contains(key) }.groupMap(_._2)(_._1).toSeq.sortBy(_._2.min)
      def who(keys: Seq[ListingKey]) = { val l = byKey(keys.min); s"${l.venue} '${l.rawTitle}'${if (keys.sizeIs > 1) s" (+${keys.size - 1})" else ""}" }
      def pct(p: Double) = ResolverDecision.percent(p)
      val prefers = nodes.flatMap { case (node, keys) =>
        node.alone.filter(a => a.film != film && a.confidence >= AloneConfident).flatMap { a =>
          val own = node.score.filter(_ => node.candidate.contains(film))
          Option.when(own.forall(_ <= a.confidence - AloneMargin))(Finding(Kind.Prefers, keys.sorted,
            s"${who(keys)} alone takes tmdb:${a.film} at ${pct(a.confidence)} by ${a.rule}; the cluster's tmdb:$film " +
              own.fold("is not in its own pool")(p => s"it scores ${pct(p)}"), math.min(1.0, 0.4 + 0.7 * a.confidence)))
        }
      }
      val denies = nodes.flatMap { case (node, keys) =>
        node.denied.filter(_ => node.candidate.contains(film)).map(d => Finding(Kind.Denies, keys.sorted,
          s"${who(keys)}'s own evidence denies tmdb:$film: ${d.why}", if (d.cutOnly) 0.4 else 1.0))
      }
      prefers.maxByOption(_.weight).toSeq ++ denies.maxByOption(_.weight).toSeq ++
        // every other member named too, for the judge: the heaviest finding carries the weight
        (prefers.sortBy(-_.weight).drop(1) ++ denies.sortBy(-_.weight).drop(1)).map(_.copy(weight = 0.0))
    }
}
