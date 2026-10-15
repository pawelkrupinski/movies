package services.identity

import services.movies.{ListingKey, TitleNormalizer}

/**
 * `resolve(E)`: film identity as a pure, deterministic function of a SET of listings and the
 * answers of a lookup source (docs/design/identity-resolver.md, phase 2). Production code that
 * serves nothing: its only consumers are the shadow report, the recording sweep and curation.
 *
 * Two stages, every step a function of the set:
 *
 *   A. CANDIDATE GENERATION ([[CandidateGeneration]]). Each listing's own detail page merges into
 *      its [[Evidence]]; listings with identical evidence are one NODE. Every node's
 *      [[CandidateQueries]] are asked — all of them, up front, in sorted order, none conditional on
 *      another's answer (A1) — and every film any answer names is looked up once. The nodes are
 *      grouped into FAMILIES ([[Families]]), the block closure of `FamilyClosure` over their title
 *      keys and the films they match.
 *
 *   B. GLOBAL ASSIGNMENT. Every node scores every candidate it has an EVIDENCE PATH to (its own
 *      query named it, or a film title relates to its title) with the calibrated model
 *      ([[IdentityCalibration]] over [[IdentityMeasures]] — the same measurements the weights were
 *      fitted on, including `venues.corroborating`, the family's venue co-occurrence; [[FamilyScope]]).
 *      A candidate a node's evidence DENIES (`LearnedCannotLinks.learnedListingFilm`: a learned
 *      rule, or its OWN facts' probability below the certified cut, and only when the node
 *      publishes a fact the film can be compared on — a title relation alone is scored, never a
 *      veto; or a film of the node's credited director its title does not name, when its title
 *      names another of that director's) is not eligible. A node ACCEPTS a candidate ALONE by the
 *      rules of [[Acceptance]]; otherwise it follows its cluster. Then:
 *        1. constraint edges between nodes sharing a block key ([[ConstraintEdges]]), solved by
 *           [[ConstraintSolver]] (cannot wins, and no component holds two films however the
 *           must-links chain; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING ([[ClusterVoting]]): a cluster no member of which accepted a film
 *           scores its members' evidence POOLED into one listing (the heaviest title, the modal
 *           year, every director), and the winner, if accepted (`Acceptance.pooled`), becomes
 *           every member's film. A winner some members' own evidence denies is not a veto of the
 *           whole cluster: those members split off and the rest take it when their own pooled
 *           facts carry it. A winner no member's title names — only a credited director's
 *           filmography reached it — must also be the one film the pooled facts and the
 *           calibration both rank first;
 *        3. the constraints are re-solved with those films, and each final cluster is a
 *           [[ResolverDecision]] with its confidence, basis and explanation ([[ResolverDecisions]]).
 *
 * Every family is resolved on its own. That is sound only while no edge crosses a family, which
 * holds by construction — every edge joins two nodes sharing a block key — and is CHECKED: a
 * crossing edge fails the resolve rather than scoping it.
 *
 * `Mutation` is for the teeth tests only: each variant breaks exactly one of the properties the
 * specs prove, and the specs must catch it.
 */
object IdentityResolver {

  /** The members billing a programme UNDER ITS BANNER: a member whose every listing's words are all words of a member
   *  billing several films by a word that says so ([[MultiFilmBill]]: a block of shorts, a marathon), with one beside the
   *  film its cluster took — the programme's banner, not the film's title. It is that programme, as none of its films:
   *  DE "Pornfilmfestival: The more the merrier" is the block "Pornfilmfestival Warm Up: The more the merrier -
   *  shortfilms presented by Paulita Pappel", not Stevens' 1943 film. A film's own bare title beside a bill of it
   *  ("The Dark Knight" beside "The Dark Knight Trilogy") carries no banner, and stays the film. */
  private[identity] def billingUnderBanner(members: Seq[EvidenceNode], bills: Set[String], filmOf: String => Option[Int],
                                           scored: EvidenceNode => Seq[services.identity.Scored]): Set[String] = {
    def words(listing: Listing) = services.movies.TitleContainment.tokens(listing.rawTitle).toSet
    val programmes = members.filter(node => bills(node.id)).flatMap(_.listings)
      .filter(listing => MultiFilmBill.marker(Seq(listing.rawTitle, listing.title)).isDefined).map(words)
    if (programmes.isEmpty) Set.empty
    else members.filterNot(node => bills(node.id)).flatMap { node =>
      filmOf(node.id).flatMap(film => scored(node).find(_.candidate.tmdbId == film)).map(_.candidate.film).filter { film =>
        val titled = film.titles.flatMap(services.movies.TitleContainment.tokens).toSet
        node.listings.forall(listing => programmes.exists(programme => words(listing).subsetOf(programme)) && (words(listing) -- titled).nonEmpty)
      }.map(_ => node.id)
    }.toSet
  }

  /** Why the film a member's cluster took is none of that member's: its billing names the film as ONE PIECE beside another
   *  thing ([[ListingShape.billedBeside]]) — DE "Jens Wawrczeck - Immer Ärger mit Harry" is an actor's reading, not
   *  Hitchcock's 1955 film; UK "SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY" an amateur company's musical. Not where
   *  what is around the film is the venue's after all:
   *  - its cluster's listings — its own, or those its title joins it to — are backed by [[PieceCorroboration]] other venues
   *    or more stating the film's year or crediting its director (`venues.corroborating`, `corroborated`): a film widely
   *    screened, whose local title, subtitle or event a venue bills beside it (PL "Rozważna i romantyczna | DZIEŃ
   *    PRZEPIĘKNYCH KOBIET", joined to the plain title 75 venues bill);
   *  - the piece beside it RECURS beside two of the corpus's whole titles or more, whatever families they fell in
   *    ([[CorpusContext.repeatedSegment]], read by `recurs` on the title's shapes): a programme's banner or a venue's offer ("Family Cinema: Paddington - Cert
   *    PG", PL Kino Kosmos' "Nakarmić kruki | Złote gody" beside "Wszyscy ludzie prezydenta | Złote gody"); or is a one-word
   *    tag in capitals beside a title that is not ([[IdentityMeasures.capitalisedTags]]: "Camino dla opornych | CHKF") or
   *    one word of consonants alone, a programme's initials ("Kopnięci w czasie|Fks"); or
   *    dates an edition after the film ("Dismember the Alamo 2026 - Chicago"); or names its venue or city (PL Kino
   *    Amondo's "Punku | Peruwiańskie pożegnanie lata w Amondo");
   *  - the piece is another title of the film: its title search's first film, or one IMDb lists under it in some language
   *    ([[CandidateQuery.ImdbTitled]]: DE "Épines du Sahel - Die Dornen der Sahelzone");
   *  - the listing states a fact of the film's own: a year, a director or an original title ([[IdentityMeasures.ownAgreement]]),
   *    a running time within [[PieceRuntimeAgreement]] minutes, a feed's TMDB link ("Notting Hill - Lady's Night" [1999]);
   *  - the film is a current release, of its screenings' year or the one before ([[CurrentRelease]]), or a foreign film
   *    TMDB knows by its original title alone, billed first ("Ekmek parası - Geld für's Brot"): the venue's subtitle or
   *    translation beside it — the namesakes a reading or a show is taken for are older catalogue films.
   *  A piece naming a STAGING beside the film — a theatre, a musical ([[ListingShape.StageWords]]) — outweighs all but a
   *  stated fact: the widely screened film, the current release and the venue's town are no answer to a staging of its work.
   *  `scored`: the member's own scoring of the film, its record and measures. Measured on the five convergence legs'
   *  whole-corpus takes (2026-10-14). */
  private[identity] def billedAsPiece(node: EvidenceNode, scored: services.identity.Scored, corroborated: Double,
                                      recurs: String => Boolean, answered: CandidateQuery => Seq[Hit]): Option[String] = {
    val film     = scored.candidate.film
    def screened = corroborated >= PieceCorroboration
    def stated   = IdentityMeasures.ownAgreement(scored.measures)._1.nonEmpty ||
      IdentityMeasures.runtimeGap(scored.measures).exists(_ <= PieceRuntimeAgreement) ||
      node.listings.exists(listing => SourceReliability.tmdbLinked(listing).exists(_._2 == scored.candidate.tmdbId))
    // a current release — of its screenings' year or the one before, or one TMDB dates no release of yet — whose subtitle
    // or translation its venue bills beside it ("Über Unterbiberger – Vom Woid in die Welt", 2026, ten venues): the
    // namesakes a reading or a show is taken for are the catalogue's older films ("Immer Ärger mit Harry", 1955)
    def current = film.year.fold(film.released.isEmpty)(year =>
      node.listings.flatMap(_.screenings.first).map(_.getYear).minOption.exists(year >= _ - CurrentRelease))
    // a foreign film TMDB knows by no title in the venue's language — its local title is its original — billed first, or
    // under one of its titles after one other piece: what the venue bills beside it is its own translation, subtitle or
    // subject (DE "Ekmek parası - Geld für's Brot", "Épines du Sahel - Die Dornen der Sahelzone", "LÍDA BAAROVÁ – DOOMED
    // BEAUTY"); a film of a country speaking the venue's language — its own, or another this deployment serves in that
    // language — carries its title, so its title being its original says nothing (UK "The Wizard Of Oz - Hot Rock", a
    // touring pantomime, is not the 1939 US film untranslated)
    def untranslated = film.originalTitle.exists(IdentityMeasures.key(_) == IdentityMeasures.key(film.title)) &&
      !film.countries.exists(_.exists(country => node.country.exists(venue => IdentityMeasures.sharesLanguage(venue, country)))) &&
      (services.movies.TitleContainment.tokens(node.evidence.rawTitle).startsWith(services.movies.TitleContainment.tokens(film.title)) ||
        ListingShape.titledAfterPiece(node.evidence.rawTitle, film))
    import services.movies.TitleContainment.tokens
    val evidence = node.evidence
    // a word of every listing's venue's name or city, inflected as it may be ("Kino w Kadrze": "Kadr Non-Fiction")
    def venues(word: String) = word.length >= VenueStem && node.listings.forall(listing => EvidenceNode.placesOf(listing.cinema).flatten
      .exists(place => place.length >= VenueStem && (word.startsWith(place) || place.startsWith(word))))
    val listing = node.listings.head.copy(rawTitle = evidence.rawTitle, title = evidence.title, originalTitle = evidence.originalTitle)
    // a piece naming a stage show — a theatre, a musical ([[ListingShape.StageWords]]), not the venue's own name — beside the
    // film: the billing is a staging of the work, however widely the film is screened or whichever town the piece names
    // (UK "SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY", four venues billing the 1992 film plainly)
    def staging(piece: Seq[String]) = piece.exists(word => ListingShape.StageWords(word) && !venues(word))
    lazy val filmWords = film.titles.flatMap(tokens).toSet
    lazy val staged = Option.when(staging(tokens(evidence.rawTitle).filterNot(filmWords)))(
      ListingShape.billedBeside(listing, Seq(film), piece => !staging(piece))).flatten
      .map(why => s"no film: one piece of the billing names '${film.title}' beside a staging — $why")
    if (stated) None
    // a billing listing several works by name is a programme, whatever its first piece's film (DE "Retro - An Undiscovered
    // World, Running High, Carrying Dreams: The Weight Between", four shorts): no keep reads it as one film's
    else if (screened || (!ListingShape.listsWorks(node.evidence.rawTitle) && (current || untranslated))) staged
    else {
      lazy val banners = evidence.published.shapes.filter(recurs).map(tokens)
      lazy val tags    = IdentityMeasures.capitalisedTags(evidence.published).map(tokens).filter(_.sizeIs == 1)
      // one word of consonants alone: a programme's initials ("Kopnięci w czasie|Fks", a seniors' film club)
      def acronym(piece: Seq[String]) = piece.sizeIs == 1 && !piece.head.exists(c => "aeiouy".contains(c) || c.isDigit)
      def dated(word: String) = word.length == 4 && word.forall(_.isDigit) && film.year.exists(_ < word.toInt)
      // another title of the film itself: its search's first film, or one IMDb lists under it in some language (DE
      // "Épines du Sahel - Die Dornen der Sahelzone", the German release title TMDB's record does not carry)
      val id = scored.candidate.tmdbId
      def titles(piece: Seq[String]) = evidence.published.shapes.exists(shape => tokens(shape) == piece &&
        (answered(CandidateQuery.Title(shape)).headOption.exists(_.tmdbId == id) || answered(CandidateQuery.ImdbTitled(shape)).exists(_.tmdbId == id)))
      staged.orElse(ListingShape.billedBeside(listing, Seq(film), piece =>
        piece.exists(word => dated(word) || venues(word)) || acronym(piece) || tags.contains(piece) || banners.contains(piece) || titles(piece))
        .map(why => s"no film: one piece of the billing names '${film.title}' — $why"))
    }
  }
  /** The members QUOTING their cluster's film within words of their own, at a venue another member of the cluster bills
   *  the film by itself at — that very title, unquoted, no bill: what that venue screens beside the film — a warm-up, a talk — not the film. The quotes name
   *  the work (PL "Festiwal Korelacje: „Zemsta” z autorską narracją Artura Andrusa", [[ListingShape.billedBeside]]),
   *  but a venue billing the work on its own as well bills this as another thing: DE Kleines Theater am Weingarten's
   *  "Mystery-Vorfilm als Warm-up zu „Miles Davis – Birth of The Cool“" beside its "MILES DAVIS: BIRTH OF THE COOL". */
  private[identity] def besideItsScreening(members: Seq[EvidenceNode]): Seq[(EvidenceNode, String)] = {
    // the film quoted mid-sentence, a word of the venue's running into the quote ("… als Warm-up zu „…“"): a quote heading
    // the billing ("„Baranek Shaun i kudłata bestia” Rodzinne Poranki Filmowe") or after a break names the evening's film
    def quotedMidSentence(node: EvidenceNode): Seq[String] =
      IdentityMeasures.QuotedTitle.findAllMatchIn(node.evidence.rawTitle).toSeq
        .filter(m => node.evidence.rawTitle.take(m.start).trim.lastOption.exists(_.isLetter)).map(m => IdentityMeasures.key(m.group(1)))
    members.flatMap { node =>
      val quoted = quotedMidSentence(node).toSet
      val venues = node.listings.map(_.cinema).toSet
      Option.when(quoted.nonEmpty)(()).flatMap(_ => members.find(other => (other ne node) &&
          ListingShape.Quote.findFirstIn(other.evidence.rawTitle).isEmpty && quoted(IdentityMeasures.key(other.evidence.rawTitle)) &&
          other.listings.exists(listing => venues(listing.cinema))))
        .map(other => node -> s"no film: it quotes the film its venue bills by itself as '${other.evidence.rawTitle}'")
    }
  }
  /** How many years before its screenings' a film's may be and still be a current release ([[billedAsPiece]]). */
  val CurrentRelease = 1
  /** The fewest letters a word shares with its venue's name or city to name it: "Kadr" in "Kino w Kadrze". */
  val VenueStem = 4
  /** How many minutes a listing's running time may stand from its film's and still state the film's own. */
  val PieceRuntimeAgreement = 3
  /** How many other venues stating the film's facts under the listing's title make it a film screened widely: below the
   *  30 and more of PL's current releases a programme bills beside its banner (2026-10-14). A staging billed beside the
   *  film is none of it however widely the film is screened (UK "SISTER ACT - NORTHALLERTON MUSICAL THEATRE COMPANY"). */
  val PieceCorroboration = 5

  private[identity] enum Mutation {
    case None
    /** A2 broken: must-links applied in arrival order, first wins. */
    case FirstWins
    /** A1 broken: listings walked in arrival order, a title query whose sanitised form an
     *  earlier listing already asked is skipped — the old "sister-row shortcut". */
    case LazyLookups
    /** Group-level voting off: a cluster with no own match stays unmatched. */
    case NoVoting
    /** Families narrowed to the sanitised title: an edge through an original title, a search
     *  form or a shared film then crosses a family and the resolve must refuse. */
    case NarrowFamilies
  }

  /** Thrown when `count` edges cross a family: a rule was added without its block key. */
  final class FamilyCrossing(val count: Int, message: String) extends IllegalStateException(message)

  /** `pins` are the curation's hard constraints (`PinConstraints`): a pinned film
   *  replaces a listing's own match, a denied one is never eligible, a pinned group is must-linked
   *  above every derived tier, and a derived edge the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              pins: PinConstraints = PinConstraints(Nil),
              decorations: TitleDecorations = TitleDecorations.resolver): Resolution =
    resolveWith(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)

  /** Stage A of a resolve — candidate generation, scoring and families — wired once, for the resolve
   *  and for [[candidatesOf]]. */
  private final class Stages(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                             calibration: IdentityCalibration, mutation: Mutation, pins: PinConstraints, decorations: TitleDecorations,
                             corpus: Option[CorpusContext] = None) {
    private val arrival = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
    // One listing per key, the smallest by the total order — never the first to arrive.
    private val all     = listings.toSeq.sorted.distinctBy(_.key)
    val ordered: Seq[Listing] = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all
    val generation = new CandidateGeneration(ordered, lookups, normalizer, pins, decorations, lazyLookups = mutation == Mutation.LazyLookups, corpus)
    val acceptance = new Acceptance(calibration)
    val scoring    = new CandidateScoring(generation, calibration, acceptance.weights, pins)
    val links      = new TitleLinks(generation.nodes, normalizer, pins, generation.context.wholeTitle, generation.context.bannerSegment)
    val families   = new Families(scoring, acceptance, links, normalizer, narrow = mutation == Mutation.NarrowFamilies)
  }

  /** One candidate as a node's family scored it, for a report reading why a listing took what it took. */
  final case class CandidateScore(tmdbId: Int, title: String, year: Option[Int], probability: Double, searchRank: Option[Int],
                                  denial: Option[String], seasonProduction: Boolean, houseProduction: Boolean, explanation: String,
                                  untaken: Option[String] = None) {
    def denied: Boolean = denial.isDefined
    def render: String =
      f"$tmdbId%8d ${ResolverDecision.percent(probability)}%6s rank ${searchRank.fold("-")(_.toString)}%2s" +
        s"${denial.fold("")(why => s" DENIED ($why)")}${untaken.fold("")(why => s" UNTAKEN ($why)")}${if (seasonProduction) " season" else ""}${if (houseProduction) " house" else ""} " +
        s"'$title'${year.fold("")(filmYear => s" ($filmYear)")} — $explanation"
  }
  object CandidateScore {
    private[identity] def of(scored: services.identity.Scored, calibration: IdentityCalibration): CandidateScore =
      CandidateScore(scored.candidate.tmdbId, scored.candidate.film.title, scored.candidate.film.year, scored.probability, scored.rank,
        scored.denial, scored.seasonProduction, scored.houseProduction, calibration.explain(IdentityMeasures.ListingFilm, scored.measures),
        scored.untaken)
  }

  /** A focused node, its candidates as its family scores them (best first), and each banner its
   *  candidates bill it under with the houses contending for that banner, best first, and the one learned. */
  final case class NodeCandidates(label: String, candidates: Seq[CandidateScore], banners: Seq[String])

  /** Why a listing resolved as it did and sits where it does — the resolve's own stages, told:
   *  its node; every family key it has and why, and every title piece it does not block under and
   *  why not; the chain of shared keys linking it to its family's heaviest member (why THIS
   *  family); its decision with the decision's own explanation; and its candidates as its family
   *  scored them. */
  final case class ListingExplanation(node: String, family: Int, familyListings: Int, keys: Seq[(String, String)],
                                      dropped: Seq[(String, String)], chain: Seq[(String, String)], decision: ResolverDecision,
                                      candidates: Seq[CandidateScore]) {
    def render: Seq[String] =
      Seq(s"listing in node $node, family $family ($familyListings listing(s))",
          s"  decision: ${decision.render}") ++
        keys.map { case (key, why) => s"  key $key — $why" } ++
        dropped.map { case (piece, why) => s"  not a key: $piece — $why" } ++
        (if (chain.isEmpty) Seq("  the family's heaviest node itself") else s"  in this family by:" +: chain.map { case (key, next) => s"    —$key→ $next" }) ++
        candidates.take(8).map(candidate => s"  candidate ${candidate.render}")
  }

  def explain(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              pins: PinConstraints = PinConstraints(Nil),
              decorations: TitleDecorations = TitleDecorations.resolver)(wanted: Set[ListingKey]): Seq[ListingExplanation] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)
    import stages.families.{blockKeysOf, familyOf, scopeOf}
    lazy val resolution = run(stages, Mutation.None)
    // One explanation per node holding a wanted listing: a node's listings share everything told.
    stages.generation.nodes.filter(_.listings.exists(listing => wanted(listing.key))).map { node =>
      val listing  = node.listings.find(listing => wanted(listing.key)).get.key
      val context  = stages.generation.context
      val keyed    = TitleLinks.keyed(node, normalizer, pins, context.wholeTitle, context.bannerSegment)
      val titled   = keyed.keys
      val accepted = blockKeysOf(node.id).toSeq.sorted.filterNot(titled).map(key => key -> "a film it, or a node sharing its keys, accepted")
      val members  = stages.generation.nodes.filter(member => familyOf(member.id) == familyOf(node.id))
      val anchor   = members.minBy(member => (-member.weight, member.id))
      // Breadth-first over shared keys from the node to the anchor: the links that put it here.
      val byKey    = members.flatMap(member => blockKeysOf(member.id).map(_ -> member)).groupMap(_._1)(_._2)
      val via      = scala.collection.mutable.HashMap(node.id -> Option.empty[(String, EvidenceNode)])
      val queue    = scala.collection.mutable.Queue(node)
      while (queue.nonEmpty && !via.contains(anchor.id)) {
        val current = queue.dequeue()
        blockKeysOf(current.id).toSeq.sorted.foreach(key => byKey.getOrElse(key, Nil).foreach { next =>
          if (!via.contains(next.id)) { via(next.id) = Some(key -> current); queue.enqueue(next) } })
      }
      def path(at: EvidenceNode): List[(String, String)] = via.get(at.id).flatten match {
        case Some((key, from)) => path(from) :+ (key -> at.label)
        case None              => Nil
      }
      val decision = resolution.decisionOf(listing)
      ListingExplanation(node.label, familyOf(node.id), members.map(_.weight).sum, keyed.kept ++ accepted, keyed.dropped,
        if (anchor eq node) Nil else path(anchor), decision, scopeOf(node).of(node).map(CandidateScore.of(_, calibration)))
    }
  }

  /** One of the largest families, taken apart: its size, the block keys holding the most of its
   *  nodes, and for each of those the nodes the family's largest piece keeps once that key is dropped
   *  — a key that alone glues the family together leaves a small piece. For a report finding why
   *  a family grew far past one film. */
  final case class FamilyAnatomy(listings: Int, nodes: Int, keys: Seq[(String, Int)], withoutKey: Seq[(String, Int)],
                                 connectors: Seq[(String, Seq[String])] = Nil, withoutKind: Seq[(String, Int)] = Nil) {
    def render: Seq[String] =
      s"family of $listings listing(s) in $nodes node(s)" +:
        (s"  keys by nodes: ${keys.map { case (key, count) => s"$key ×$count" }.mkString(", ")}" +:
          (withoutKind.map { case (kind, largest) => s"  without every $kind key: largest piece $largest node(s)" } ++
            withoutKey.map { case (key, largest) => s"  without $key: largest piece $largest node(s)" } ++
            connectors.map { case (node, shared) => s"  connector $node: ${shared.mkString(", ")}" }))
  }

  def familyAnatomy(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                    calibration: IdentityCalibration = IdentityCalibration.resolver,
                    decorations: TitleDecorations = TitleDecorations.resolver)(largest: Int): Seq[FamilyAnatomy] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations)
    import stages.families.{blockKeysOf, familyOf}
    stages.generation.nodes.groupBy(node => familyOf(node.id)).values.toSeq.sortBy(members => -members.map(_.weight).sum).take(largest).map { members =>
      val keysOf = members.map(node => node.id -> blockKeysOf(node.id)).toMap
      val byKey  = keysOf.toSeq.flatMap { case (id, keys) => keys.map(_ -> id) }.groupMap(_._1)(_._2)
      val ranked = byKey.toSeq.map { case (key, ids) => key -> ids.size }.sortBy { case (key, count) => (-count, key) }
      def largestPieceKeeping(kept: String => Boolean): Int =
        FamilyClosure.families(keysOf.map { case (id, keys) => id -> keys.filter(kept) }).groupBy(_._2).values.map(_.size).maxOption.getOrElse(0)
      def largestPieceWithout(dropped: String): Int = largestPieceKeeping(_ != dropped)
      // Which KIND of key meshes it: a sanitised title or segment (t:), a search form (q:), a film
      // members accepted (id:), a pin's group.
      val withoutKind = Seq("t:", "q:", "id:").map(kind => kind -> largestPieceKeeping(key => !key.startsWith(kind))) :+
        ("t: and q:" -> largestPieceKeeping(key => !key.startsWith("t:") && !key.startsWith("q:")))
      // The nodes joining the most keys other members carry: in a mesh no single key bridges, these weld it.
      // Each title key with the inputs the segment rule read: its spread and whether it is a whole title.
      val context = stages.generation.context
      def explained(key: String) =
        if (key.startsWith("t:")) s"$key(spread ${context.segmentSpread(key.drop(2))}${if (context.wholeTitle(key.drop(2))) ", whole" else ""})" else key
      val connectors = members.map(node => node -> keysOf(node.id).filter(key => byKey(key).sizeIs > 1).toSeq.sorted)
        .filter(_._2.sizeIs >= 2).sortBy { case (node, shared) => (-shared.size, node.id) }.take(40)
        .map { case (node, shared) => s"${node.label} «${node.evidence.published.rawTitle.getOrElse(node.evidence.title)}»" -> shared.map(explained) }
      FamilyAnatomy(members.map(_.weight).sum, members.size, ranked.take(25), ranked.take(15).map { case (key, _) => key -> largestPieceWithout(key) },
        connectors, withoutKind)
    }
  }

  /** Every node holding a listing `focused` selects — the resolve's own stages, so the report shows
   *  what the resolve read. */
  def candidatesOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                   calibration: IdentityCalibration = IdentityCalibration.resolver,
                   pins: PinConstraints = PinConstraints(Nil),
                   decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeCandidates] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)
    import stages.scoring.{houseRanking, houses}
    focusedScored(stages)(focused).map { case (node, scored) =>
      val banners = scored.flatMap(candidate => StageRelays.billing(node.evidence.measured, candidate.candidate.film)).map(_.listingHouse).distinct.sorted
      NodeCandidates(node.label, scored.map(CandidateScore.of(_, calibration)),
        banners.map(banner => s"banner '$banner' → ${houses.of.getOrElse(banner, "no house")}; contenders: " +
          houseRanking.getOrElse(banner, Nil).take(4).map(_.render).mkString(", ")))
    }
  }

  /** One candidate as a node's family scored it: the calibrated probability, the search rank, whether the node's own
   *  evidence denies it, whether the title names it, and whether its record is the listing's season or house production
   *  — the model's side of the unified evidence model's signals ([[UnifiedEvidence]]) — the measures it was scored
   *  on, which a refit of the calibration counts (`scripts.IdentityRefit`), and, a namesake the release veto took out
   *  ([[ReleaseVeto]]), the namesake it stood beside that TMDB dates a release of in the venue's country. */
  final case class CandidateEvidence(tmdbId: Int, film: IdentityMeasures.Film, probability: Double, rank: Option[Int], denied: Boolean,
                                     titleNamesIt: Boolean, seasonProduction: Boolean, houseProduction: Boolean,
                                     measures: Map[String, IdentityMeasures.Measure] = Map.empty, releasedBeside: Option[Int] = None)
  /** A node's listings and its candidates as its family scored them (best first). */
  final case class NodeEvidence(keys: Seq[ListingKey], candidates: Seq[CandidateEvidence])

  /** Every node holding a listing `focused` selects, its candidates scored as the resolve scores them. */
  def evidenceOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                 calibration: IdentityCalibration = IdentityCalibration.resolver,
                 decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeEvidence] =
    evidenceIn(new Stages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations))(focused)

  private def evidenceIn(stages: Stages)(focused: Listing => Boolean): Seq[NodeEvidence] =
    focusedScored(stages)(focused).map { case (node, scored) =>
      NodeEvidence(node.listings.map(_.key), scored.map(s => CandidateEvidence(s.candidate.tmdbId, s.candidate.film, s.probability, s.rank, s.denied,
        s.titleNamesIt, s.seasonProduction, s.houseProduction, s.measures, s.denial.flatMap(ReleaseVeto.releasedBeside))))
    }

  /** Every node holding a listing `focused` selects, with its candidates as its family scores them (best first). */
  private def focusedScored(stages: Stages)(focused: Listing => Boolean): Seq[(EvidenceNode, Seq[services.identity.Scored])] =
    stages.generation.nodes.filter(_.listings.exists(focused)).map(node => node -> stages.families.scopeOf(node).of(node))

  /** A [[resolve]], and every node's candidates as that same resolve scored them ([[evidenceOf]], read only when asked):
   *  one pass for a caller wanting both — building the stages a second time was a whole second resolve. A family's verdict
   *  (`agreement.Agreement.verdict`, its only caller) takes a film one piece of the billing names ([[billedAsPiece]]): what
   *  the families agree on is the agreement stage's to read against the billing (`AgreementStage.pieceOf`), which also keeps
   *  a fill off it. */
  final class Scored private[IdentityResolver] (val resolution: Resolution, stages: Stages) {
    lazy val evidence: Seq[NodeEvidence] = evidenceIn(stages)(_ => true)
  }
  def resolveScored(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                    calibration: IdentityCalibration = IdentityCalibration.resolver): Scored = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), TitleDecorations.resolver)
    new Scored(run(stages, Mutation.None, pieces = false), stages)
  }

  /** The corpus-wide facts a resolve of `listings` reads ([[CorpusContext]]): what one family,
   *  resolved alone with `corpus = Some(...)`, needs to decide as the whole resolve does. */
  private[identity] def contextOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                  pins: PinConstraints = PinConstraints(Nil),
                                  decorations: TitleDecorations = TitleDecorations.None): CorpusContext =
    new CandidateGeneration(listings.toSeq.sorted.distinctBy(_.key), lookups, normalizer, pins, decorations, lazyLookups = false).context

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    calibration: IdentityCalibration, mutation: Mutation,
                                    pins: PinConstraints = PinConstraints(Nil),
                                    decorations: TitleDecorations = TitleDecorations.None,
                                    corpus: Option[CorpusContext] = None): Resolution = {
    run(new Stages(listings, lookups, normalizer, calibration, mutation, pins, decorations, corpus), mutation)
  }

  /** One family of a region's resolve ([[resolveRegion]]): its listings, its decisions, the keys it
   *  blocks under (its title keys and the films its members matched — what another family must share
   *  to merge with it), the questions and records its answers came from, and the corpus-wide facts
   *  it could read ([[CorpusContext.Reads]]). */
  private[identity] final case class RegionFamily(listings: Set[ListingKey], decisions: Seq[ResolverDecision], blockKeys: Set[String],
                                                  queries: Set[CandidateQuery], films: Set[Int], reads: CorpusContext.Reads,
                                                  nodeKeys: Map[ListingKey, String]) {
    /** This family naming each listing by `keyOf`'s object for its key and each node by `textOf`'s for its text — equal
     *  values, so it decides and stores exactly as before, and is built anew only where some object differs. A family
     *  decoded from the store names its listings by keys of its own; kept so, worker-us held two more key objects per
     *  listing (its family's and its decision's) and a node text per node beside the corpus's, for the model's life. */
    def sharing(keyOf: ListingKey => ListingKey, textOf: (ListingKey, String) => String): RegionFamily = {
      val keysShared  = listings.forall(key => keyOf(key) eq key)
      val decided     = decisions.forall(_.members.forall(key => keyOf(key) eq key))
      val nodesShared = nodeKeys.forall { case (key, text) => (keyOf(key) eq key) && (textOf(key, text) eq text) }
      if (keysShared && decided && nodesShared) this
      else copy(
        listings  = if (keysShared) listings else listings.map(keyOf),
        decisions = if (decided) decisions else decisions.map(decision => decision.copy(members = decision.members.map(keyOf))(decision.trace)),
        nodeKeys  = if (nodesShared) nodeKeys else nodeKeys.map { case (key, text) => keyOf(key) -> textOf(key, text) })
    }
  }

  /** Resolve `listings` — a union of whole families — against the corpus's `corpus` context, family
   *  by family: A3 with the corpus's facts, so each decides as the whole resolve would. */
  private[identity] def resolveRegion(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                      calibration: IdentityCalibration, pins: PinConstraints, decorations: TitleDecorations,
                                      corpus: CorpusContext): Seq[RegionFamily] = {
    val stages     = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations, Some(corpus))
    val resolution = run(stages, Mutation.None)
    import stages.{families, generation}
    // Each decision is one family's: filed by its first member's, not filtered out of every decision per family.
    val decisionsOf = resolution.decisions.groupBy(decision => resolution.familyOf(decision.members.head))
    generation.nodes.groupBy(node => families.familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members) =>
      val keys  = members.flatMap(_.listings.map(_.key)).toSet
      val pool  = families.scopes(family).pool
      val queries = members.flatMap(node => generation.queriesOf(node.id)).toSet
      val films = members.flatMap(node => generation.hitsOf(node.id).map(_.tmdbId)).toSet
      val decisions = decisionsOf.getOrElse(family, Nil)
      RegionFamily(keys, decisions, members.flatMap(node => families.blockKeysOf(node.id)).toSet, queries, films,
        CorpusContext.Reads.of(members, pool.map(_.film), films ++ pool.map(_.tmdbId) ++ decisions.flatMap(_.film), queries, normalizer.sanitize),
        members.flatMap(node => node.listings.map(listing =>
          listing.key -> CandidateGeneration.nodeKeyText(CandidateGeneration.nodeKey(listing, node.evidence, pins)))).toMap)
    }
  }

  /** `pieces`: whether a member billing its cluster's film as one piece beside another thing is decided apart, as no film
   *  ([[billedAsPiece]]) — the model's own takes; not a family's verdict ([[resolveScored]]). `corroborated`: whether a
   *  member no fact of its own corroborates is decided apart too ([[Corroboration.refused]]) — the model's own takes; a
   *  family's verdict refuses only an evidenced namesake's. */
  private def run(stages: Stages, mutation: Mutation, pieces: Boolean = true, corroborated: Boolean = true): Resolution = {
    import stages.{acceptance, families, generation, links, ordered, scoring}
    import generation.{answers, candidateOf, details, issued, nodeById, nodes, records}
    import families.{familyOf, scopes}

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    val edges     = new ConstraintEdges(scoring, families, links)
    val voting    = new ClusterVoting(scoring, families, acceptance)
    val decisions = new ResolverDecisions(scoring, families, acceptance)

    def solve(members: Seq[EvidenceNode], edges: Seq[ResolverEdge], filmOf: String => Option[Int]): Seq[Seq[EvidenceNode]] = {
      val constraints = edges.map(edge => ConstraintSolver.Constraint(edge.a, edge.b, edge.must, edge.tier, edge.reason))
      val presentation = if (mutation == Mutation.FirstWins) ConstraintSolver.Presentation.AsGiven else ConstraintSolver.Presentation.Canonical
      val presented = if (mutation == Mutation.FirstWins) {
        val position = ordered.zipWithIndex.map { case (listing, index) => listing.sortKey -> index }.toMap
        members.sortBy(node => node.listings.map(listing => position(listing.sortKey)).min)
      } else members
      val orderedConstraints = if (mutation == Mutation.FirstWins) {
        val position = presented.zipWithIndex.map { case (node, index) => node.id -> index }.toMap
        constraints.sortBy(constraint => (math.min(position(constraint.a), position(constraint.b)), math.max(position(constraint.a), position(constraint.b))))
      } else constraints
      ConstraintSolver.solveAs(presented.map(_.id), orderedConstraints, presentation, members.flatMap(node => filmOf(node.id).map(node.id -> _)).toMap)
        .map(_.map(nodeById))
    }

    val ownFilms: Map[String, Int] = families.bestOf.map { case (id, (scored, _)) => id -> scored.candidate.tmdbId }
    // Nodes one chain lists under ONE catalogue id that accepted different films, each on its own
    // evidence: neither acceptance stands, so the id's must-link unites them and the cluster votes on
    // its pooled evidence. Showcase's "9 to 5 (2026)" credits Colin Higgins at two venues; its two
    // bare venues, dated only by the re-release, had taken a two-minute 2026 short.
    val contested: Set[String] =
      nodes.flatMap(node => node.listings.flatMap(_.catalogueIds).distinct.map(id => edges.catalogueKey(id, node) -> node.id)).groupMap(_._1)(_._2).values
        .flatMap(ids => if (ids.distinct.flatMap(ownFilms.get).distinct.sizeIs > 1) ids else Nil).toSet
    val acceptedAll: Map[String, Int] = ownFilms.filterNot { case (id, _) => contested(id) } ++ families.pinnedFilm
    // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
    // edge between two families means the scoping would silently drop it, so the resolve stops.
    val roundAEdges = edges.of(nodes, acceptedAll.get)
    val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(edge => FamilyClosure.Edge(edge.a, edge.b, edge.must, edge.reason)))
    if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
    val roundAByFamily = roundAEdges.groupBy(edge => familyOf(edge.a))

    val perFamily = nodes.groupBy(node => familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members0) =>
      val members = members0.sortBy(_.id)
      val scope   = scopes(family)
      val accepted: Map[String, Int] = members.flatMap(node => acceptedAll.get(node.id).map(node.id -> _)).toMap

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil), accepted.get)
      // Group-level voting over the clusters no member matched alone.
      val unaccepted = roundA.filter(_.forall(node => !accepted.contains(node.id)))
      // A facts-free cluster in a split title family follows the family's clear majority
      // (`ClusterVoting.familyMajority`); every other cluster votes on its pooled evidence.
      val familyTaken: Map[String, (Int, Double, String)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else {
          // The title family: the round's title must-links (same title, search form, original
          // title, segment), however the solver then split the nodes they join.
          val titleEdges = roundAByFamily.getOrElse(family, Nil).filter(edge => edge.must && ConstraintEdges.TitleTiers(edge.tier))
          unaccepted.flatMap(cluster => voting.familyMajority(cluster, members, accepted, titleEdges).toSeq.flatMap(taken => cluster.map(_.id -> taken))).toMap
        }
      val votes: Map[String, ResolverDecisions.Vote] =
        if (mutation == Mutation.NoVoting) Map.empty
        else unaccepted.flatMap { cluster =>
          if (cluster.exists(node => familyTaken.contains(node.id)))
            cluster.map(node => node.id -> { val (film, probability, why) = familyTaken(node.id); ResolverDecisions.Vote(film, probability, Some(why)) })
          else voting.vote(cluster, scope).map { case (id, (film, probability)) => id -> ResolverDecisions.Vote(film, probability, None) }
        }.toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(votes.get(id).map(_.film))
      val familyEdges = edges.of(members, filmOf)
      val clusters    = solve(members, familyEdges, filmOf)

      val clusterIndex = clusters.zipWithIndex.flatMap { case (cluster, index) => cluster.map(_.id -> index) }.toMap
      val violations   = familyEdges.count(edge => !edge.must && clusterIndex(edge.a) == clusterIndex(edge.b))
      // A member billing several works is none of them, whichever film its cluster took or voted
      // ("We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw}, joined by its
      // title to the spelling crediting Harrison alone — itself a bill): it is decided apart, as no film.
      val bills   = members.filter(node => acceptance.billsSeveralWorks(scope.of(node))).map(_.id).toSet
      val billing = bills ++ IdentityResolver.billingUnderBanner(members, bills, filmOf, scope.of)
      def answered(query: CandidateQuery): Seq[Hit] = answers.get(query).flatMap(_.toOption).getOrElse(Nil)
      // a piece the corpus bills beside two whole titles or more, whatever families they fell in
      def recurs(shape: String) = generation.context.repeatedSegment(generation.normalizer.sanitize(shape))
      val backed  = Corroboration.support(clusters, _.iterator.flatMap(node => filmOf(node.id)).nextOption(), scope.of)
      val decided = clusters.flatMap { cluster =>
        val (bills, unbilled) = cluster.partition(node => billing(node.id))
        // a member billing its cluster's film as one piece beside another thing is none of it either — unless a pin took it
        val film   = cluster.iterator.flatMap(node => filmOf(node.id)).nextOption()
        val pinned = cluster.exists(node => families.pinnedFilm.contains(node.id))
        val piecesOf = film.filterNot(_ => pinned || !pieces).toSeq.flatMap { film =>
          val scored       = unbilled.flatMap(node => scope.of(node).find(_.candidate.tmdbId == film).map(node -> _))
          val corroborated = scored.flatMap(_._2.number("venues.corroborating")).maxOption.getOrElse(0.0)
          scored.flatMap { case (node, own) => IdentityResolver.billedAsPiece(node, own, corroborated, recurs, answered).map(node -> _) }
        }.toMap ++ film.filterNot(_ => pinned || !pieces).toSeq.flatMap(_ => IdentityResolver.besideItsScreening(unbilled)).toMap
        val placed = unbilled.filterNot(piecesOf.contains)
        // a member nothing of its own corroborates the film by, nor a member billed alike or title-linked to one
        val weak   = film.filterNot(_ => pinned).toSeq.flatMap(Corroboration.refused(placed, _, scope.of, familyEdges, backed, corroborated)).toMap
        val rest   = placed.filterNot(weak.contains)
        if ((bills.isEmpty && piecesOf.isEmpty && weak.isEmpty) || film.isEmpty)
          Seq(decisions.of(cluster, scope, filmOf, accepted, votes, familyEdges, clusterIndex))
        else {
          // a cluster of bills or pieces alone takes no film its vote reaches either
          def none(nodes: Seq[EvidenceNode], why: Seq[String], veto: String) = {
            val neither = decisions.of(nodes, scope, _ => None, Map.empty, Map.empty, familyEdges, clusterIndex)
            neither.copy(explanation = why ++ neither.explanation)(neither.trace.copy(vetoed = Some(DecisionTrace.Veto(veto, None))))
          }
          Option.when(rest.nonEmpty)(decisions.of(rest, scope, filmOf, accepted, votes, familyEdges, clusterIndex)).toSeq ++
            Option.when(bills.nonEmpty)(none(bills, Seq("bills several works: none of them"), "bills both works")) ++
            Option.when(piecesOf.nonEmpty)(none(unbilled.filter(piecesOf.contains), unbilled.flatMap(piecesOf.get).distinct, "billed as one piece")) ++
            Option.when(weak.nonEmpty)(none(placed.filter(weak.contains), placed.flatMap(weak.get).distinct, "uncorroborated"))
        }
      }
      (familyEdges, decided, violations)
    }

    val decided = perFamily.flatMap(_._2).sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(
      decisions      = decided,
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(node => node.listings.map(_.key -> familyOf(node.id))).toMap,
      edges          = perFamily.flatMap(_._1),
      queries        = issued.toSeq,
      filmLookups    = if (generation.partOfCorpus) 0 else records.size,
      unknownQueries = answers.count(!_._2.isKnown),
      unknownDetails = details.count(!_._2.isKnown),
      unknownFilms   = if (generation.partOfCorpus) 0 else records.count(!_._2.isKnown),
      violations     = perFamily.map(_._3).sum,
      films          = decided.flatMap(_.film).distinct.flatMap(id => candidateOf(id).map(id -> _.film)).toMap,
      scorings       = families.scorings,
      titleRelations = families.titleRelations)
  }
}
