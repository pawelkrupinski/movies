package services.identity

/** Does a listing's OWN evidence corroborate the film its cluster took — at least one fact independent of the title and
 *  of the search that found the film? A bare title with nothing else takes whichever namesake TMDB ranks first or the
 *  cluster pooled to, and a namesake is the commonest wrong match: DE CITY 46's "THE BLACK HOLE" (Siimets' 2024 film by
 *  its Cineamo id) took Disney's 1979 film from the "Das schwarze Loch" listing it was clustered with; ES Bahia Mar's
 *  "ESCAPE ROOM 2" the 2021 sequel while 63 venues bill the 2026 Catalan film; DE Caligari's "ES (ULRICH SCHAMONI)"
 *  Muschietti's "It".
 *
 *  A node corroborates film `f` by any of:
 *  - `year`: a published year within one of the film's ([[IdentityMeasures.ownAgreement]]), or a year its title states;
 *  - `director`: a director it credits is the film's;
 *  - `original title`: an original title of its own (not its title restated) that is the film's;
 *  - `runtime`: a running time within [[RuntimeBand]] minutes of the film's that no namesake's fits as well — 90 minutes
 *    fit both "Escape Room 2"s, so they tell neither apart;
 *  - `feed id`: a TMDB-keyed feed's id linking it to `f` ([[SourceReliability.tmdbLinked]]);
 *  - `season`: a stage relay's screening days placing it in the season its record names ([[Scored.seasonProduction]]);
 *  - `house`: a stage relay's billing naming the house whose record of its work the film is ([[Scored.houseProduction]]);
 *  - `current release`: its first screening falls within [[CurrentRelease]] years of a cinema release TMDB dates of the
 *    film in the venue's country (any country's when it dates none there) while no namesake's, nor a film's whose work
 *    its title heads, does (DE CineStar's bare "Lieblingsmenschen" beside Hufnagel's 2026 film) — the venue's own
 *    showtimes, not its title: a bare "Eliminacja" screening in the month it opens is that release, a bare "Patagonien"
 *    three years after Bozzelli's "Patagonia" opened is a guess at it.
 *
 *  A node with none still takes the film when another node of its cluster billed under the SAME title corroborates it on
 *  its own — its title, clean title, search title, any of them without a bracketed year, or what a learned decoration leaves of
 *  it alike — or joined to such a node by a [[Vouching]] must-link (US
 *  Showcase's chain id joins "El laberinto del fauno" to its credited "Pan's Labyrinth"): a bare "Frankenstein" beside
 *  the venues crediting del Toro is the cluster's film, not a guess — unless the family takes an evidenced namesake under
 *  that very title on more nodes' facts, when only a catalogue id vouches. A node with neither is decided apart, as no film
 *  ([[IdentityResolver]], [[refused]]): an unmatched listing is far better than a wrong one. */
private[identity] object Corroboration {

  /** The veto a cluster [[refused]] here carries ([[DecisionTrace.Veto]]); the agreement stage asks the same of the films
   *  the families agree on (`agreement.AgreementStage.corroborated`). */
  val Refusal = "uncorroborated"

  /** Minutes a venue's running time may stand off the film's and still corroborate it: trailers and rounding. */
  val RuntimeBand = 10

  /** Years a first screening may stand off a cinema release of the film and still date it as that release: a run
   *  opening late in a year plays on into the next, and a preview screens before its release year. */
  val CurrentRelease = 1

  /** The years of the cinema releases TMDB dates of `film` in `country` (any country's when it dates none there), else
   *  its own year when no release is dated. */
  private def releaseYears(film: IdentityMeasures.Film, country: Option[String]): Seq[Int] = {
    val dated = film.releasesIn(country).map(_.year)
    if (dated.nonEmpty) dated else film.year.toSeq
  }

  /** The facts by which `own` — the node's scoring of the film — corroborates it, `rivals` being the node's other
   *  scored candidates, by name; empty when none does. */
  def facts(node: EvidenceNode, own: Scored, rivals: Seq[Scored]): Seq[String] = {
    val (agree, _) = IdentityMeasures.ownAgreement(own.measures)
    val original   = agree("originalTitle") && node.listings.exists(listing => listing.originalTitle.exists(original =>
      IdentityMeasures.key(original) != IdentityMeasures.key(listing.title) && IdentityMeasures.key(original) != IdentityMeasures.key(listing.rawTitle)))
    def inBand(scored: Scored) = IdentityMeasures.runtimeGap(scored.measures).exists(_ <= RuntimeBand)
    val runtime    = inBand(own) && !rivals.exists(rival => (rival ne own) && rival.candidate.tmdbId != own.candidate.tmdbId && rival.titleNamesIt && inBand(rival))
    val linked     = node.listings.exists(listing => SourceReliability.tmdbLinked(listing).exists(_._2 == own.candidate.tmdbId))
    val season     = own.seasonProduction || own.number("season.delta").contains(0.0)
    val screened   = node.listings.flatMap(_.screenings.first).filterNot(_ == ScreeningDays.UnreadDay).minOption.map(_.getYear)
    def current(scored: Scored) = screened.exists(year => releaseYears(scored.candidate.film, node.country).exists(released =>
      math.abs(year - released) <= CurrentRelease))
    // a rival the title names, or whose work it heads ([[headedBy]]), released as recently tells neither apart
    val released   = current(own) && !rivals.exists(rival => (rival ne own) && rival.candidate.tmdbId != own.candidate.tmdbId &&
      (rival.titleNamesIt || headedBy(rival, naming(own))) && current(rival))
    Seq("year" -> agree("year"), "director" -> agree("director"), "original title" -> original, "runtime" -> runtime, "feed id" -> linked,
      "season" -> season, "house" -> own.houseProduction, "current release" -> released)
      .collect { case (name, true) => name }
  }

  /** The must-links that vouch a node to a corroborated one: its title, clean or search title, catalogue id, a curator's
   *  same-film pin or a delimited segment of the other's title — never the original title alone, which a feed copies from the entry it
   *  linked and so pulls in that entry's namesakes (DE CITY 46's "THE BLACK HOLE", its copied "The Black Hole" beside
   *  "Das schwarze Loch"). */
  val Vouching: Set[String] = Set(ConstraintEdges.SameTitle, ConstraintEdges.SameCatalogueId, ConstraintEdges.SameSearchForm, ConstraintEdges.TitleSegment,
    services.movies.ListingConstraints.MustLink.Pinned.toString)

  /** How many nodes of `clusters` take each film on a fact of their own ([[facts]]) — a film's standing in its title
   *  family, against which a bare node's namesake take is weighed ([[refused]]). `filmOf` is a cluster's film. */
  def support(clusters: Seq[Seq[EvidenceNode]], filmOf: Seq[EvidenceNode] => Option[Int], scored: EvidenceNode => Seq[Scored]): Map[Int, Int] =
    corroborating(clusters, filmOf, scored).groupMapReduce(_._1)(_ => 1)(_ + _)

  /** The titles ([[titlesOf]]) of the nodes of `clusters` that take each film on a fact of their own: the family's
   *  evidenced billings of it, which vouch for a bare node billed alike — or by one piece of its title alike — in another
   *  cluster of the family ([[refused]]): PL Kino Kosmos' "Nakarmić kruki | Złote gody" beside Helios's "Nakarmić kruki"
   *  [1976], which credits Saura's film. */
  def titled(clusters: Seq[Seq[EvidenceNode]], filmOf: Seq[EvidenceNode] => Option[Int], scored: EvidenceNode => Seq[Scored]): Map[Int, Set[String]] =
    corroborating(clusters, filmOf, scored).groupMapReduce(_._1)(entry => titlesOf(entry._2))(_ ++ _)

  private def corroborating(clusters: Seq[Seq[EvidenceNode]], filmOf: Seq[EvidenceNode] => Option[Int], scored: EvidenceNode => Seq[Scored]): Seq[(Int, EvidenceNode)] =
    clusters.flatMap(cluster => filmOf(cluster).toSeq.flatMap(film => cluster.filter(node =>
      scored(node).find(_.candidate.tmdbId == film).exists(facts(node, _, scored(node)).nonEmpty)).map(film -> _)))

  /** A node's titles as billings of one film are compared: its title, clean and search titles, each without a bracketed
   *  year, and what a learned venue decoration leaves of them — "Candyman (1992)" and "(Retro Rewind) Shrek" are billed
   *  under "Candyman" and "Shrek". */
  private[identity] def titlesOf(node: EvidenceNode): Set[String] = {
    val own = node.listings.flatMap(listing => Seq(listing.title, listing.cleanTitle) ++ listing.searchTitle)
    (own ++ own.map(IdentityMeasures.withoutYears) ++ node.evidence.measured.undecoratedWords.map(_.mkString(" "))).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
  }
  /** [[titlesOf]], and each delimited piece of its titles ([[IdentityMeasures.pieces]]). */
  private def piecesOf(node: EvidenceNode): Set[String] =
    titlesOf(node) ++ node.listings.flatMap(listing => IdentityMeasures.pieces(listing.title)).map(IdentityMeasures.key).filter(_.nonEmpty)

  /** How closely a candidate's title names the listing's: whole (or its original), an alternative title, a delimited
   *  piece of it — a rival is a NAMESAKE only when it names the listing at least as closely as the film taken (PL
   *  "ORLANDO - MOJA POLITYCZNA BIOGRAFIA" names Potter's "Orlando" only by a piece, Preciado's film whole), and by
   *  every piece the film taken is named by (DE Acud's "BARWY OCHRONNE / CAMOUFLAGE" names Zanussi's film by both, the
   *  2026 Ukrainian "Camouflage" by one). */
  private def naming(scored: Scored): Int = scored.category("title") match {
    case Some("exact") | Some("original") => Whole
    case Some("alternative")              => 2
    case Some("segment") | Some("decorated") => 1
    case _                                => 0
  }

  /** Is the listing's whole title, which names the film taken whole (`taken`, its [[naming]]), the WORK `rival`'s title
   *  heads before its subtitle — not another instalment of it? Then the listing names the rival by its short title as
   *  closely as the film it took by its whole one: DE CineStar's bare "Lieblingsmenschen" (three venues) took a 65-minute
   *  2021 indie while 23 venues link Hufnagel's 2026 "Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes
   *  & Amir" by their feed's id. "Dune" heads "Dune: Part Two" too, but numbers no instalment of it. */
  private def headedBy(rival: Scored, taken: Int): Boolean =
    taken == Whole && IdentityMeasures.titleIsWorkOf(rival.listing, rival.candidate.film).isDefined &&
      !rival.category("numeral").exists(IdentityMeasures.OtherInstalment)

  /** [[naming]]'s tier of a title naming the film whole: its own title or its original. */
  private val Whole = 3

  /** How many nodes must take a namesake on their own facts for it to stand against a bare node's take. */
  val EvidencedNamesake = 2

  /** The nodes of a cluster that took `film` UNCORROBORATED, with why: no fact of their own corroborates it ([[facts]]),
   *  nor a node of the cluster billed under the same title or joined to such a one by a [[Vouching]] must-link. A take
   *  needs one independent fact; without one the listing stays unmatched, an unmatched listing being far better than a
   *  wrong one (DE Kinopolis Freiberg's bare "Patagonien", a travel lecture, took Bozzelli's 2023 "Patagonia"). The why
   *  names the facts missing and, where the family takes a rival naming the listing as closely ([[naming]]) on more nodes'
   *  facts — and [[EvidencedNamesake]] at least ([[support]]) — that namesake: ES Bahia Mar's bare "ESCAPE ROOM 2" beside
   *  the 2026 film the venues crediting Claramunt take, UK Savoy's "Home Alone (Pyjama Party)" pooled to "Home Alone 3".
   *  Measured 2026-10-15 without the `current release` fact: 46 wrong takes cured, ~2,000 lost (402 judged right) — most
   *  of them a current release billed by its bare title, which that fact now corroborates.
   *
   *  `corroborated` false — a review family's verdict (`agreement.Agreement.verdict`), whose take is that database's VOTE,
   *  not the listing's film — refuses only the node an evidenced namesake disputes: the agreement stage weighs the votes,
   *  and takes a film they agree on only on evidence beside them (`agreement.AgreementStage.corroborated`). Refusing the
   *  votes too took the families' leg from every correction a venue poster makes (PL Kino za Rogiem's bare "Lalka", Has's
   *  1968 film by its poster and IMDb's and Wikidata's takes). */
  def refused(cluster: Seq[EvidenceNode], film: Int, scored: EvidenceNode => Seq[Scored], edges: Seq[ResolverEdge],
              support: Map[Int, Int], corroborated: Boolean = true, familyTitled: Map[Int, Set[String]] = Map.empty): Seq[(EvidenceNode, String)] = {
    val own = cluster.map(node => node -> scored(node).find(_.candidate.tmdbId == film).fold(Seq.empty[String])(facts(node, _, scored(node)))).toMap
    def titles(node: EvidenceNode) = titlesOf(node)
    val ids    = cluster.map(_.id).toSet
    val backing = support.getOrElse(film, 0)
    def namesake(node: EvidenceNode): Option[String] =
      val own    = scored(node).find(_.candidate.tmdbId == film)
      lazy val taken  = own.fold(0)(naming)
      lazy val pieces = own.fold(Set.empty[Seq[String]])(scored => IdentityMeasures.namingPieces(scored.listing, scored.candidate.film))
      // the support lookup first: most rivals no node takes on its own facts, and naming them costs a title walk each
      scored(node).find(rival => rival.candidate.tmdbId != film && {
        val rivalBacking = support.getOrElse(rival.candidate.tmdbId, 0)
        rivalBacking >= EvidencedNamesake && rivalBacking > backing
      } && (naming(rival) > 0 && naming(rival) >= taken &&
        pieces.subsetOf(IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)) || headedBy(rival, taken))).map(rival => s"namesake ${rival.candidate.tmdbId} is taken on their own facts by ${support(rival.candidate.tmdbId)} node(s), $film by $backing")
    // A title vouches only for a node no evidenced namesake shares it with: where the family takes another film its title
    // names as closely (or heads, [[headedBy]]) on more nodes' facts, under any spelling, the title is that film's at least
    // as much (DE "The Killer" ×5,
    // bare, joined to the two venues dating Woo's 2024 remake while seven bill his 1989 film). A catalogue id still does.
    val disputed = cluster.filter(own(_).isEmpty).flatMap(node => namesake(node).map(node.id -> _)).toMap
    val linked = edges.filter(edge => edge.must && Vouching(edge.reason) && ids(edge.a) && ids(edge.b))
      .flatMap(edge => Seq(edge.a -> (edge.b, edge.reason), edge.b -> (edge.a, edge.reason))).groupMap(_._1)(_._2)
    val vouched = scala.collection.mutable.Set.from(cluster.filter(own(_).nonEmpty).map(_.id))
    val titled  = cluster.filter(own(_).nonEmpty).flatMap(titles).toSet
    val evidenced = familyTitled.getOrElse(film, Set.empty)
    cluster.filter(node => !disputed.contains(node.id) && (titles(node).exists(titled) || piecesOf(node).exists(evidenced))).foreach(vouched += _.id)
    var frontier = vouched.toSeq
    while (frontier.nonEmpty) {
      frontier = frontier.flatMap(linked.getOrElse(_, Nil)).collect {
        case (id, reason) if reason == ConstraintEdges.SameCatalogueId || !disputed.contains(id) => id
      }.filter(vouched.add)
    }
    cluster.filterNot(node => vouched(node.id) || !corroborated && !disputed.contains(node.id)).map(node => node ->
      (s"uncorroborated: no year, director, original title, discriminating runtime, feed id, season, house or current release of its own names tmdb $film" +
        disputed.get(node.id).fold("")(why => s", and $why")))
  }
}
