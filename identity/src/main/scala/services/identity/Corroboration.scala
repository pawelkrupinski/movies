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
 *  - `director`: a director it credits is the film's — by its own credit, a banner, or its own text naming them whole
 *    ([[CreditMeasures.creditedByText]]: ES taquilla's "Hope" ×56, "su directora Maria Sødahl", beside Na Hong-jin's
 *    2026 "Hope" taken by 4 nodes' facts);
 *  - `original title`: an original title of its own (not its title restated) that is the film's;
 *  - `runtime`: a running time within [[RuntimeBand]] minutes of the film's that no namesake's fits as well — 90 minutes
 *    fit both "Escape Room 2"s, so they tell neither apart; nor does a time longer than a namesake's, a slot's adverts
 *    and introduction ([[fits]]);
 *  - `feed id`: a TMDB-keyed feed's id linking it to `f` (`linked`: [[FamilyScope.feedLinked]], none where the resolve's
 *    films are a family's own numbers);
 *  - `season`: a stage relay's screening days placing it in the season its record names ([[Scored.seasonProduction]]);
 *  - `cast`: the venue's own cast field ([[ListedCast]]) names [[CastEvidence.Names]] of the film's top-billed cast, no
 *    namesake's being named as often, and the field is the film's ([[CastEvidence.castIsTheFilms]]: no relay, stage
 *    work, performance or bill of several works lists its performers there).
 *
 *  A node with none still takes the film when another node of its cluster billed under the SAME title corroborates it on
 *  its own — its title, clean title or search title alike — or joined to such a node by a [[Vouching]] must-link (US
 *  Showcase's chain id joins "El laberinto del fauno" to its credited "Pan's Labyrinth"): a bare "Frankenstein" beside
 *  the venues crediting del Toro is the cluster's film, not a guess — unless the family takes an evidenced namesake under
 *  that very title on more nodes' facts, when only a catalogue id vouches. A node with neither is decided apart, as no film
 *  ([[IdentityResolver]]), only where its take is wrong-shaped ([[refused]]): a fact of its own against the film, or a
 *  namesake its family's corroborated nodes take more often. */
private[identity] object Corroboration {

  /** Minutes a venue's running time may stand off the film's and still corroborate it: trailers and rounding. */
  val RuntimeBand = 10

  /** Does the venue's running time fit `rival` as a namesake: within [[RuntimeBand]] of it, or LONGER — a slot runs the
   *  film and more (adverts, trailers, an intro), so a longer one says nothing against it ([[agreement.Agreement.runsShort]]).
   *  UK Mowlem's "Purbeck Film Festival - Hen" at 110 minutes fits Pálfi's 96-minute "Hen" as well as the 108-minute
   *  Afrikaans one: its runtime tells neither apart (convergence #792). */
  def fits(rival: Scored): Boolean =
    rival.number("runtime.delta").exists(delta => math.abs(delta) <= RuntimeBand || delta > 0)

  /** The facts by which `own` — the node's scoring of the film — corroborates it, `rivals` being the node's other
   *  scored candidates, by name; empty when none does. */
  def facts(node: EvidenceNode, own: Scored, rivals: Seq[Scored], linked: EvidenceNode => Option[Int]): Seq[String] = {
    val (agree, _) = IdentityMeasures.ownAgreement(own.measures)
    val original   = agree("originalTitle") && node.listings.exists(listing => listing.originalTitle.exists(original =>
      IdentityMeasures.key(original) != IdentityMeasures.key(listing.title) && IdentityMeasures.key(original) != IdentityMeasures.key(listing.rawTitle)))
    def inBand(scored: Scored) = IdentityMeasures.runtimeGap(scored.measures).exists(_ <= RuntimeBand)
    val runtime    = inBand(own) && !rivals.exists(rival => (rival ne own) && rival.candidate.tmdbId != own.candidate.tmdbId && rival.titleNamesIt && fits(rival))
    val feedId     = linked(node).contains(own.candidate.tmdbId)
    val season     = own.seasonProduction || own.number("season.delta").contains(0.0)
    def castNamed(scored: Scored) = scored.candidate.film.cast.exists(cast => node.listings.exists(listing =>
      listing.listedCast.nonEmpty && cast.namedBy(listing.listedCast) >= CastEvidence.Names))
    val cast       = castNamed(own) && CastEvidence.castIsTheFilms(node.listings) &&
      !rivals.exists(rival => (rival ne own) && rival.candidate.tmdbId != own.candidate.tmdbId && rival.titleNamesIt && castNamed(rival))
    Seq("year" -> agree("year"), "director" -> agree("director"), "original title" -> original, "runtime" -> runtime, "feed id" -> feedId,
      "season" -> season, "cast" -> cast)
      .collect { case (name, true) => name }
  }

  /** The must-links that vouch a node to a corroborated one: its title, clean or search title, catalogue id or a
   *  delimited segment of the other's title — never the original title alone, which a feed copies from the entry it
   *  linked and so pulls in that entry's namesakes (DE CITY 46's "THE BLACK HOLE", its copied "The Black Hole" beside
   *  "Das schwarze Loch"). */
  val Vouching: Set[String] = Set(ConstraintEdges.SameTitle, ConstraintEdges.SameCatalogueId, ConstraintEdges.SameSearchForm, ConstraintEdges.TitleSegment)

  /** How many nodes of `clusters` take each film on a fact of their own ([[facts]]) — a film's standing in its title
   *  family, against which a bare node's namesake take is weighed ([[refused]]). `filmOf` is a cluster's film. */
  def support(clusters: Seq[Seq[EvidenceNode]], filmOf: Seq[EvidenceNode] => Option[Int], scored: EvidenceNode => Seq[Scored],
              linked: EvidenceNode => Option[Int]): Map[Int, Int] =
    clusters.flatMap(cluster => filmOf(cluster).toSeq.flatMap(film => cluster.filter(node =>
      scored(node).find(_.candidate.tmdbId == film).exists(facts(node, _, scored(node), linked).nonEmpty)).map(_ => film))).groupMapReduce(identity)(_ => 1)(_ + _)

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
   *  & Amir" by their feed's id. "Dune" heads "Dune: Part Two" too, but numbers no instalment of it. A head names the
   *  rival only by its work, so it stands only against a take no node evidences (`backing`): beside a node dating the
   *  2009 "Avatar", a bare "Avatar" keeps the re-release though two nodes take "Avatar: Fire and Ash" on their own facts. */
  private def headedBy(rival: Scored, taken: Int, backing: Int): Boolean =
    backing == 0 && taken == Whole && IdentityMeasures.titleIsWorkOf(rival.listing, rival.candidate.film).isDefined &&
      !rival.category("numeral").exists(IdentityMeasures.OtherInstalment)

  /** [[naming]]'s tier of a title naming the film whole: its own title or its original. */
  private val Whole = 3

  /** How many nodes must take a namesake on their own facts for it to stand against a bare node's take. */
  val EvidencedNamesake = 2

  /** The nodes of a cluster that took `film` WRONG-SHAPED, with why: no fact of their own corroborates it, nor a node of
   *  the cluster billed under the same title or joined to such a one by a [[Vouching]] must-link, while a rival naming the
   *  listing as closely ([[naming]]) is taken on their own facts by more nodes of the family than `film` is, and by
   *  [[EvidencedNamesake]] at least
   *  ([[support]]): ES Bahia Mar's bare "ESCAPE ROOM 2" beside the 2026 film the venues crediting Claramunt take, UK
   *  Savoy's "Home Alone (Pyjama Party)" pooled to "Home Alone 3". A bare title nothing else speaks against keeps its
   *  film: a current release billed by its exact title at a venue publishing nothing else is that release far more often
   *  than not (measured 2026-10-15: refusing every uncorroborated take cured 29 wrong ones and lost ~2,000, 399 of them
   *  judged right). Neither is a fact of its own against the film enough: a feed's link of another film is often the
   *  operator's wrong pick (DE Cineamo's "Das Piano", "Persona", "Rent"), and TMDB's credits often name another director
   *  than the venue (PL "Tedi i magiczna lampa"). */
  def refused(cluster: Seq[EvidenceNode], film: Int, scored: EvidenceNode => Seq[Scored], linked: EvidenceNode => Option[Int],
              edges: Seq[ResolverEdge], support: Map[Int, Int]): Seq[(EvidenceNode, String)] = {
    val own = cluster.map(node => node -> scored(node).find(_.candidate.tmdbId == film).fold(Seq.empty[String])(facts(node, _, scored(node), linked))).toMap
    def titles(node: EvidenceNode) = node.listings.flatMap(listing => Seq(listing.title, listing.cleanTitle) ++ listing.searchTitle).map(IdentityMeasures.key).toSet
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
        pieces.subsetOf(IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)) || headedBy(rival, taken, backing))).map(rival => s"namesake ${rival.candidate.tmdbId} is taken on their own facts by ${support(rival.candidate.tmdbId)} node(s), $film by $backing")
    // A title vouches only for a node no evidenced namesake shares it with: where the family takes another film its title
    // names as closely (or heads, [[headedBy]]) on more nodes' facts, under any spelling, the title is that film's at least
    // as much (DE "The Killer" ×5,
    // bare, joined to the two venues dating Woo's 2024 remake while seven bill his 1989 film). A catalogue id still does.
    val disputed = cluster.filter(own(_).isEmpty).flatMap(node => namesake(node).map(node.id -> _)).toMap
    val joined = edges.filter(edge => edge.must && Vouching(edge.reason) && ids(edge.a) && ids(edge.b))
      .flatMap(edge => Seq(edge.a -> (edge.b, edge.reason), edge.b -> (edge.a, edge.reason))).groupMap(_._1)(_._2)
    val vouched = scala.collection.mutable.Set.from(cluster.filter(own(_).nonEmpty).map(_.id))
    val titled  = cluster.filter(own(_).nonEmpty).flatMap(titles).toSet
    cluster.filter(node => !disputed.contains(node.id) && titles(node).exists(titled)).foreach(vouched += _.id)
    var frontier = vouched.toSeq
    while (frontier.nonEmpty) {
      frontier = frontier.flatMap(joined.getOrElse(_, Nil)).collect {
        case (id, reason) if reason == ConstraintEdges.SameCatalogueId || !disputed.contains(id) => id
      }.filter(vouched.add)
    }
    cluster.filterNot(node => vouched(node.id)).flatMap(node => disputed.get(node.id).map(why =>
      node -> s"uncorroborated namesake: no year, director, original title, discriminating runtime, feed id, season or cast of its own names tmdb $film, and $why"))
  }
}
