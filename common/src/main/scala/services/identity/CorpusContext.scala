package services.identity

/**
 * What a family's resolve reads from the WHOLE corpus rather than from its own listings — the
 * facts the resolver learns across families:
 *
 *  - `venueTitles`: the titles venues publish for a record, learned from every listing's original
 *    title that names exactly one record ([[IdentityMeasures.venueTitles]]);
 *  - `houseEvidence`: how every node's candidates bill their works, whence each banner's house
 *    ([[IdentityMeasures.Houses]]);
 *  - `titleGroups`: every listing by title key with its venue — a decorated title is backed by
 *    venues listing it plain, whose listings are not title-linked to it ([[IdentityMeasures.VenueBacking]]);
 *  - `wholeTitles`: every node's sanitised whole title (`TitleLinks.titlesBeside`);
 *  - `reachedByTitle`: for each title key, every node carrying it and the candidates its own
 *    evidence reached (`CandidateGeneration.sharedOf`);
 *  - `candidates`: every record any node's answers named, with its venue titles — what a node's
 *    evidence is read against for a film outside its family's pool (`Families.denies`).
 *
 * A family resolved alone against the corpus's context decides exactly as the whole resolve does
 * (A3). This is the seam an incremental model keeps current instead of re-deriving it per resolve.
 */
private[identity] trait CorpusContext {
  /** A record any node's answers named, with the titles venues publish for it. */
  def candidate(id: Int): Option[Candidate]
  /** Which house each listing banner is. */
  def houses: IdentityMeasures.Houses
  /** Each banner's contending houses, as [[houses]] ranked them — for a report reading why a banner is, or is not, a house. */
  def houseRanking: Map[String, Seq[IdentityMeasures.Houses.Contender]]
  /** Every listing carrying title key `key`, with its venue, in node order. */
  def titleGroup(key: String): Seq[(String, IdentityMeasures.Listing)]
  /** Is `sanitised` some node's whole title? */
  def wholeTitle(sanitised: String): Boolean
  /** How many distinct whole titles carry `sanitised` as one of their pieces ([[bannerSegment]]). */
  def segmentSpread(sanitised: String): Int
  /** Is `sanitised` a BANNER shared across films rather than a work: no listing's whole title, and a
   *  piece of [[CorpusContext.BannerSpread]] whole titles or more ("Młode Horyzonty" ×48,
   *  "Splat!FilmFest" ×60)? A banner is no family key (`TitleLinks.titleKeys`). */
  final def bannerSegment(sanitised: String): Boolean = !wholeTitle(sanitised) && segmentSpread(sanitised) >= CorpusContext.BannerSpread
  /** Every node carrying title key `key`, with the candidates its own evidence reached, in node order. */
  def reachedBy(key: String): Seq[(String, Set[Int])]
  /** The films `query`'s answer names, best first; `None` when the answer is not known. */
  def ranked(query: CandidateQuery): Option[Seq[Int]]
  /** The evidence the context already holds for `listing` — one instance kept across resolves, so
   *  what it caches (its measured listing's title forms) is computed once — or `None`. */
  def evidenceOf(listing: Listing): Option[Evidence] = None

  /** A fresh one per resolve (`CandidateScoring` takes one): its memo is not thread-safe, and a
   *  context kept across events would serve a group's backing after the group moved. */
  def backing: IdentityMeasures.VenueBacking = new IdentityMeasures.VenueBacking(CorpusContext.KeyedView(key => Option(titleGroup(key)).filter(_.nonEmpty)))

  /** What of this context a family with `reads` can read — equal slices, equal decisions (A3). */
  def slice(reads: CorpusContext.Reads): CorpusContext.Slice = CorpusContext.Slice(
    reads.titles.map(key => key -> reachedBy(key)).toMap,
    reads.groups.map(key => key -> titleGroup(key)).toMap,
    reads.segments.filter(wholeTitle),
    reads.segments.filter(bannerSegment),
    reads.banners.flatMap(banner => houses.of.get(banner).map(banner -> _)).toMap,
    reads.films.flatMap(id => candidate(id).map(id -> _)).toMap,
    reads.queries.map(query => query -> ranked(query)).toMap)

  /** The candidates the other nodes of `node`'s IDENTICAL title reached by their own evidence that
   *  its own did not (`CandidateGeneration.sharedOf`). */
  def sharedOf(node: EvidenceNode): Set[Int] = {
    val sameTitled = reachedBy(CorpusContext.titleOf(node))
    if (sameTitled.sizeIs < 2) Set.empty
    else {
      val own = sameTitled.collectFirst { case (id, reached) if id == node.id => reached }.getOrElse(Set.empty)
      sameTitled.collect { case (id, reached) if id != node.id => reached }.flatten.toSet -- own
    }
  }
}

/** The context of one resolve's own listings, derived whole from them ([[CorpusContext.of]]). */
private[identity] final class WholeCorpusContext(
  answers:        CandidateQuery => Option[Seq[Int]],
  candidates:     Map[Int, Candidate],
  houseEvidence:  Seq[IdentityMeasures.Billing],
  titleGroups:    Map[String, Seq[(String, IdentityMeasures.Listing)]],
  wholeTitles:    Set[String],
  reachedByTitle: Map[String, Seq[(String, Set[Int])]],
  segmentSpreads: Map[String, Int]
) extends CorpusContext {
  def segmentSpread(sanitised: String): Int = segmentSpreads.getOrElse(sanitised, 0)
  def candidate(id: Int): Option[Candidate] = candidates.get(id)
  lazy val houses: IdentityMeasures.Houses = IdentityMeasures.Houses.learn(houseEvidence)
  lazy val houseRanking: Map[String, Seq[IdentityMeasures.Houses.Contender]] = IdentityMeasures.Houses.ranking(houseEvidence)
  def titleGroup(key: String): Seq[(String, IdentityMeasures.Listing)] = titleGroups.getOrElse(key, Nil)
  def wholeTitle(sanitised: String): Boolean = wholeTitles(sanitised)
  def reachedBy(key: String): Seq[(String, Set[Int])] = reachedByTitle.getOrElse(key, Nil)
  def ranked(query: CandidateQuery): Option[Seq[Int]] = answers(query)
}

private[identity] object CorpusContext {
  def titleOf(node: EvidenceNode): String = IdentityMeasures.key(node.evidence.title)

  /** The records a listing's own title searches found — TMDB's, and IMDb's FIRST suggestion for its title: IMDb
   *  matches a title's other-language spellings TMDB's search lacks ("Camino dla opornych" is Compostelle, which TMDB
   *  holds under no Polish title), and its first suggestion is the title's best match, not a filmography's film. */
  def searchFound(listing: IdentityMeasures.Listing, answers: CandidateQuery => Option[Seq[Int]]): Seq[Int] =
    IdentityMeasures.searchQueries(listing).flatMap(query => answers(CandidateQuery.Title(query)).getOrElse(Nil)) ++
      Seq(listing.title.trim).filter(_.nonEmpty).flatMap(title => answers(CandidateQuery.Imdb(title)).getOrElse(Nil).take(1))

  /** `IdentityMeasures.titlesByFacts` of ONE title key's nodes (their measured listings, by node id), over
   *  the records their own title SEARCHES found — never a director's filmography alone: a director makes
   *  more than one work a year ("SEVENTEEN World Tour 'New_'" {Taehwan Yum} is not his TXT tour film,
   *  "It (1990)" {Tommy Lee Wallace} not his 1991 "And the Sea Will Tell"), while a title search finds a
   *  record by any of its translations. The same inputs whether a whole resolve asks or the live corpus
   *  re-titles the key. */
  def titlesByFacts(listings: Seq[IdentityMeasures.Listing], found: Iterable[Int], records: Int => Option[Candidate]): Seq[(Int, String)] =
    IdentityMeasures.titlesByFacts(listings, found.toSeq.distinct.sorted.flatMap(id => records(id).map(id -> _.film)))

  /** A segment carried by this many distinct whole titles, and by none as its own whole title, is a
   *  banner ([[CorpusContext.bannerSegment]]): PL's gluing programme and festival banners span 11–60
   *  titles, a work's spellings across banners a handful. */
  val BannerSpread = 8

  /** A node's title pieces other than its whole title, sanitised: what [[CorpusContext.segmentSpread]] counts. */
  def piecesOf(node: EvidenceNode, sanitize: String => String): Set[String] = {
    val whole = sanitize(node.evidence.cleanTitle)
    IdentityMeasures.titleShapes(node.evidence.published).map(sanitize).filter(piece => piece.nonEmpty && piece != whole).toSet
  }
  /** Each piece's number of distinct whole titles among `nodes`. */
  def spreads(nodes: Seq[EvidenceNode], sanitize: String => String): Map[String, Int] =
    nodes.flatMap(node => piecesOf(node, sanitize).map(_ -> sanitize(node.evidence.cleanTitle))).groupMap(_._1)(_._2)
      .map { case (piece, wholes) => piece -> wholes.distinct.size }

  /** Every key of the context a family's resolve can read — a superset, so a family whose slice is
   *  unchanged is certain to decide as before: its nodes' title keys (`reachedByTitle`), their title
   *  groups (`titleGroups`, and their search forms' `searchGroupsAny`), their titles' sanitised shapes (`wholeTitles`), every banner a node bills
   *  a pool film's work under (`houses`), and every film its answers named or it decided. */
  final case class Reads(titles: Set[String], groups: Set[String], segments: Set[String], banners: Set[String], films: Set[Int],
                         queries: Set[CandidateQuery]) {
    /** The keys [[Changed.keys]] names them by — a title group is filed under its title key. */
    def keys: Set[String] = (titles ++ groups).map("t:" + _) ++ segments.map("s:" + _) ++ banners.map("b:" + _) ++ films.map("f:" + _)
  }
  object Reads {
    def of(members: Seq[EvidenceNode], pool: Seq[IdentityMeasures.Film], films: Set[Int], queries: Set[CandidateQuery],
           sanitize: String => String): Reads = Reads(
      members.map(titleOf).toSet,
      members.flatMap(node => IdentityMeasures.titleGroups(node.evidence.measured) ++ IdentityMeasures.searchGroupsAny(node.evidence.measured)).toSet,
      members.flatMap(node => IdentityMeasures.titleShapes(node.evidence.published).map(sanitize)).toSet,
      members.flatMap(node => pool.flatMap(film => IdentityMeasures.billings(node.evidence.measured, film).map(_.listingHouse))).toSet,
      films, queries)
  }

  /** The keys an event moved facts under: title keys (their nodes' reached candidates and title
   *  groups), whole-title shapes, banners, and films whose candidate changed. A family whose
   *  [[Reads]] touch none of them reads exactly what it read before. */
  final case class Changed(titles: Set[String], segments: Set[String], banners: Set[String], films: Set[Int]) {
    def keys: Set[String] = titles.map("t:" + _) ++ segments.map("s:" + _) ++ banners.map("b:" + _) ++ films.map("f:" + _)
    def ++(other: Changed): Changed = Changed(titles ++ other.titles, segments ++ other.segments, banners ++ other.banners, films ++ other.films)
  }
  object Changed { val None: Changed = Changed(Set.empty, Set.empty, Set.empty, Set.empty) }

  /** The values a family read, as [[CorpusContext.slice]] cut them. */
  final case class Slice(reached: Map[String, Seq[(String, Set[Int])]], groups: Map[String, Seq[(String, IdentityMeasures.Listing)]],
                         wholeTitles: Set[String], bannerSegments: Set[String], houses: Map[String, String], candidates: Map[Int, Candidate],
                         answers: Map[CandidateQuery, Option[Seq[Int]]]) {
    /** 64 bits of the slice's content — what a stored family keeps to tell, after a restart, whether
     *  it still reads what it read ([[ContentHash]]). */
    def digest: Long = ContentHash.of(this)
  }

  /** A map that only answers lookups by key — what `VenueBacking` asks of its groups — over a
   *  context that keeps them per key rather than as one map. */
  final case class KeyedView[K, V](lookup: K => Option[V]) extends scala.collection.immutable.AbstractMap[K, V] {
    def get(key: K): Option[V] = lookup(key)
    def iterator: Iterator[(K, V)] = throw new UnsupportedOperationException("a keyed view answers lookups only")
    def removed(key: K): Map[K, V] = throw new UnsupportedOperationException("a keyed view answers lookups only")
    def updated[V1 >: V](key: K, value: V1): Map[K, V1] = throw new UnsupportedOperationException("a keyed view answers lookups only")
  }

  /** Every recorded season production, by the (work, season) pairs its titles name. */
  def seasonProductions(films: Iterable[(Int, IdentityMeasures.Film)]): Map[(String, Int), Set[Int]] =
    films.toSeq.flatMap { case (id, film) => IdentityMeasures.seasonWorks(film).map(_ -> id) }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  /** The films a node's banner is billed against (`Houses.evidence`): the ones it reached, then —
   *  a listing naming a season — every recorded production of its work that season, whoever searched
   *  it up. PL Kino 1410's "Cosi fan tutte | metropolitan opera: live in hd 2026/27" reached only
   *  Royal Ballet & Opera's; the Met's record, another venue's hit, is what names its banner's house. */
  def billedFilms(reached: Seq[Int], listing: IdentityMeasures.Listing, seasonFilms: ((String, Int)) => Iterable[Int]): Seq[Int] = {
    val own = reached.toSet
    reached ++ IdentityMeasures.seasonWorks(listing).toSeq.flatMap(seasonFilms).distinct.sorted.filterNot(own)
  }

  /** The context of exactly `nodes`: what a whole resolve of them reads. `reached` is each node's
   *  own candidates (its searches' and walks'), `recorded` every record their answers named. */
  def of(nodes: Seq[EvidenceNode], reached: EvidenceNode => Seq[Int], recorded: Map[Int, Candidate],
         answers: CandidateQuery => Option[Seq[Int]], sanitize: String => String): CorpusContext = {
    // a fallback source's film is a candidate only a no-match's fallback scores, never a record the corpus's titles,
    // seasons or houses are read off ([[FallbackIds]])
    val corpusRecords = recorded.filter { case (id, _) => !FallbackIds.isFallback(id) }
    val byOriginal  = IdentityMeasures.venueTitles(nodes.map(_.evidence.measured), corpusRecords.toSeq.sortBy(_._1).map { case (id, candidate) => id -> candidate.film })
    val byFacts     = nodes.groupBy(titleOf).toSeq.flatMap { case (_, keyed) =>
      val sorted = keyed.sortBy(_.id)
      val found  = sorted.flatMap(node => searchFound(node.evidence.measured, answers))
      titlesByFacts(sorted.map(_.evidence.measured), found, recorded.get) }
    val venueTitles = (byOriginal.toSeq.flatMap { case (id, titles) => titles.map(id -> _) } ++ byFacts)
      .groupMap(_._1)(_._2).map { case (id, titles) => id -> titles.distinct.sorted }
    val candidates  = recorded.map { case (id, candidate) =>
      id -> candidate.copy(film = IdentityMeasures.withVenueTitles(candidate.film, venueTitles.getOrElse(id, Nil))) }
    val seasonFilms   = seasonProductions(candidates.collect { case (id, candidate) if !FallbackIds.isFallback(id) => id -> candidate.film })
    val houseEvidence = nodes.flatMap { node =>
      val listing = node.evidence.measured
      IdentityMeasures.Houses.evidence(listing, billedFilms(reached(node), listing, seasonFilms.getOrElse(_, Set.empty)).map(candidates(_).film)) }
    val titleGroups = nodes.flatMap(node => node.listings.map(listing => IdentityMeasures.key(node.evidence.title) -> (listing.venue -> node.evidence.measured)))
      .groupMap(_._1)(_._2)
    val wholeTitles = nodes.map(node => sanitize(node.evidence.cleanTitle)).filter(_.nonEmpty).toSet
    val reachedByTitle = nodes.groupBy(titleOf).filter(_._1.nonEmpty).map { case (title, sameTitled) =>
      title -> sameTitled.map(node => node.id -> reached(node).toSet) }
    new WholeCorpusContext(answers, candidates, houseEvidence, titleGroups, wholeTitles, reachedByTitle, spreads(nodes, sanitize))
  }
}
