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
private[identity] final class CorpusContext(
  val venueTitles:    Map[Int, Seq[String]],
  val candidates:     Map[Int, Candidate],
  val houseEvidence:  Seq[IdentityMeasures.Billing],
  val titleGroups:    Map[String, Seq[(String, IdentityMeasures.Listing)]],
  val wholeTitles:    Set[String],
  val reachedByTitle: Map[String, Seq[(String, Set[Int])]]
) {
  /** Which house each listing banner is. */
  lazy val houses: IdentityMeasures.Houses = IdentityMeasures.Houses.learn(houseEvidence)
  /** Each banner's contending houses, as [[houses]] ranked them — for a report reading why a banner is, or is not, a house. */
  lazy val houseRanking: Map[String, Seq[IdentityMeasures.Houses.Contender]] = IdentityMeasures.Houses.ranking(houseEvidence)
  /** One per resolve: its memo is not thread-safe. */
  lazy val backing: IdentityMeasures.VenueBacking = new IdentityMeasures.VenueBacking(titleGroups)

  /** What of this context a family with `reads` can read — equal slices, equal decisions (A3). */
  def slice(reads: CorpusContext.Reads): CorpusContext.Slice = CorpusContext.Slice(
    reachedByTitle.filter { case (title, _) => reads.titles(title) },
    titleGroups.filter { case (group, _) => reads.groups(group) },
    wholeTitles intersect reads.segments,
    houses.of.filter { case (banner, _) => reads.banners(banner) },
    candidates.filter { case (id, _) => reads.films(id) })

  /** The candidates the other nodes of `node`'s IDENTICAL title reached by their own evidence that
   *  its own did not (`CandidateGeneration.sharedOf`). */
  def sharedOf(node: EvidenceNode): Set[Int] = {
    val sameTitled = reachedByTitle.getOrElse(CorpusContext.titleOf(node), Nil)
    if (sameTitled.sizeIs < 2) Set.empty
    else {
      val own = sameTitled.collectFirst { case (id, reached) if id == node.id => reached }.getOrElse(Set.empty)
      sameTitled.collect { case (id, reached) if id != node.id => reached }.flatten.toSet -- own
    }
  }
}

private[identity] object CorpusContext {
  def titleOf(node: EvidenceNode): String = IdentityMeasures.key(node.evidence.title)

  /** Every key of the context a family's resolve can read — a superset, so a family whose slice is
   *  unchanged is certain to decide as before: its nodes' title keys (`reachedByTitle`), their title
   *  groups (`titleGroups`), their titles' sanitised shapes (`wholeTitles`), every banner a node bills
   *  a pool film's work under (`houses`), and every film its answers named or it decided. */
  final case class Reads(titles: Set[String], groups: Set[String], segments: Set[String], banners: Set[String], films: Set[Int])
  object Reads {
    def of(members: Seq[EvidenceNode], pool: Seq[IdentityMeasures.Film], films: Set[Int], sanitize: String => String): Reads = Reads(
      members.map(titleOf).toSet,
      members.flatMap(node => IdentityMeasures.titleGroups(node.evidence.measured)).toSet,
      members.flatMap(node => IdentityMeasures.titleShapes(node.evidence.published).map(sanitize)).toSet,
      members.flatMap(node => pool.flatMap(film => IdentityMeasures.billings(node.evidence.measured, film).map(_.listingHouse))).toSet,
      films)
  }

  /** The values a family read, as [[CorpusContext.slice]] cut them. */
  final case class Slice(reached: Map[String, Seq[(String, Set[Int])]], groups: Map[String, Seq[(String, IdentityMeasures.Listing)]],
                         wholeTitles: Set[String], houses: Map[String, String], candidates: Map[Int, Candidate])

  /** The context of exactly `nodes`: what a whole resolve of them reads. `reached` is each node's
   *  own candidates (its searches' and walks'), `recorded` every record their answers named. */
  def of(nodes: Seq[EvidenceNode], reached: EvidenceNode => Seq[Int], recorded: Map[Int, Candidate],
         sanitize: String => String): CorpusContext = {
    val venueTitles = IdentityMeasures.venueTitles(nodes.map(_.evidence.measured), recorded.toSeq.sortBy(_._1).map { case (id, candidate) => id -> candidate.film })
    val candidates  = recorded.map { case (id, candidate) =>
      id -> candidate.copy(film = IdentityMeasures.withVenueTitles(candidate.film, venueTitles.getOrElse(id, Nil))) }
    val houseEvidence = nodes.flatMap(node => IdentityMeasures.Houses.evidence(node.evidence.measured, reached(node).map(candidates(_).film)))
    val titleGroups = nodes.flatMap(node => node.listings.map(listing => IdentityMeasures.key(node.evidence.title) -> (listing.venue -> node.evidence.measured)))
      .groupMap(_._1)(_._2)
    val wholeTitles = nodes.map(node => sanitize(node.evidence.cleanTitle)).filter(_.nonEmpty).toSet
    val reachedByTitle = nodes.groupBy(titleOf).filter(_._1.nonEmpty).map { case (title, sameTitled) =>
      title -> sameTitled.map(node => node.id -> reached(node).toSet) }
    new CorpusContext(venueTitles, candidates, houseEvidence, titleGroups, wholeTitles, reachedByTitle)
  }
}
