package services.identity

import services.movies.TitleNormalizer

import scala.collection.mutable

/** Stage A of [[IdentityResolver]]: each listing's own detail page merged into its [[Evidence]],
 *  listings with identical evidence as one [[EvidenceNode]], every node's [[CandidateQueries]]
 *  asked — all of them, up front, in sorted order, none conditional on another's answer — and
 *  every film any answer names looked up once. `ordered` is the listings in the order they are
 *  walked; `lazyLookups` is the teeth tests' `Mutation.LazyLookups`. */
private[identity] final class CandidateGeneration(ordered: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                                  pins: PinConstraints, decorations: TitleDecorations, lazyLookups: Boolean,
                                                  corpus: Option[CorpusContext] = None) {
  val details = mutable.LinkedHashMap.empty[(String, String), Answer[Option[DetailFacts]]]
  def detailOf(listing: Listing): Option[DetailFacts] =
    if (!lookups.hasDetail(listing)) None
    else details.getOrElseUpdate((listing.venue, listing.page.getOrElse("")), lookups.detail(listing)).toOption.flatten
  val withEvidence = ordered.map(listing => listing -> Evidence.of(listing, detailOf(listing), decorations))
  // A pinned listing is a node of its own kind: identical evidence under different pins is not
  // one question any more.
  val nodes = withEvidence.groupBy { case (listing, evidence) => (evidence.key, pins.blockKeys(listing.key)) }.values.toSeq
    .map(group => new EvidenceNode(group.head._2, group.map(_._1).sorted))
    .sortBy(_.id)
  val nodeById = nodes.map(node => node.id -> node).toMap

  val queriesOf: Map[String, Seq[CandidateQuery]] = nodes.map(node => node.id -> CandidateQueries.of(node.evidence)).toMap
  val answers = mutable.HashMap.empty[CandidateQuery, Answer[Seq[Hit]]]
  val issued  = mutable.ArrayBuffer.empty[CandidateQuery]
  def ask(query: CandidateQuery): Answer[Seq[Hit]] = answers.getOrElseUpdate(query, { issued += query; lookups.candidates(query) })
  if (lazyLookups) {
    val answeredShapes = mutable.HashSet.empty[String]
    val arrivalNodes = ordered.flatMap(listing => nodes.find(_.listings.contains(listing))).distinct
    arrivalNodes.foreach(node => queriesOf(node.id).foreach {
      case query @ CandidateQuery.Title(text) =>
        val shape = normalizer.sanitize(text)
        if (!answeredShapes(shape)) { if (ask(query).toOption.exists(_.nonEmpty)) answeredShapes += shape }
        else answers.getOrElseUpdate(query, Answer.Known(Nil))
      case query => ask(query)
    })
  } else queriesOf.values.flatten.toSeq.distinct.sorted.foreach(ask)

  val isTitle: CandidateQuery => Boolean = { case _: CandidateQuery.Title => true; case _ => false }
  // Each candidate a node's own title searches named, at the best (1-based) rank any gave it.
  val ownSearch: Map[String, Map[Int, Int]] = nodes.map { node =>
    node.id -> queriesOf(node.id).filter(isTitle).flatMap(query => answers(query).toOption.getOrElse(Nil).zipWithIndex)
      .groupMapReduce(_._1.tmdbId)(_._2 + 1)(math.min)
  }.toMap
  // Each candidate a node's other paths reached: its credited directors' filmographies, and the
  // films IMDb lists under its title (found by their IMDb ids) — paths, never a search rank.
  val ownWalk: Map[String, Set[Int]] = nodes.map(node =>
    node.id -> queriesOf(node.id).filterNot(isTitle).flatMap(query => answers(query).toOption.getOrElse(Nil)).map(_.tmdbId).toSet).toMap
  val hitsById = nodes.flatMap(node => queriesOf(node.id).flatMap(query => answers(query).toOption.getOrElse(Nil))).groupBy(_.tmdbId)
  val records  = hitsById.keys.toSeq.sorted.map(id => id -> lookups.film(id)).toMap
  val recorded: Map[Int, Candidate] = hitsById.map { case (id, hits) => id -> Candidate.of(id, hits, records(id).toOption.flatten) }
  /** Every candidate a node's own evidence reached: its searches' and its walks'. */
  def reached(node: EvidenceNode): Seq[Int] = (ownSearch(node.id).keys ++ ownWalk(node.id)).toSeq.distinct.sorted

  /** What these listings' families read from the whole corpus: `corpus` when they are part of a
   *  larger one, else their own. */
  lazy val context: CorpusContext = corpus.getOrElse(CorpusContext.of(nodes, reached, recorded, normalizer.sanitize))
  /** Each record with the titles the venues publish for it (`IdentityMeasures.venueTitles`). */
  lazy val candidateById: Map[Int, Candidate] = context.candidates
  /** The candidates the nodes of a node's IDENTICAL title (`IdentityMeasures.key`) reached by their
   *  own evidence — a credited director, a detail page's original title — that its own did not:
   *  "Vincent. Legenda oceanu" at a venue publishing nothing else searches empty (TMDB titles the
   *  film "The Last Whale Singer"), while the same title credited elsewhere walks its director to
   *  it. A path to a candidate, never evidence for it: the node scores it on its own facts (no
   *  search rank), and still denies it when they rule it out. */
  def sharedOf(node: EvidenceNode): Set[Int] = context.sharedOf(node)
}
