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
  lookups.prefetch(Nil, Nil, ordered.filter(listing => corpus.flatMap(_.evidenceOf(listing)).isEmpty && lookups.hasDetail(listing)))
  val withEvidence = ordered.map(listing => listing -> corpus.flatMap(_.evidenceOf(listing)).getOrElse(Evidence.of(listing, detailOf(listing), decorations)))
  // A pinned listing is a node of its own kind: identical evidence under different pins is not
  // one question any more.
  val nodes = withEvidence.groupBy { case (listing, evidence) => CandidateGeneration.nodeKey(listing, evidence, pins) }.values.toSeq
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
  } else {
    val asked = queriesOf.values.flatten.toSeq.distinct.sorted
    lookups.prefetch(asked, Nil, Nil)
    asked.foreach(ask)
  }

  val ownSearch: Map[String, Map[Int, Int]] = nodes.map(node => node.id -> CandidateGeneration.ownSearch(queriesOf(node.id), answers)).toMap
  val ownWalk: Map[String, Set[Int]]        = nodes.map(node => node.id -> CandidateGeneration.ownWalk(queriesOf(node.id), answers)).toMap
  val hitsById = nodes.flatMap(node => queriesOf(node.id).flatMap(query => answers(query).toOption.getOrElse(Nil))).groupBy(_.tmdbId)
  // Looked up only when these listings are the whole corpus: a region reads its candidates from
  // the corpus's context, which holds every record already.
  lazy val records  = {
    val ids = hitsById.keys.toSeq.sorted
    lookups.prefetch(Nil, ids, Nil)
    ids.map(id => id -> lookups.film(id)).toMap
  }
  lazy val recorded: Map[Int, Candidate] = hitsById.map { case (id, hits) => id -> Candidate.of(id, hits, records(id).toOption.flatten) }
  /** Whether these listings are part of a larger corpus, read through its context. */
  def partOfCorpus: Boolean = corpus.isDefined
  /** Every candidate a node's own evidence reached: its searches' and its walks'. */
  def reached(node: EvidenceNode): Seq[Int] = CandidateGeneration.reached(ownSearch(node.id), ownWalk(node.id))

  /** What these listings' families read from the whole corpus: `corpus` when they are part of a
   *  larger one, else their own. */
  lazy val context: CorpusContext = corpus.getOrElse(CorpusContext.of(nodes, reached, recorded,
    query => answers.get(query).flatMap(_.toOption).map(CandidateGeneration.ranked), normalizer.sanitize))
  // One instance of each record for this resolve's whole length, whatever the context hands out: the
  // title forms a record caches are computed once per resolve, not once per node that scores it.
  private val held = mutable.HashMap.empty[Int, Option[Candidate]]
  /** A record with the titles the venues publish for it (`IdentityMeasures.venueTitles`). */
  def candidateOf(id: Int): Option[Candidate] = held.getOrElseUpdate(id, context.candidate(id).orElse(filedSince(id)))
  /** A record this resolve's own answers reach that the corpus's context does not hold yet: an
   *  answer filed after the context last heard of it (the fill files while the model drains). Its
   *  event is queued, and re-resolves the family with the context that knows it; until then the
   *  record is read from this resolve's own hits. */
  private def filedSince(id: Int): Option[Candidate] =
    if (corpus.isEmpty) None else hitsById.get(id).map(hits => Candidate.of(id, hits, lookups.film(id).toOption.flatten))
  /** A record some node reached: every one is known to the context, or filed since it last heard. */
  def candidateById(id: Int): Candidate = candidateOf(id).getOrElse(throw new NoSuchElementException(s"no candidate $id in the corpus context"))
  /** The candidates the nodes of a node's IDENTICAL title (`IdentityMeasures.key`) reached by their
   *  own evidence — a credited director, a detail page's original title — that its own did not:
   *  "Vincent. Legenda oceanu" at a venue publishing nothing else searches empty (TMDB titles the
   *  film "The Last Whale Singer"), while the same title credited elsewhere walks its director to
   *  it. A path to a candidate, never evidence for it: the node scores it on its own facts (no
   *  search rank), and still denies it when they rule it out. */
  def sharedOf(node: EvidenceNode): Set[Int] = context.sharedOf(node)
}

private[identity] object CandidateGeneration {
  /** Which node a listing is one of: identical evidence, and — a pinned listing being a node of
   *  its own kind — the same pins. */
  def nodeKey(listing: Listing, evidence: Evidence, pins: PinConstraints): (String, Set[String]) = (evidence.key, pins.blockKeys(listing.key))

  private val isTitle: CandidateQuery => Boolean = { case _: CandidateQuery.Title => true; case _ => false }
  /** Each candidate a node's own title searches named, at the best (1-based) rank any gave it. */
  def ownSearch(queries: Seq[CandidateQuery], answer: CandidateQuery => Answer[Seq[Hit]]): Map[Int, Int] =
    queries.filter(isTitle).flatMap(query => answer(query).toOption.getOrElse(Nil).zipWithIndex).groupMapReduce(_._1.tmdbId)(_._2 + 1)(math.min)
  /** Each candidate a node's other paths reached: its credited directors' filmographies, and the
   *  films IMDb lists under its title (found by their IMDb ids) — paths, never a search rank. */
  def ownWalk(queries: Seq[CandidateQuery], answer: CandidateQuery => Answer[Seq[Hit]]): Set[Int] =
    queries.filterNot(isTitle).flatMap(query => answer(query).toOption.getOrElse(Nil)).map(_.tmdbId).toSet
  /** Every candidate a node's own evidence reached: its searches' and its walks'. */
  def reached(ownSearch: Map[Int, Int], ownWalk: Set[Int]): Seq[Int] = (ownSearch.keys ++ ownWalk).toSeq.distinct.sorted
  /** The films an answer names, best first. */
  def ranked(hits: Seq[Hit]): Seq[Int] = hits.map(_.tmdbId).distinct
  /** A node's key as text: its evidence's, and its pins'. */
  def nodeKeyText(key: (String, Set[String])): String = (key._1 +: key._2.toSeq.sorted).mkString("\u0000")
}
