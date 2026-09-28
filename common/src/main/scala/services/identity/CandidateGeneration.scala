package services.identity

import services.movies.TitleNormalizer

import scala.collection.mutable

/** Stage A of [[IdentityResolver]]: each listing's own detail page merged into its [[Evidence]],
 *  listings with identical evidence as one [[EvidenceNode]], every node's [[CandidateQueries]]
 *  asked — all of them, up front, in sorted order, none conditional on another's answer — and
 *  every film any answer names looked up once. `ordered` is the listings in the order they are
 *  walked; `lazyLookups` is the teeth tests' `Mutation.LazyLookups`. */
private[identity] final class CandidateGeneration(ordered: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                                  pins: PinConstraints, decorations: TitleDecorations, lazyLookups: Boolean) {
  val details = mutable.LinkedHashMap.empty[(String, String), Answer[Option[DetailFacts]]]
  def detailOf(l: Listing): Option[DetailFacts] =
    if (!lookups.hasDetail(l)) None
    else details.getOrElseUpdate((l.venue, l.page.getOrElse("")), lookups.detail(l)).toOption.flatten
  val withEvidence = ordered.map(l => l -> Evidence.of(l, detailOf(l), decorations))
  // A pinned listing is a node of its own kind: identical evidence under different pins is not
  // one question any more.
  val nodes = withEvidence.groupBy { case (l, e) => (e.key, pins.blockKeys(l.key)) }.values.toSeq
    .map(g => new EvidenceNode(g.head._2, g.map(_._1).sorted))
    .sortBy(_.id)
  val nodeById = nodes.map(n => n.id -> n).toMap

  val queriesOf: Map[String, Seq[CandidateQuery]] = nodes.map(n => n.id -> CandidateQueries.of(n.evidence)).toMap
  val answers = mutable.HashMap.empty[CandidateQuery, Answer[Seq[Hit]]]
  val issued  = mutable.ArrayBuffer.empty[CandidateQuery]
  def ask(q: CandidateQuery): Answer[Seq[Hit]] = answers.getOrElseUpdate(q, { issued += q; lookups.candidates(q) })
  if (lazyLookups) {
    val answeredShapes = mutable.HashSet.empty[String]
    val arrivalNodes = ordered.flatMap(l => nodes.find(_.listings.contains(l))).distinct
    arrivalNodes.foreach(n => queriesOf(n.id).foreach {
      case q @ CandidateQuery.Title(text) =>
        val shape = normalizer.sanitize(text)
        if (!answeredShapes(shape)) { if (ask(q).toOption.exists(_.nonEmpty)) answeredShapes += shape }
        else answers.getOrElseUpdate(q, Answer.Known(Nil))
      case q => ask(q)
    })
  } else queriesOf.values.flatten.toSeq.distinct.sorted.foreach(ask)

  val isTitle: CandidateQuery => Boolean = { case _: CandidateQuery.Title => true; case _ => false }
  // Each candidate a node's own title searches named, at the best (1-based) rank any gave it.
  val ownSearch: Map[String, Map[Int, Int]] = nodes.map { n =>
    n.id -> queriesOf(n.id).filter(isTitle).flatMap(q => answers(q).toOption.getOrElse(Nil).zipWithIndex)
      .groupMapReduce(_._1.tmdbId)(_._2 + 1)(math.min)
  }.toMap
  // Each candidate a node's other paths reached: its credited directors' filmographies, and the
  // films IMDb lists under its title (found by their IMDb ids) — paths, never a search rank.
  val ownWalk: Map[String, Set[Int]] = nodes.map(n =>
    n.id -> queriesOf(n.id).filterNot(isTitle).flatMap(q => answers(q).toOption.getOrElse(Nil)).map(_.tmdbId).toSet).toMap
  /** The candidates the nodes of a node's IDENTICAL title (`IdentityMeasures.key`) reached by their
   *  own evidence — a credited director, a detail page's original title — that its own did not:
   *  "Vincent. Legenda oceanu" at a venue publishing nothing else searches empty (TMDB titles the
   *  film "The Last Whale Singer"), while the same title credited elsewhere walks its director to
   *  it. A path to a candidate, never evidence for it: the node scores it on its own facts (no
   *  search rank), and still denies it when they rule it out. */
  val titleOf: EvidenceNode => String = n => IdentityMeasures.key(n.evidence.title)
  val sharedOf: Map[String, Set[Int]] = nodes.groupBy(titleOf).filter { case (t, ns) => t.nonEmpty && ns.size > 1 }.values.toSeq
    .flatMap { ns =>
      val reached = ns.map(n => n.id -> (ownSearch(n.id).keySet ++ ownWalk(n.id))).toMap
      ns.map(n => n.id -> (ns.filterNot(_ eq n).flatMap(m => reached(m.id)).toSet -- reached(n.id)))
    }.toMap.withDefaultValue(Set.empty)
  val hitsById = nodes.flatMap(n => queriesOf(n.id).flatMap(q => answers(q).toOption.getOrElse(Nil))).groupBy(_.tmdbId)
  val records  = hitsById.keys.toSeq.sorted.map(id => id -> lookups.film(id)).toMap
  val recorded: Map[Int, Candidate] = hitsById.map { case (id, hs) => id -> Candidate.of(id, hs, records(id).toOption.flatten) }
  // Each record with the titles the venues publish for it (`IdentityMeasures.venueTitles`).
  val venueTitles = IdentityMeasures.venueTitles(nodes.map(_.evidence.measured), recorded.toSeq.sortBy(_._1).map { case (id, c) => id -> c.film })
  val candidateById: Map[Int, Candidate] = recorded.map { case (id, c) =>
    id -> c.copy(film = IdentityMeasures.withVenueTitles(c.film, venueTitles.getOrElse(id, Nil))) }
}
