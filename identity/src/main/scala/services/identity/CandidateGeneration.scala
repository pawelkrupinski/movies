package services.identity

import services.movies.TitleNormalizer

import scala.collection.mutable

/** Stage A of [[IdentityResolver]]: each listing's own detail page merged into its [[Evidence]],
 *  listings with identical evidence as one [[EvidenceNode]], every node's [[CandidateQueries]]
 *  asked — all of them, up front, in sorted order, none conditional on another's answer — and
 *  every film any answer names looked up once. `ordered` is the listings in the order they are
 *  walked; `lazyLookups` is the teeth tests' `Mutation.LazyLookups`. */
private[identity] final class CandidateGeneration(ordered: Seq[Listing], lookups: IdentityLookups, val normalizer: TitleNormalizer,
                                                  pins: PinConstraints, decorations: TitleDecorations, lazyLookups: Boolean,
                                                  corpus: Option[CorpusContext] = None) {
  val details = mutable.LinkedHashMap.empty[(String, String), Answer[Option[DetailFacts]]]
  /** Are the films numbered by their TMDB ids ([[IdentityLookups.numbersFilmsByTmdbId]])? */
  def numbersFilmsByTmdbId: Boolean = lookups.numbersFilmsByTmdbId
  /** Whether `listing`'s venue page is to be read and not answered yet -- read meanwhile as no detail at all: a question
   *  a decision taking no film awaits ([[ResolverDecision.awaits]]). Read through the lookups, so an incremental model
   *  files the family as the page's reader. */
  def pagePending(listing: Listing): Boolean =
    lookups.hasDetail(listing) && details.getOrElseUpdate((listing.venue, listing.page.getOrElse("")), lookups.detail(listing)).isAwaited
  def detailOf(listing: Listing): Option[DetailFacts] =
    if (!lookups.hasDetail(listing)) None
    else details.getOrElseUpdate((listing.venue, listing.page.getOrElse("")), lookups.detail(listing)).answered.flatten
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
        if (!answeredShapes(shape)) { if (ask(query).answered.exists(_.nonEmpty)) answeredShapes += shape }
        else answers.getOrElseUpdate(query, Answer.Known(Nil))
      case query => ask(query)
    })
  } else {
    val asked = queriesOf.values.flatten.toSeq.distinct.sorted
    lookups.prefetch(asked, Nil, Nil)
    asked.foreach(ask)
  }

  /** What each node's own queries reached ([[CandidateGeneration.Reach]]). */
  val reach: Map[String, CandidateGeneration.Reach] = nodes.map(node => node.id -> reachOf(queriesOf(node.id))).toMap
  /** What `queries` reached, from the answers this resolve holds: a node's own, or those of the evidence a family's scope
   *  reads it by (`FamilyScope.reach`) — a query no node asked is not answered (Unknown, never "answered empty"), and
   *  reaches nothing: [[CandidateGeneration.Reach.of]] reads only what answers hold. */
  def reachOf(queries: Seq[CandidateQuery]): CandidateGeneration.Reach =
    CandidateGeneration.Reach.of(queries, query => answers.getOrElse(query, Answer.Unknown))
  /** The films a fallback source lists under a node's search titles that TMDB holds no record of (the fallback ids an
   *  [[CandidateQuery.ImdbTitled]] answer carries): never in a TMDB pool, only what a cluster no TMDB film was taken for
   *  may fall back to. */
  val fallbackOf: Map[String, Seq[Int]] = nodes.map(node => node.id -> hitsOf(node.id).map(_.tmdbId).filter(FallbackIds.isFallback).distinct.sorted).toMap
  val hitsById = nodes.flatMap(node => hitsOf(node.id)).groupBy(_.tmdbId)
  /** Every hit a node's own queries' answers hold — none for a query not answered. */
  def hitsOf(nodeId: String): Seq[Hit] = queriesOf(nodeId).flatMap(query => answers(query).answered.getOrElse(Nil))
  // Looked up only when these listings are the whole corpus: a region reads its candidates from
  // the corpus's context, which holds every record already.
  lazy val records  = {
    val ids = hitsById.keys.toSeq.sorted
    lookups.prefetch(Nil, ids, Nil)
    ids.map(id => id -> lookups.film(id)).toMap
  }
  lazy val recorded: Map[Int, Candidate] = hitsById.map { case (id, hits) => id -> Candidate.of(id, hits, records(id)) }
  /** Whether these listings are part of a larger corpus, read through its context. */
  def partOfCorpus: Boolean = corpus.isDefined
  /** Every candidate a node's own evidence reached: its searches' and its walks'. */
  def reached(node: EvidenceNode): Seq[Int] = CandidateGeneration.reached(reach(node.id).search, reach(node.id).walk)

  /** What these listings' families read from the whole corpus: `corpus` when they are part of a
   *  larger one, else their own. */
  lazy val context: CorpusContext = corpus.getOrElse(CorpusContext.of(nodes, reached, recorded,
    query => answers.get(query).flatMap(_.answered).map(CandidateGeneration.ranked), normalizer.sanitize))
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
    if (corpus.isEmpty) None else hitsById.get(id).map(hits => Candidate.of(id, hits, lookups.film(id)))
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
    queries.filter(isTitle).flatMap(query => answer(query).answered.getOrElse(Nil).zipWithIndex).groupMapReduce(_._1.tmdbId)(_._2 + 1)(math.min)
  /** Each candidate a node's other paths reached: its credited directors' filmographies, and the
   *  films IMDb lists under its title (found by their IMDb ids) — paths, never a search rank. */
  def ownWalk(queries: Seq[CandidateQuery], answer: CandidateQuery => Answer[Seq[Hit]]): Set[Int] =
    queries.filterNot(isTitle).flatMap(query => answer(query).answered.getOrElse(Nil)).map(_.tmdbId).filterNot(FallbackIds.isFallback).toSet
  /** What a node's queries reached: each film its title searches named, at its best rank ([[ownSearch]]); the films its
   *  other paths reached ([[ownWalk]]); the films the people it credits — found by its OWN spelling of their names —
   *  directed or wrote; the films that were the ONLY result of one of its title searches; the films IMDb suggests for
   *  its own title, in IMDb's order; and the films IMDb lists under one of its search titles in some language
   *  ([[CandidateQuery.ImdbTitled]]), each with those titles. An IMDb-titled film is an ordinary candidate, scored on the
   *  listing's facts and counted as a rival like any other: what IMDb's title alone may do is `Acceptance.imdbSuggested`'s
   *  titled rung. */
  final case class Reach(search: Map[Int, Int], walk: Set[Int], directed: Set[Int], soleResults: Set[Int], imdbSuggested: Seq[Int],
                         imdbTitled: Map[Int, Set[String]], soleQueries: Map[Int, Set[String]] = Map.empty) {
    /** The films that were the only result of a title search other than those `excluded` names. */
    def soleResultsBeside(excluded: String => Boolean): Set[Int] =
      soleQueries.collect { case (id, texts) if texts.exists(text => !excluded(text)) => id }.toSet
    /** The films only IMDb's suggestions reached — no title search and no director walk. */
    lazy val imdbOnly: Set[Int] = imdbSuggested.toSet -- search.keySet -- directed
  }
  object Reach {
    def of(queries: Seq[CandidateQuery], answer: CandidateQuery => Answer[Seq[Hit]]): Reach = {
      def answered(query: CandidateQuery) = answer(query).answered.getOrElse(Nil)
      Reach(ownSearch(queries, answer), ownWalk(queries, answer),
        queries.collect { case query: CandidateQuery.Director => answered(query).map(_.tmdbId) }.flatten.toSet,
        queries.filter(isTitle).flatMap(query => answer(query).answered.map(_.map(_.tmdbId).distinct)).collect { case Seq(id) => id }.toSet,
        queries.collect { case query: CandidateQuery.Imdb => answered(query).map(_.tmdbId) }.flatten.distinct,
        queries.collect { case query @ CandidateQuery.ImdbTitled(title) => answered(query).map(_.tmdbId).filterNot(FallbackIds.isFallback).map(_ -> title) }
          .flatten.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap,
        queries.collect { case query @ CandidateQuery.Title(text) => answered(query).map(_.tmdbId).distinct match {
          case Seq(id) => Some(id -> text)
          case _       => None
        } }.flatten.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)
    }
  }
  /** Every candidate a node's own evidence reached: its searches' and its walks'. */
  def reached(ownSearch: Map[Int, Int], ownWalk: Set[Int]): Seq[Int] = (ownSearch.keys ++ ownWalk).toSeq.distinct.sorted
  /** The films an answer names, best first. */
  def ranked(hits: Seq[Hit]): Seq[Int] = hits.map(_.tmdbId).distinct
  /** A node's key as text: its evidence's, and its pins'. */
  def nodeKeyText(key: (String, Set[String])): String =
    // No pin (the usual case): the evidence's own key, the one instance every listing of the node shares.
    if (key._2.isEmpty) key._1 else (key._1 +: key._2.toSeq.sorted).mkString("\u0000")
}
