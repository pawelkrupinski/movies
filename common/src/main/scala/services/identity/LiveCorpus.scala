package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * The corpus context ([[CorpusContext]]) kept current event by event: every fact is held per key —
 * a node, a question, a film, a title key, a banner — and an event re-derives only the keys it
 * reaches, along the facts' own dependencies:
 *
 *   listing → its node → the node's questions and their answers → the films they name (hits, record)
 *     → the venue titles of every title key whose original files one of them → each film's candidate
 *     → the billings of every node reaching it → each banner's house.
 *
 * Every derivation is the whole-corpus one applied to one key's inputs (`Candidate.of`,
 * `IdentityMeasures.venueTitles`, `Houses.learn`), so the context equals [[CorpusContext.of]] of the
 * listings held — `LiveCorpusSpec` asserts it after every event.
 */
private[identity] final class LiveCorpus(lookups: IdentityLookups, normalizer: TitleNormalizer, pins: PinConstraints,
                                         decorations: TitleDecorations,
                                         slices: LiveCorpus.Slices = LiveCorpus.Slices.Default) extends CorpusContext {
  private type NodeKey = (String, Set[String])
  /** A node's listings, and the one key instance every one of them is filed under. */
  private final class Members(val key: NodeKey) { val held = mutable.TreeMap.empty[Listing, Listing] }

  private final class Node(val node: EvidenceNode, val queries: Seq[CandidateQuery], val reached: Seq[Int]) {
    def id: String        = node.id
    val titleKey: String  = CorpusContext.titleOf(node)
    val whole: String     = normalizer.sanitize(node.evidence.cleanTitle)
    val pieces: Set[String] = CorpusContext.piecesOf(node, normalizer.sanitize)
    def translated: Boolean = node.evidence.measured.originalTitle.nonEmpty
  }

  // listings and nodes
  // per listing only the node it is one of; evidence per node — its head listing's, which is the node's
  private val listings = mutable.HashMap.empty[ListingKey, NodeKey]
  private val heads    = mutable.HashMap.empty[NodeKey, (ListingKey, Evidence)]
  private val members  = mutable.HashMap.empty[NodeKey, Members]
  private val nodes    = mutable.HashMap.empty[NodeKey, Node]
  // questions and films — never a question's whole answer, which the lookups keep: only the films it
  // named, and per film the best hit it gave (all `Candidate.of` reads: the most popular hit first)
  private val askers   = mutable.HashMap.empty[CandidateQuery, Set[NodeKey]]
  private val named    = mutable.HashMap.empty[CandidateQuery, Option[Seq[Int]]]
  private val bestHits = mutable.LongMap.empty[Map[CandidateQuery, Hit]]
  private val records  = mutable.LongMap.empty[Answer[Option[IdentityMeasures.Film]]]
  private val base     = mutable.LongMap.empty[Candidate]
  private val reachers = mutable.LongMap.empty[Set[NodeKey]]
  // A season-naming node's banner bills every recorded production of its work that season, not only
  // the ones it reached (`CorpusContext.billedFilms`): both sides by (work, season) pair, never per node.
  private val seasonNodes = mutable.HashMap.empty[(String, Int), Set[NodeKey]]
  private val seasonFilms = mutable.HashMap.empty[(String, Int), Set[Int]]
  // venue titles: the translated nodes under each title key, their origins, the films under each key
  private val translatedBy  = mutable.HashMap.empty[String, Set[NodeKey]]
  private val originsOfKey  = mutable.HashMap.empty[String, Set[String]]
  private val keysOfOrigin  = mutable.HashMap.empty[String, Set[String]]
  private val filmsByKey    = mutable.HashMap.empty[String, Set[Int]]
  private val venueTitlesOf = mutable.LongMap.empty[Map[String, Seq[String]]]
  private val titledBy      = mutable.HashMap.empty[String, Set[Int]]
  private val candidates    = mutable.LongMap.empty[Candidate]
  // houses, title groups, whole titles, reached-by-title
  private val billingsOf   = mutable.HashMap.empty[NodeKey, Seq[IdentityMeasures.Billing]]
  private val byBanner     = mutable.HashMap.empty[String, Map[NodeKey, Seq[IdentityMeasures.Billing]]]
  private val houseOf      = mutable.HashMap.empty[String, String]
  private val rankingOf    = mutable.HashMap.empty[String, Seq[IdentityMeasures.Houses.Contender]]
  private val groups       = mutable.HashMap.empty[String, mutable.TreeMap[String, Seq[(String, IdentityMeasures.Listing)]]]
  private val wholes       = mutable.HashMap.empty[String, Int]
  // per title piece, the whole titles carrying it, with how many nodes carry each (`segmentSpread`)
  private val spreads      = mutable.HashMap.empty[String, mutable.HashMap[String, Int]]
  private val reachedByKey = mutable.HashMap.empty[String, mutable.TreeMap[String, Set[Int]]]

  // ── CorpusContext ───────────────────────────────────────────────────────────────────────────
  // The records are kept LEAN: every reader — a region's resolve, the billings, the venue titles —
  // gets a fresh copy of the film, whose cached title forms are computed for that use and dropped
  // with it, never kept on the ~30k resident records (1.6M strings on the UK corpus when they were).
  def candidate(id: Int): Option[Candidate] = candidates.get(id).map(fresh)
  // Read many times per resolve, changed per event: each view is built once after it changes.
  private var housesView: Option[IdentityMeasures.Houses] = None
  private val groupView   = mutable.HashMap.empty[String, Seq[(String, IdentityMeasures.Listing)]]
  private val reachedView = mutable.HashMap.empty[String, Seq[(String, Set[Int])]]
  def houses: IdentityMeasures.Houses = housesView.getOrElse { val built = IdentityMeasures.Houses(houseOf.toMap); housesView = Some(built); built }
  def houseRanking: Map[String, Seq[IdentityMeasures.Houses.Contender]] = rankingOf.toMap
  def titleGroup(key: String): Seq[(String, IdentityMeasures.Listing)] =
    groupView.getOrElseUpdate(key, groups.get(key).fold(Seq.empty)(_.values.flatten.toSeq))
  def wholeTitle(sanitised: String): Boolean = wholes.contains(sanitised)
  def segmentSpread(sanitised: String): Int = spreads.get(sanitised).fold(0)(_.size)
  def reachedBy(key: String): Seq[(String, Set[Int])] = reachedView.getOrElseUpdate(key, reachedByKey.get(key).fold(Seq.empty)(_.toSeq))
  def ranked(query: CandidateQuery): Option[Seq[Int]] = named.get(query).flatten
  override def evidenceOf(listing: Listing): Option[Evidence] =
    listings.get(listing.key).flatMap(heads.get).collect { case (head, evidence) if head == listing.key => evidence }
  /** The node a held listing is one of, as text ([[CandidateGeneration.nodeKeyText]]). */
  def nodeKeyOf(key: ListingKey): Option[String] =
    listings.get(key).map(CandidateGeneration.nodeKeyText)
  /** `keys`, held, split by the title keys their nodes block under (`TitleLinks.titleKeys`): the
   *  families they seed before any match grows one — what a full build resolves one at a time. */
  def titleComponents(keys: Iterable[ListingKey]): Seq[Seq[ListingKey]] = {
    val wanted = keys.toSet
    val byNode = nodes.values.filter(_.node.listings.exists(listing => wanted(listing.key))).map(live =>
      live.id -> (live.node.listings.map(_.key).filter(wanted), TitleLinks.titleKeys(live.node, normalizer, pins, wholeTitle, bannerSegment))).toMap
    FamilyClosure.families(byNode.map { case (id, (_, blockKeys)) => id -> blockKeys }).groupMap(_._2)(_._1).toSeq.sortBy(_._1)
      .map { case (_, ids) => ids.toSeq.sorted.flatMap(id => byNode(id)._1) }
  }

  /** How many nodes the listings held make. */
  def nodeCount: Int = nodes.size
  /** The questions a node asked whose answer is not known, and the named films whose record is not. */
  def gaps: AnswersChanged =
    AnswersChanged(named.collect { case (query, None) => query }.toSet, records.collect { case (id, answer) if !answer.isKnown => id.toInt }.toSet)

  /** How many records the context holds: every film some node's answers name, and no other. */
  def candidateCount: Int = candidates.size

  // ── events ──────────────────────────────────────────────────────────────────────────────────
  def seen(arrived: Seq[Listing]): CorpusContext.Changed = {
    val touched = mutable.HashSet.empty[NodeKey]
    // A slice of detail pages at a time: what a prefetch holds is bounded, however many listings arrive.
    arrived.grouped(slices.details).foreach { slice =>
      lookups.prefetch(Nil, Nil, slice.filter(lookups.hasDetail))
      slice.foreach { listing =>
        listings.remove(listing.key).foreach(key => touched += leave(listing.key, key))
        val evidence = derive(listing)
        val derived  = CandidateGeneration.nodeKey(listing, evidence, pins)
        val group    = members.getOrElseUpdate(derived, new Members(derived))
        // The node's own key instance, not this listing's equal copy: one per node, not per listing.
        val key      = group.key
        listings(listing.key) = key
        val held = group.held
        held(listing) = listing
        if (held.head._2.key == listing.key) heads(key) = listing.key -> evidence
        touched += key
      }
    }
    refresh(touched.toSet, Set.empty, Set.empty)
  }

  def gone(keys: Seq[ListingKey]): CorpusContext.Changed = {
    val left = keys.flatMap(key => listings.remove(key).map(leave(key, _))).toSet
    lookups.released(Nil, Nil, keys)
    refresh(left, Set.empty, Set.empty)
  }

  def answered(changed: AnswersChanged): CorpusContext.Changed =
    refresh(Set.empty, changed.queries.filter(askers.contains), changed.films.filter(bestHits.contains))

  private def leave(listing: ListingKey, key: NodeKey): NodeKey = {
    members.get(key).map(_.held).foreach { held =>
      held.find(_._2.key == listing).foreach { case (sorted, _) => held.remove(sorted) }
      if (held.isEmpty) { members.remove(key); heads.remove(key) }
    }
    key
  }

  private def derive(listing: Listing): Evidence =
    Evidence.of(listing, if (lookups.hasDetail(listing)) lookups.detail(listing).toOption.flatten else None, decorations, lookups.proposal(listing))

  /** Re-derive what the touched nodes, re-answered questions and re-recorded films reach; the keys
   *  whose facts moved. */
  private def refresh(touched: Set[NodeKey], reasked: Set[CandidateQuery], rerecorded: Set[Int]): CorpusContext.Changed = {
    val films     = mutable.HashSet.empty[Int] ++ rerecorded
    val titleKeys = mutable.HashSet.empty[String]
    val segments  = mutable.HashSet.empty[String]
    val moved     = mutable.HashSet.empty[Int]
    val rebuilt   = touched ++ reasked.flatMap(askers.getOrElse(_, Set.empty))
    // 1. nodes: unhook the old, re-ask what was asked anew, hook the new — noting each touched piece's
    // banner bit before, so a piece crossing `CorpusContext.BannerSpread` dirties the families reading it
    val bannersBefore = mutable.HashMap.empty[String, Boolean]
    def noteBefore(pieces: Set[String]): Unit = pieces.foreach(piece => bannersBefore.getOrElseUpdate(piece, bannerSegment(piece)))
    rebuilt.foreach(key => nodes.remove(key).foreach(old => { noteBefore(old.pieces); films ++= unlink(key, old); titleKeys += old.titleKey; segments += old.whole }))
    val built = rebuilt.toSeq.flatMap(key => members.get(key).map(_.held).map { held =>
      val listed  = held.values.toSeq
      val head    = heads.get(key).filter(_._1 == listed.head.key).map(_._2).getOrElse(derive(listed.head))
      heads(key)  = listed.head.key -> head
      val node    = new EvidenceNode(head, listed)
      (key, node, CandidateQueries.of(node.evidence))
    })
    // A slice of nodes at a time, its answers read for that slice only and dropped with it: a
    // refresh of the whole corpus (a take-up) never holds every answer at once.
    built.grouped(slices.nodes).foreach { slice =>
      val answers = mutable.HashMap.empty[CandidateQuery, Answer[Seq[Hit]]]
      val answer  = (query: CandidateQuery) => answers.getOrElseUpdate(query, lookups.candidates(query))
      lookups.prefetch(slice.flatMap(_._3).distinct, Nil, Nil)
      slice.foreach { case (key, node, queries) =>
        // Film ids kept unboxed (`ArraySeq.ofInt`): a TMDB id is above the JVM's small-integer cache,
        // so a Seq[Int] of them held one Integer object per id for the node's lifetime.
        val live = new Node(node, queries, ArraySeq.from(
          CandidateGeneration.reached(CandidateGeneration.ownSearch(queries, answer), CandidateGeneration.ownWalk(queries, answer))))
        nodes(key) = live
        noteBefore(live.pieces)
        films ++= link(key, live, answer)
        titleKeys += live.titleKey
        segments += live.whole
      }
    }
    // 2. films: each one's base candidate, and the title keys it files under
    val filedUnder = mutable.HashSet.empty[String]
    films.toSeq.sorted.grouped(slices.records).foreach { slice =>
    lookups.prefetch(Nil, slice.filter(id => bestHits.contains(id) && (rerecorded(id) || !records.contains(id))), Nil)
    slice.foreach { id =>
      base.remove(id).foreach(old => filmKeys(old.film).foreach { key => filedUnder += key; filmsByKey.updateWith(key)(_.map(_ - id).filter(_.nonEmpty)) })
      bestHits.get(id) match {
        case Some(byQuery) =>
          if (rerecorded(id) || !records.contains(id)) records(id) = lookups.film(id)
          val candidate = Candidate.of(id, byQuery.values.toSeq, records(id).toOption.flatten)
          base(id) = candidate
          filmKeys(candidate.film).foreach { key => filedUnder += key; filmsByKey.updateWith(key)(ids => Some(ids.getOrElse(Set.empty) + id)) }
        case None => records.remove(id); lookups.released(Nil, Seq(id), Nil)
      }
    }
    }
    // 3. venue titles of every title key a touched node carries, whose original a touched film files under,
    // or whose nodes reach a touched film (the titles their facts give it)
    val retitled  = mutable.HashSet.empty[Int] ++ films
    val reachKeys = films.iterator.flatMap(id => reachers.getOrElse(id, Set.empty)).flatMap(nodes.get).map(_.titleKey)
    (titleKeys ++ filedUnder.flatMap(keysOfOrigin.getOrElse(_, Set.empty)) ++ reachKeys).foreach(key => retitled ++= retitle(key))
    // 4. candidates; a changed one re-bills every node reaching it
    val billed = mutable.HashSet.empty[NodeKey] ++ rebuilt
    retitled.foreach { id =>
      val next = base.get(id).map(candidate => candidate.copy(film = IdentityMeasures.withVenueTitles(candidate.film,
        venueTitlesOf.get(id).fold(Seq.empty[String])(_.values.flatten.toSeq.distinct.sorted))))
      if (next != candidates.get(id)) {
        val before = candidates.get(id).toSeq.flatMap(candidate => IdentityMeasures.seasonWorks(candidate.film))
        next.fold(candidates.remove(id))(candidate => candidates.put(id, candidate))
        val after  = next.toSeq.flatMap(candidate => IdentityMeasures.seasonWorks(candidate.film))
        before.foreach(pair => seasonFilms.updateWith(pair)(_.map(_ - id).filter(_.nonEmpty)))
        after.foreach(pair => seasonFilms.updateWith(pair)(held => Some(held.getOrElse(Set.empty) + id)))
        billed ++= reachers.getOrElse(id, Set.empty) ++ (before ++ after).distinct.flatMap(seasonNodes.getOrElse(_, Set.empty))
        moved += id
      }
    }
    // 5. billings, and each touched banner's house
    val banners = mutable.HashSet.empty[String]
    // Film by film: each copied once, billed against every node here that reaches it, and dropped —
    // a take-up bills every node, and a film reached by many nodes derived its title forms again for
    // each (6 of a UK take-up's 16 s). A node's billings are its films' in its reached order, which is
    // `Houses.evidence` over them all.
    val billedBy  = mutable.HashMap.empty[NodeKey, Map[Int, Seq[IdentityMeasures.Billing]]]
    val billedOn  = billed.iterator.flatMap(key => nodes.get(key).map(live => key -> CorpusContext.billedFilms(live.reached,
      live.node.evidence.measured, seasonFilms.getOrElse(_, Set.empty)).filter(candidates.contains))).toMap
    val billersOf = billedOn.toSeq.flatMap { case (key, ids) => ids.map(_ -> key) }.groupMap(_._1)(_._2)
    billersOf.foreach { case (id, keys) =>
      val film = fresh(candidates(id)).film
      keys.foreach { key =>
        val own = IdentityMeasures.Houses.evidence(nodes(key).node.evidence.measured, Seq(film)).toSeq
        if (own.nonEmpty) billedBy.updateWith(key)(held => Some(held.getOrElse(Map.empty) + (id -> own)))
      }
    }
    billed.foreach { key =>
      billingsOf.remove(key).foreach(_.map(_.listingHouse).distinct.foreach { banner =>
        banners += banner; byBanner.updateWith(banner)(_.map(_ - key).filter(_.nonEmpty)) })
      nodes.get(key).foreach { live =>
        val byFilm   = billedBy.getOrElse(key, Map.empty)
        val billings = billedOn.getOrElse(key, Nil).flatMap(byFilm.getOrElse(_, Nil))
        if (billings.nonEmpty) {
          billingsOf(key) = billings
          billings.groupBy(_.listingHouse).foreach { case (banner, own) =>
            banners += banner; byBanner.updateWith(banner)(held => Some(held.getOrElse(Map.empty) + (key -> own))) }
        }
      }
    }
    banners.foreach { banner =>
      val evidence = byBanner.getOrElse(banner, Map.empty).values.flatten.toSeq
      IdentityMeasures.Houses.learn(evidence).of.get(banner).fold(houseOf.remove(banner))(house => houseOf.put(banner, house))
      IdentityMeasures.Houses.ranking(evidence).get(banner).fold(rankingOf.remove(banner))(ranked => rankingOf.put(banner, ranked))
    }
    if (banners.nonEmpty) housesView = None
    groupView --= titleKeys
    reachedView --= titleKeys
    segments ++= bannersBefore.collect { case (piece, before) if bannerSegment(piece) != before => piece }
    CorpusContext.Changed(titleKeys.toSet, segments.toSet, banners.toSet, moved.toSet)
  }

  private def fresh(candidate: Candidate): Candidate = candidate.copy(film = candidate.film.copy())

  private def filmKeys(film: IdentityMeasures.Film): Seq[String] =
    (Seq(film.title) ++ film.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).distinct

  /** Unhook a node from every per-key fact; the films whose hits it took away. */
  private def unlink(key: NodeKey, old: Node): Set[Int] = {
    old.queries.foreach(query => askers.updateWith(query)(_.map(_ - key).filter(_.nonEmpty)))
    val dropped = old.queries.filterNot(askers.contains).distinct
    lookups.released(dropped, Nil, Nil)
    val films   = dropped.flatMap(query => named.remove(query).flatten.getOrElse(Nil)).toSet
    films.foreach(id => bestHits.updateWith(id)(_.map(_ -- dropped).filter(_.nonEmpty)))
    old.reached.foreach(id => reachers.updateWith(id)(_.map(_ - key).filter(_.nonEmpty)))
    IdentityMeasures.seasonWorks(old.node.evidence.measured).foreach(pair => seasonNodes.updateWith(pair)(_.map(_ - key).filter(_.nonEmpty)))
    groups.get(old.titleKey).foreach { byNode => byNode.remove(old.id); if (byNode.isEmpty) groups.remove(old.titleKey) }
    if (old.whole.nonEmpty) wholes.updateWith(old.whole)(_.map(_ - 1).filter(_ > 0))
    old.pieces.foreach(piece => spreads.get(piece).foreach { byWhole =>
      byWhole.updateWith(old.whole)(_.map(_ - 1).filter(_ > 0)); if (byWhole.isEmpty) spreads.remove(piece) })
    reachedByKey.get(old.titleKey).foreach { byNode => byNode.remove(old.id); if (byNode.isEmpty) reachedByKey.remove(old.titleKey) }
    if (old.translated) translatedBy.updateWith(old.titleKey)(_.map(_ - key).filter(_.nonEmpty))
    films
  }

  /** Hook a node into every per-key fact; the films its answers name. A question another node
   *  already asked is filed already. */
  private def link(key: NodeKey, live: Node, answer: CandidateQuery => Answer[Seq[Hit]]): Set[Int] = {
    val films = live.queries.distinct.filterNot(askers.contains).flatMap { query =>
      val known  = answer(query).toOption
      val byFilm = known.getOrElse(Nil).groupBy(_.tmdbId)
      named(query) = known.map(hits => ArraySeq.from(CandidateGeneration.ranked(hits)))
      byFilm.map { case (id, hits) =>
        val best = hits.minBy(hit => (-hit.popularity, hit.title, hit.originalTitle.getOrElse(""), hit.year.getOrElse(0)))
        bestHits.updateWith(id)(held => Some(held.getOrElse(Map.empty) + (query -> best))); id }
    }.toSet
    live.queries.foreach(query => askers.updateWith(query)(held => Some(held.getOrElse(Set.empty) + key)))
    live.reached.foreach(id => reachers.updateWith(id)(held => Some(held.getOrElse(Set.empty) + key)))
    IdentityMeasures.seasonWorks(live.node.evidence.measured).foreach(pair => seasonNodes.updateWith(pair)(held => Some(held.getOrElse(Set.empty) + key)))
    groups.getOrElseUpdate(live.titleKey, mutable.TreeMap.empty)(live.id) = live.node.listings.map(listing => listing.venue -> live.node.evidence.measured)
    if (live.whole.nonEmpty) wholes.updateWith(live.whole)(count => Some(count.getOrElse(0) + 1))
    live.pieces.foreach(piece => spreads.getOrElseUpdate(piece, mutable.HashMap.empty).updateWith(live.whole)(count => Some(count.getOrElse(0) + 1)))
    if (live.titleKey.nonEmpty) reachedByKey.getOrElseUpdate(live.titleKey, mutable.TreeMap.empty)(live.id) = live.reached.toSet
    if (live.translated) translatedBy.updateWith(live.titleKey)(held => Some(held.getOrElse(Set.empty) + key))
    films
  }

  /** The venue titles title key `key` gives — `venueTitles` over its translated nodes and the films
   *  filed under their originals' keys; the films whose venue titles moved. */
  private def retitle(key: String): Set[Int] = {
    val translated = translatedBy.getOrElse(key, Set.empty).toSeq.flatMap(nodes.get).sortBy(_.id).map(_.node.evidence.measured)
    val origins    = translated.flatMap(_.originalTitle).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    originsOfKey.getOrElse(key, Set.empty).diff(origins).foreach(origin => keysOfOrigin.updateWith(origin)(_.map(_ - key).filter(_.nonEmpty)))
    origins.foreach(origin => keysOfOrigin.updateWith(origin)(held => Some(held.getOrElse(Set.empty) + key)))
    if (origins.isEmpty) originsOfKey.remove(key) else originsOfKey(key) = origins
    val films  = origins.flatMap(filmsByKey.getOrElse(_, Set.empty)).toSeq.sorted.flatMap(id => base.get(id).map(candidate => id -> fresh(candidate).film))
    // The title key's own nodes (as `groups` holds them, by node id) and what their title searches found.
    val keyed  = groups.get(key).toSeq.flatMap(_.valuesIterator.flatMap(_.headOption.map(_._2)))
    val found  = keyed.flatMap(CorpusContext.searchFound(_, query => named.get(query).flatten))
    val next   = (IdentityMeasures.venueTitles(translated, films).toSeq.flatMap { case (id, titles) => titles.map(id -> _) } ++
      CorpusContext.titlesByFacts(keyed, found, base.get)).groupMap(_._1)(_._2).map { case (id, titles) => id -> titles.distinct.sorted }
    val before = titledBy.getOrElse(key, Set.empty)
    if (next.isEmpty) titledBy.remove(key) else titledBy(key) = next.keySet
    (before ++ next.keys).filter { id =>
      val titles = next.getOrElse(id, Nil)
      val moved  = venueTitlesOf.get(id).flatMap(_.get(key)).getOrElse(Nil) != titles
      venueTitlesOf.updateWith(id)(held => Some(held.getOrElse(Map.empty) - key ++ Option.when(titles.nonEmpty)(key -> titles)).filter(_.nonEmpty))
      moved
    }
  }
}

private[identity] object LiveCorpus {
  /** How many nodes, detail pages and records one prefetch — and one slice's answers — may hold. */
  final case class Slices(nodes: Int, details: Int, records: Int)
  object Slices { val Default: Slices = Slices(nodes = 500, details = 1000, records = 2000) }
}
