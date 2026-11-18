package services.identity

import services.identity.IdentityMeasures.ListingFilm

/** Each final cluster as a [[ResolverDecision]]: its film, confidence, basis and explanation. */
private[identity] final class ResolverDecisions(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.{answers, nodeById, queriesOf}
  import families.{bestOf, pinnedFilm}

  private val traces = new ResolverDecisions.Traces(scoring.generation, families, acceptance)

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
         votes: Map[String, ResolverDecisions.Vote], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
    val films = cluster.flatMap(node => filmOf(node.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val read  = new ClusterRead(cluster, scope, films.headOption, accepted, votes)
    val ids   = cluster.map(_.id).toSet
    val apart = keptApart(edges, ids, clusterIndex)
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, read.film, read.confidence, read.basis,
      explanation(read, edges, ids, apart), contradictions = apart, fallback = read.fallback, leaning = read.leaning,
      unanswered = read.unknown, candidate = read.candidate, awaits = read.awaits,
      filmTitled = cluster.flatMap(node => node.listings.filter(agreement.NonFilmEvents.filmTitled(_, scoring.generation.hitsOf(node.id))).map(_.key)).toSet)(
      traces.of(cluster, scope, read.scored, read.film, read.basis, edges, ids))
  }

  /** What one cluster's decision reads: its film, its pooled scoring, the confidence and basis they make, and what it
   *  falls back to, leans to or waits on. */
  private final class ClusterRead(val cluster: Seq[EvidenceNode], val scope: FamilyScope, val film: Option[Int],
                                  accepted: Map[String, Int], val votes: Map[String, ResolverDecisions.Vote]) {
    val pinned   = film.isDefined && cluster.exists(node => pinnedFilm.contains(node.id))
    val scored   = scope.pooled(cluster)
    val eligible = scored.filterNot(_.denied)
    private val ownMatch = cluster.exists(node => accepted.contains(node.id))
    val unknown  = cluster.flatMap(node => queriesOf(node.id)).distinct.count(query => answers(query).isAwaited)

    /** A cluster the title family alone decided is filed at the family's bound when its own scoring rates the film
     *  lower. */
    private def familyBound = film.filter(_ => !ownMatch).flatMap(filmId =>
      cluster.flatMap(node => votes.get(node.id)).filter(vote => vote.majority.isDefined && vote.film == filmId).map(_.probability).minOption)

    val confidence =
      if (pinned) 1.0
      else film.fold(eligible.map(1 - _.probability).product)(filmId => math.max(acceptance.confidenceOf(scored, filmId), familyBound.getOrElse(0.0)))

    val basis =
      if (pinned) ResolverDecision.Basis.Pinned
      else if (film.isDefined) (if (ownMatch) ResolverDecision.Basis.OwnMatch else ResolverDecision.Basis.PooledMatch)
      else if (scored.isEmpty) (if (unknown > 0) ResolverDecision.Basis.NoEvidence else ResolverDecision.Basis.NoCandidate)
      else if (scored.head.denied) ResolverDecision.Basis.Vetoed
      else ResolverDecision.Basis.BelowThreshold

    // the family's fallback-source scope, built only for a cluster no TMDB film was taken for
    private lazy val fallbacks = new FamilyScope(scope.members, scoring, acceptance, () => (), fallback = true)
    // the records of the films its family's pools weigh -- TMDB's and the fallback's -- not filed yet: each is weighed on its
    // hits meanwhile, its record's facts unknown (FamilyScope, step 6), so the take is provisional until they are filed, and
    // a fallback rests on its IMDb record. Told only: no take moves by it. US "Parkway Drive: Home" x123 (drift 3991) held no film while its
    // fallback's record was pending, read as Vetoed with nothing waited on, and took tt47657111 once it was filed
    val awaited = (if (film.isDefined) scope.pool else scope.pool ++ fallbacks.pool).filter(_.awaitingRecord).map(_.tmdbId).distinct.sorted
    // a cluster no TMDB film was taken for: the one fallback film its members take -- a search unanswered too, decided on
    // the inputs as they stand and Pending on it (step 6, the owner 2026-10-31: no take withheld while a question is open;
    // it waited for every search before)
    val fallback = if (film.isDefined) None else fallbackOf(cluster, scope, fallbacks)

    // what a cluster taking no film still waits on (ResolverDecision.awaits: its outcome Pending, not NoFilm): its searches
    // unanswered, the records its family's pools await, its listings' venue pages not read yet
    // -- and a cluster taking a film too: a search not answered yet may name a film it weighs, a venue page not read yet a
    // fact it reads, so its take is interim until they are (P1, identity-flips-structural-2026-10-30 step 6: PL "Lalka"
    // pooled to 1001 with a search unanswered, corrected to 1002 once it was). Typed only: the film is served all the same
    // -- and a fallback taken: it is the decision on what is filed, provisional while anything it reads is not
    // -- and every node of its FAMILY: the decision READ their evidence, not its own cluster's alone (the flip-unification
    // design's stage 4, read-set closure). The family's pool is every member's searches' hits; a sibling's denial withdraws
    // a member's own match, a facts-free cluster follows the family's majority, a sibling's own match keeps a film the
    // group vote may not take back, and the solver's must-links reach nodes held apart (UK Torrington "Dracula",
    // convergence #595: left alone while Quad Derby's "Dracula (15)" page was unread, back in tmdb 11868's cluster once it
    // was read -- 589a6e3ea awaited the reach alone). Each reads that sibling's facts as they stand, so the take awaits
    // their unanswered questions too: Pending, served all the same, until they close.
    def awaits: Set[Question] = {
      val reads = scope.members
      reads.flatMap(node => queriesOf(node.id)).distinct.filter(query => answers(query).isAwaited).map(Question.Candidate(_)).toSet ++
        awaited.map(Question.FilmRecord(_)) ++
        reads.flatMap(_.listings).filter(scoring.generation.pagePending).map(listing => Question.VenuePage(listing.key))
    }

    // a film a reused page's copied original title and director alone name ([[Acceptance.copiedBeyondItsBilling]]): facts
    // that agree with it by construction, so no vote of TMDB's for it (DE Scheune Dresden's "Regio Tales", an event on
    // kinoheld's "Heads and Tales" page)
    private def copiedOnly(tmdbId: Int) = eligible.exists(s => s.candidate.tmdbId == tmdbId && acceptance.copiedBeyondItsBilling(s))
    private def deniedByMember(tmdbId: Int) = cluster.exists(node => families.denies(node, tmdbId))

    /** The one TMDB film the members lean to, none of them denying it. */
    val leaning: Option[ResolverDecision.Leaning] = {
      val leaned = if (film.isDefined) Nil else cluster.flatMap(node => acceptance.leaning(scope.of(node))).map(_.candidate).distinctBy(_.tmdbId)
      leaned match {
        case Seq(lean) if lean.film.imdbNumber > 0 && !deniedByMember(lean.tmdbId) && !copiedOnly(lean.tmdbId) =>
          Some(ResolverDecision.Leaning(lean.tmdbId, lean.film.imdbNumber))
        case _ => None
      }
    }

    /** The best-ranked TMDB film the pooled evidence weighed that no member denies: the agreement stage's TMDB voter —
     *  none where the release veto took out a film ranked above it, as the veto adds no evidence for its namesake
     *  ([[ReleaseVeto]]: PL "Stan wyjątkowy", Brešan's 2018 film best only once Hřebejk's 2024 one was vetoed), nor a
     *  reused page's copied record. */
    def candidate: Option[ResolverDecision.Leaning] =
      if (film.isDefined || scored.takeWhile(_.denied).exists(_.denial.exists(ReleaseVeto.vetoes))) None
      else eligible.headOption.filter(best => best.untaken.isEmpty && !copiedOnly(best.candidate.tmdbId)).map(_.candidate)
        .filterNot(best => deniedByMember(best.tmdbId))
        .map(best => ResolverDecision.Leaning(best.tmdbId, best.film.imdbNumber))
  }

  /** The decision's explanation, line by line. */
  private def explanation(read: ClusterRead, edges: Seq[ResolverEdge], ids: Set[String], apart: Seq[String]): Seq[String] = {
    import read.cluster
    val own = ownMatches(cluster, read.scope)
    (overruledLines(read) ++ own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ voteLine(read) ++
      joinLines(edges, ids) ++ apart.take(4) ++ bestRejected(read) ++ Option.when(read.unknown > 0)(s"${read.unknown} lookup(s) unanswerable") ++
      Option.when(read.awaited.nonEmpty)(s"${read.awaited.size} record(s) awaited: ${read.awaited.take(5).mkString(", ")}") ++
      read.fallback.map(taken => s"falls back to ${taken.source} ${taken.id} at ${ResolverDecision.percent(taken.probability)}") ++
      read.leaning.map(lean => s"leans to ${lean.film} (tt${lean.imdbNumber})")) :+
      s"node ${cluster.head.listings.head.key}"
  }

  private def overruledLines(read: ClusterRead): Seq[String] =
    read.cluster.flatMap(node => read.scope.overruled(node).map(link => s"${node.label}: ${link.explanation}"))

  private def ownMatches(cluster: Seq[EvidenceNode], scope: FamilyScope): Seq[String] =
    cluster.flatMap(node => bestOf.get(node.id).map { case (scored, confidence) =>
      s"${node.label}: own match ${scored.candidate.tmdbId} at ${ResolverDecision.percent(confidence)}${acceptance.liftedBy(scope.of(node), scored, confidence)}" +
        scope.acceptedBy(node, scored.candidate.tmdbId).fold("")(rule => s" by $rule") + " " +
        s"(${calibration.explain(ListingFilm, scored.measures)})" })

  private def joinLines(edges: Seq[ResolverEdge], ids: Set[String]): Seq[String] =
    edges.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).groupBy(_.reason).toSeq.sortBy(_._1)
      .map { case (reason, reasonEdges) => s"joined by ${reason} ×${reasonEdges.size}" }

  private def keptApart(edges: Seq[ResolverEdge], ids: Set[String], clusterIndex: Map[String, Int]): Seq[String] =
    edges.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map { edge =>
      val other = if (ids(edge.a)) edge.b else edge.a
      s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${edge.reason}"
    }.distinct.sorted

  private def voteLine(read: ClusterRead): Option[String] = {
    val clusterVotes = read.cluster.flatMap(node => read.votes.get(node.id))
    clusterVotes.flatMap(_.majority).headOption.orElse(clusterVotes.headOption.map(v =>
      s"pooled evidence of ${read.cluster.size} node(s) → ${v.film} at ${ResolverDecision.percent(v.probability)}"))
  }

  private def bestRejected(read: ClusterRead): Option[String] =
    read.scored.headOption.filter(scored => !read.film.contains(scored.candidate.tmdbId)).map { scored =>
      val why = scored.denial.orElse(scored.untaken).fold("")(reason => s", ${if (scored.denied) "denied" else "untaken"}: $reason,")
      s"best ${if (scored.denied) "vetoed" else "rejected"} candidate ${scored.candidate.tmdbId} at ${ResolverDecision.percent(scored.probability)}$why (${calibration.explain(ListingFilm, scored.measures)})"
    }

  /** The fallback film a cluster no TMDB film was taken for falls back to: the one every member that takes one takes
   *  ([[Acceptance.fallback]], over the family's fallback pool), denied by no member's evidence — and none while a TMDB
   *  film the title names, or crediting the listing's director, stood undenied: TMDB then knows a film the listing may
   *  be, and its rules not taking it is no licence to take another source's in its place. UK Cineworld's "Royal Ballet
   *  and Opera: Tosca" ×123 fell back to IMDb's record of the 2025/26 production, which TMDB holds too, unlinked. */
  private def fallbackOf(cluster: Seq[EvidenceNode], scope: FamilyScope, fallbacks: FamilyScope): Option[ResolverDecision.Fallback] = {
    lazy val tmdbNamesake = cluster.exists(node => scope.of(node).exists(scored =>
      !scored.denied && (scored.titleNamesIt || IdentityMeasures.sameDirector(scored.measures))))
    if (fallbacks.pool.isEmpty) None
    else cluster.flatMap(node => acceptance.fallback(fallbacks.of(node))).distinctBy(_.candidate.tmdbId) match {
      case Seq(taken) if !tmdbNamesake && !cluster.exists(node => families.denies(node, taken.candidate.tmdbId)) =>
        FallbackIds.unapply(taken.candidate.tmdbId).flatMap { case (source, _) =>
          FallbackIds.imdbId(taken.candidate.tmdbId).map(ResolverDecision.Fallback(source.label, _, taken.probability)) }
      case _ => None
    }
  }
}

private[identity] object ResolverDecisions {
  /** The film a cluster no member matched alone voted for, at `probability` — `majority` saying why when its title
   *  family's clear majority decided it (`ClusterVoting.familyMajority`), its confidence then the family's bound. */
  final case class Vote(film: Int, probability: Double, majority: Option[String])

  /** The rules behind a cluster's decision ([[DecisionTrace]]): each node's own accepting rule, joins and cannot-links,
   *  the pooled rule when the pooled scoring decided, and which member's own evidence denied a vetoed film. */
  private[identity] final class Traces(generation: CandidateGeneration, families: Families, acceptance: Acceptance) {
    import generation.{answers, nodeById, queriesOf}
    import families.bestOf

    def of(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int],
           basis: ResolverDecision.Basis, edges: Seq[ResolverEdge], ids: Set[String]): DecisionTrace = {
      val nodes = cluster.flatMap { node =>
        val traced = nodeTrace(node, scope, film, edges, ids)
        node.listings.map(_.key -> traced)
      }.toMap
      DecisionTrace(pooledRule(scored, film, basis), vetoOf(cluster, scope, scored, film), nodes)
    }

    /** One node's part of the trace. Computed here, not when the trace is written: a thunk would keep `own` — every scored
     *  candidate of the node — alive until the trace writer got to it, and a restore hands the whole corpus over at once. */
    private def nodeTrace(node: EvidenceNode, scope: FamilyScope, film: Option[Int], edges: Seq[ResolverEdge],
                          ids: Set[String]): DecisionTrace.Node = {
      val accepted = bestOf.get(node.id).flatMap { case (best, _) => scope.acceptedBy(node, best.candidate.tmdbId) }
      val touching = edges.filter(edge => edge.a == node.id || edge.b == node.id)
      val joins    = touching.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).map(_.reason).distinct.sorted
      val apart    = touching.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map(_.reason).distinct.sorted
      val own      = scope.of(node)
      val weighed  = film.flatMap(id => own.find(_.candidate.tmdbId == id)).orElse(own.headOption)
      val withdrawn = if (accepted.isDefined) None else withdrawnOf(node, scope)
      val refusals = if (accepted.isDefined) Nil else withdrawn.toSeq ++ acceptance.refusals(own)
      val searched = if (accepted.isDefined) Nil else searchedBy(node)
      // What it weighed: for a node no rule took, its five best candidates; for one a rule took, the runner-up it beat.
      val shown    = if (accepted.isDefined) own.filterNot(scored => weighed.exists(_ eq scored)).take(1) else own.take(5)
      val blocker  = Option.when(accepted.isEmpty)(withdrawn.map(_ => "withdrawn:sibling-denies-film")
        .getOrElse(DecisionTrace.blockerOf(own, searched.exists(_.endsWith(": unanswered")), refusals)))
      // the best other film it weighed, by probability: what the take beat (or, taking none, its next best)
      val rival    = own.iterator.filterNot(scored => weighed.exists(_ eq scored)).maxByOption(_.probability)
      DecisionTrace.Node(accepted, joins, apart, weighed.fold(Map.empty[String, IdentityMeasures.Measure])(_.measures),
        weighed.map(_.candidate.tmdbId), refusals, searched, shown.map(DecisionTrace.renderCandidate), blocker,
        weighed.map(_.probability), rival.map(scored => DecisionTrace.Rival(scored.candidate.tmdbId, scored.probability)))
    }

    /** A match the node's own rules took, withdrawn because a title-linked sibling's evidence denies that film. */
    private def withdrawnOf(node: EvidenceNode, scope: FamilyScope): Option[DecisionTrace.Refusal] =
      scope.takenAlone(node).flatMap { case ((best, _), _) =>
        families.deniedBySibling(node, scope.members, scope, best, id => scope.takenAlone(nodeById(id)).map(_._1._1))
          .map { case (sibling, denial) => DecisionTrace.Refusal("withdrawn", "a title-linked sibling's own evidence denies its film",
            Some(best.candidate.tmdbId), s"${sibling.label}: $denial") }
      }

    /** What a node no rule took searched: each query with what it found — enough to tell a search that found nothing from
     *  a veto from a rule that would not take it. Strings, like the refusals, so nothing scored is kept. */
    private def searchedBy(node: EvidenceNode): Seq[String] =
      queriesOf(node.id).map(query => s"${DecisionTrace.renderQuery(query)}: " +
        answers.get(query).flatMap(_.answered).fold("unanswered")(hits => s"${hits.size} film(s)"))

    /** The pooled rule, when the pooled scoring decided. */
    private def pooledRule(scored: Seq[Scored], film: Option[Int], basis: ResolverDecision.Basis): Option[String] =
      Option.when(basis == ResolverDecision.Basis.PooledMatch)(acceptance.pooledNamed(scored).collect {
        case ((accepted, _), rule) if film.contains(accepted.candidate.tmdbId) => rule }).flatten

    /** Which member's own evidence denied the vetoed best film, and why. */
    private def vetoOf(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int]): Option[DecisionTrace.Veto] =
      scored.headOption.filter(best => best.denied && !film.contains(best.candidate.tmdbId)).map { best =>
        val by = cluster.find(node => scope.of(node).exists(own => own.candidate.tmdbId == best.candidate.tmdbId && own.denied))
        DecisionTrace.Veto(by.flatMap(node => scope.of(node).find(_.candidate.tmdbId == best.candidate.tmdbId)).flatMap(_.denial)
          .orElse(best.denial).fold("denied")(_.why), by.map(_.name))
      }
  }
}
