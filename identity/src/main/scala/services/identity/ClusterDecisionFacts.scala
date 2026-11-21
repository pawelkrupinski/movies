package services.identity

import services.identity.measures.{EvidenceNode, IdentityMeasures, ResolverDecision}
import services.identity.vocabulary.Question

/** What one cluster's decision reads: its film, its pooled scoring, the confidence and basis they make, and what it
 *  falls back to, leans to or waits on. */
private[identity] final class ClusterDecisionFacts(scoring: CandidateScoring, families: Families, acceptance: Acceptance,
                                          val cluster: Seq[EvidenceNode], val scope: FamilyScope, val film: Option[Int],
                                          accepted: Map[String, Int], val votes: Map[String, ResolverDecisions.Vote]) {
  import scoring.generation.{awaitsAnswer, queriesOf}
  import families.pinnedFilm

  private val pinned   = film.isDefined && cluster.exists(node => pinnedFilm.contains(node.id))
  val scored   = scope.pooled(cluster)
  private val eligible = scored.filterNot(_.denied)
  private val ownMatch = cluster.exists(node => accepted.contains(node.id))
  val unknown  = cluster.flatMap(node => queriesOf(node.id)).distinct.count(query => awaitsAnswer(query))

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
    else noFilmBasis

  /** Why a cluster takes no film: nothing to weigh (a search still unanswered, or none naming a candidate), its best
   *  candidate denied, or none rated high enough. */
  private def noFilmBasis =
    if (scored.isEmpty) (if (unknown > 0) ResolverDecision.Basis.NoEvidence else ResolverDecision.Basis.NoCandidate)
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
    reads.flatMap(node => queriesOf(node.id)).distinct.filter(query => awaitsAnswer(query)).map(Question.Candidate(_)).toSet ++
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
    if (film.isDefined || scored.takeWhile(_.denied).exists(_.denial.exists(_.isReleaseVeto))) None
    else eligible.headOption.filter(best => best.untaken.isEmpty && !copiedOnly(best.candidate.tmdbId)).map(_.candidate)
      .filterNot(best => deniedByMember(best.tmdbId))
      .map(best => ResolverDecision.Leaning(best.tmdbId, best.film.imdbNumber))

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
        taken.candidate.fallbackRecord.map { case (source, imdbId) => ResolverDecision.Fallback(source, imdbId, taken.probability) }
      case _ => None
    }
  }
}
