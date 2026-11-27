package services.identity.agreement

import scala.collection.mutable

import services.identity.agreement.StageWaits.ClusterWait
import services.identity.measures.{Listing, PosterEvidence, ResolverDecision}
import services.identity.vocabulary.{Answer, Question, VoterFamily}

/** The stage's takes of a cluster the model left unmatched, one class a take kind: the film the families agree on
 *  ([[AgreedTake]]), else the first of [[AgreementTakes.FallThrough]] to take one — the venue posters' vote
 *  ([[PosterVoteTake]]), the broadcast its screening days name ([[BroadcastTake]]), a fill rule's ([[FillTake]]) and its
 *  catalogue ids' ([[CatalogueTake]]); the agreed and the filled film each refused where a venue's synopsis names
 *  another's description ([[VenueText]]), the agreed one where its billing names the film as one piece beside another
 *  thing. Each reads through `evidence` ([[AgreementEvidence]]) and the cluster's `candidates`, the fill by the selected
 *  `rules`; a film TMDB holds no record of stands on the first of `identities` that names it, and the listings'
 *  catalogue ids are read from `catalogue`. */
private[agreement] final class AgreementTakes(evidence: AgreementEvidence, candidates: AgreementCandidates, rules: services.identity.agreement.UnifiedRules,
                                             identities: Seq[VoterFamily], catalogue: AgreementSources.CatalogueSource) {
  import AgreementTakes.taking

  private val venueText  = new VenueText(evidence, candidates)
  private val agreed      = new AgreedTake(evidence, venueText, identities)
  private val posterVotes = new PosterVoteTake(evidence)
  private val broadcasts  = new BroadcastTake(evidence, candidates)
  private val fills       = new FillTake(evidence, candidates, rules, venueText)
  private val catalogues  = new CatalogueTake(evidence, candidates, catalogue)

  /** The clusters whose agreed take the last apply left pending ([[AgreementTakes.Take.Pending]]: a venue poster not
   *  hashed, an IMDb id not found in TMDB yet), with what it asked: the model's take is served meanwhile, typed Pending
   *  ([[StageWaits.outcomeOf]]). UK "Hocus Pocus", PL "Siostry (1972)" (drift slices): agreed, then after a restart pending on the
   *  posters of the candidates it read again, the model's no film served. */
  val takeWaits = scala.collection.concurrent.TrieMap.empty[String, ClusterWait]

  /** Forget the pending takes of every cluster but those `seen`. */
  def forgetAllBut(seen: collection.Set[String]): Unit = takeWaits --= takeWaits.keySet.toSet -- seen

  /** The film the first selected fill rule takes for a cluster the families agreed on nothing for, filed with their
   *  verdict ([[ClusterVerdicts]]): the fill take's rule ([[FillTake.filledOf]]). */
  val filledOf = fills.filledOf

  /** What the cluster `id` (its sorted `listings` digesting to `digest`), a model no-match, is taken as on the families'
   *  `verdict` — the decision as the model left it while there is none. What its take asks — its posters, its finds — is
   *  kept apart, so a take pending on them names them ([[takeWaits]]), and noted in `pass`. */
  def verdictTake(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: Option[StoredVerdict],
                  pass: AgreementPass): ResolverDecision = {
    val asking = mutable.Set.empty[Question]
    takeWaits -= id
    // the venue posters' distances to the cluster's candidates and to the film the families agree on, read through the
    // apply's memo, as a correction's are: each poster decoded once an apply, not once a take
    val distances: AgreementTakes.Distances = also => candidates.posterDistances(id, digest, listings, also, asking, hashes = pass.reading.posters)
    // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
    val took = verdict.fold(decision)(v => takeOfVerdict(AgreementTakes.Unagreed(decision, id, digest, listings, v, distances, asking), pass))
    pass.asked ++= asking
    took
  }

  /** The take of the families' verdict on `cluster` ([[verdictTake]]): the film they agree on, unless its billing names it
   *  as one piece beside another thing, a venue poster vetoes it, the venue's synopsis refuses it, or an answer it waits
   *  on is not in (pending); else the first of [[AgreementTakes.FallThrough]] to take one, and the line saying why none did. */
  private def takeOfVerdict(cluster: AgreementTakes.Unagreed, pass: AgreementPass): ResolverDecision = {
    val AgreementTakes.Unagreed(decision, id, digest, listings, v, distances, asking) = cluster
    agreed.pieceOf(v, listings).fold(agreed.agreedTake(decision, id, digest, listings, v, asking, distances) match {
      // a screen adaptation agreed for a listing naming a stage work ([[Agreement.agreed]]) yields to the relay its
      // screening days name: PL Kino Amok's bare "Manon" on the Met's broadcast day is the Met's, not Clouzot's
      case AgreementTakes.Take.Taken(agreed) if listings.exists(services.identity.measures.ListingShape.stagesAWork) =>
        ruleTake(AgreementTakes.BroadcastTake, cluster).getOrElse(agreed)
      case AgreementTakes.Take.Taken(agreed) => agreed
      case AgreementTakes.Take.Pending       => takeWaits(id) = ClusterWait(digest, asking.toSet); decision
      case AgreementTakes.Take.Vetoed        => pass.posterVetoed += 1; fellWaiting(cluster)
      // the venue's synopsis naming a namesake's description says why, where nothing else takes one
      case AgreementTakes.Take.Refused(why)  =>
        val fell = fellWaiting(cluster)
        if (fell eq decision) taking(decision)(decision.basis, why) else fell
      // the guard that refused the film the families would agree on says why, where nothing else takes one
      case AgreementTakes.Take.Untaken       =>
        val fell = fellWaiting(cluster)
        v.refused.filter(_ => fell eq decision).fold(fell)(why => taking(decision)(decision.basis, why))
    })(line => taking(decision)(decision.basis, line))
  }

  /** The first of the fall-through takes to take a film for `cluster` ([[fellThrough]]) — and where none did while one of
   *  them waits on a question (a find, a poster, a broadcast's record), the cluster pending on it, as an agreed take is
   *  (PL "SEVENTEEN World Tour": filled by IMDb once TMDB's find of its id was filed, read as no film before it; the
   *  property "refine each take as answers are only filed"). */
  private def fellWaiting(cluster: AgreementTakes.Unagreed): ResolverDecision = {
    val fell = fellThrough(cluster)
    if ((fell eq cluster.decision) && cluster.asking.nonEmpty) takeWaits(cluster.id) = ClusterWait(cluster.digest, cluster.asking.toSet)
    fell
  }

  /** The takes a cluster the families' agreement took no film for is read by, by name — in [[AgreementTakes.FallThrough]]'s
   *  order, the first to take one deciding: each reads its own evidence and notes in the cluster's `asking` every question
   *  it waits on. */
  private[agreement] lazy val takes: Map[String, AgreementTakes.Unagreed => Option[ResolverDecision]] = Map(
    AgreementTakes.PosterVoteTake -> (u => posterVotes.voted(u.decision, u.distances(None))),
    AgreementTakes.BroadcastTake  -> (u => broadcasts.broadcast(u.decision, u.id, u.digest, u.listings, u.asking)),
    AgreementTakes.FillTake       -> (u => fills.filledTake(u.decision, u.id, u.digest, u.listings, u.verdict, u.asking, u.distances)),
    AgreementTakes.CatalogueTake  -> (u => catalogues.catalogued(u.decision, u.listings, u.asking)))
  private def ruleTake(rule: String, untaken: AgreementTakes.Unagreed): Option[ResolverDecision] = takes(rule)(untaken)
  private[agreement] lazy val fallThrough = AgreementTakes.FallThrough.map(takes)
  /** The first of [[AgreementTakes.FallThrough]] to take a film for the cluster, else the cluster as the model left it. */
  private def fellThrough(untaken: AgreementTakes.Unagreed): ResolverDecision =
    fallThrough.iterator.flatMap(_(untaken)).nextOption().getOrElse(untaken.decision)
}

private[identity] object AgreementTakes {
  /** The takes a cluster the families agreed on no film for (or one a venue poster vetoed) is read by, IN ORDER: the
   *  first to take a film decides, and none after it is read (nor asks a question). The order is behaviour:
   *  - [[PosterVoteTake]]: the one candidate a venue poster matches within [[PosterEvidence.VoteBits]], none vetoing it;
   *  - [[BroadcastTake]]: the one record of the billed stage work broadcast on a screening day ([[agreement.Broadcast]]);
   *  - [[FillTake]]: the first selected unified fill rule's film ([[services.identity.agreement.UnifiedRules.filled]]), unless a venue
   *    poster vetoes it;
   *  - [[CatalogueTake]]: the film the listings' own catalogue ids name ([[agreement.Catalogue]]). Last though an exact id:
   *    where it names another film than a take above, the projection keeps that take, for the measure to list.
   *  A film the families agree on for a listing billing a stage work yields to [[BroadcastTake]] first (PL Kino Amok's bare
   *  "Manon" on the Met's broadcast day is the Met's, not Clouzot's). `identity-resolver.md` §21 lists this order; a spec
   *  holds the two together. */
  val PosterVoteTake = "poster-vote"
  val BroadcastTake  = "broadcast"
  val FillTake       = "fill"
  val CatalogueTake  = "catalogue"
  val FallThrough: Seq[String] = Seq(PosterVoteTake, BroadcastTake, FillTake, CatalogueTake)

  /** A model no-match the families' agreement took no film for, as the takes read it: its cluster id, listings and their
   *  digest, the families' verdict, and its venue posters' distances to its candidates (and to `also`). */
  private[agreement] final case class Unagreed(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: StoredVerdict,
                                   distances: Distances, asking: mutable.Set[Question] = mutable.Set.empty)
  /** A cluster's venue posters' distances to its candidates and to the film given (an agreed or filled one): what a take
   *  is screened by ([[AgreementTakes.Take.screened]]). `Unknown` while a poster is not hashed. */
  private[agreement] type Distances = Option[Int] => Answer[Seq[Map[Int, Option[Int]]]]

  /** What became of the film a cluster's families agree on: taken, pending an answer, vetoed by a venue poster, refused by
   *  the venue's synopsis (and why), or none to take. */
  private[agreement] enum Take[+A] {
    case Taken(decision: A)
    case Pending
    case Vetoed
    case Refused(why: String)
    case Untaken
  }
  private[agreement] object Take {
    /** `take` of `film` as the venues' evidence lets it stand — what an agreed film and a filled one are both screened by:
     *  pending while a venue poster is not hashed (its `distances`), VETOED where one names another candidate
     *  ([[PosterEvidence.veto]]), REFUSED where the venue's synopsis names another's description (`synopsis`,
     *  [[VenueText.synopsisAgainst]]), else taken. */
    def screened[A](film: Option[Int], distances: Distances, synopsis: Int => Option[String])(take: => A): Take[A] =
      // a poster given up on (Failed) is no evidence for or against: as no distance found
      distances(film).failedAs(Nil) match {
        case Answer.Known(found) =>
          if (PosterEvidence.veto(film, found).nonEmpty) Take.Vetoed
          else film.flatMap(synopsis).fold[Take[A]](Take.Taken(take))(Take.Refused(_))
        case Answer.Unknown | Answer.Failed => Take.Pending // failedAs leaves no Failed
      }
  }

  /** `d` taken on `basis`, `line` explaining it — with the `film`, `fallback` and `agreed` ids it takes, where they move. */
  private[agreement] def taking(d: ResolverDecision)(basis: ResolverDecision.Basis, line: String, film: Option[Int] = d.film,
                                          fallback: Option[ResolverDecision.Fallback] = d.fallback, agreed: Map[String, String] = d.agreed,
                                          rule: Option[String] = None): ResolverDecision =
    d.copy(film = film, basis = basis, explanation = d.explanation :+ line, fallback = fallback, agreed = agreed)(
      rule.fold(d.trace)(r => d.trace.copy(stage = Some(r))))

  /** A fallback film standing on `source`'s `id`: one standing on another database's id than IMDb's carries its `title`
   *  and `year`, that a link to its page is built of. */
  private[agreement] def fallbackOn(source: String, id: String, title: Option[String], year: Option[Int]): ResolverDecision.Fallback = {
    val notImdb = source != VoterFamily.Imdb.database
    ResolverDecision.Fallback(source, id, 1.0, title.filter(_ => notImdb), year.filter(_ => notImdb))
  }
}
