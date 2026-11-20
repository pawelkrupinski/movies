package services.identity.agreement

import scala.collection.concurrent.TrieMap

import services.identity.measures.{ResolverDecision, WaitingOn}
import services.identity.vocabulary.{Dependency, Question}

/** What the stage waits on, cluster by cluster, as the last [[AgreementStage.apply]] left it: the waits it keeps —
 *  `waiting` (a cluster's families' questions or its fill's TMDB answers), `takeWaits` (an agreed take pending on its
 *  posters or finds) and `correctionWaits` (a model take's correction's questions), written by the applying thread — and
 *  what a decision comes to while they stand ([[waitingOn]], [[outcomeOf]]), read from any thread. The stage owns the
 *  maps and decides what goes in them; this reads them, beside the previous takes standing meanwhile (`standingTakes`)
 *  and the verdicts kept (`held`, by key: read only once something asks, loading them being the first apply's), each
 *  cluster named by `keyOf` ([[ClusterKeys.keyOf]]). */
final class StageWaits(waiting: TrieMap[String, ClusterWait], takeWaits: TrieMap[String, ClusterWait],
                       correctionWaits: TrieMap[String, ClusterWait], keyOf: ResolverDecision => String,
                       standingTakes: StandingTakes, digested: java.util.IdentityHashMap[ResolverDecision, AgreementEvidence.Cluster],
                       held: String => Option[StoredVerdict], readNow: Dependency => Option[Long],
                       tried: Question => Boolean, givenUp: Question => Boolean) {

  /** What `decision`'s take waits on, as the last [[AgreementStage.apply]] left it: the model's gaps, and the stage's own —
   *  its cluster's family questions or its fill's TMDB answers (`waiting`), and its take's correction's questions
   *  (`correctionWaits`). Read off the kept waits: a decision the stage does not wait on allocates nothing past its own. */
  def waitingOn(decision: ResolverDecision): WaitingOn =
    if (decision.members.isEmpty) WaitingOn.model(decision)
    else {
      val id         = keyOf(decision)
      val cluster    = waiting.get(id)
      val correction = correctionWaits.get(AgreementStage.correctionId(id))
      val hold       = standingTakes.holdOf(id, decision.members)
      if (cluster.isEmpty && correction.isEmpty && hold.isEmpty) WaitingOn.model(decision)
      else WaitingOn(
        decision.unanswered,
        // a question attempted is waited on no more: asked still, the take decided without it
        (cluster.fold(Set.empty[Question])(_.gaps) ++ correction.fold(Set.empty[Question])(_.gaps) ++
          hold.fold(Set.empty[Question])(_.waits)).filterNot(tried),
        onTmdb = cluster.exists(_.blindAt.isDefined))
    }

  /** What `decision` comes to as the last [[AgreementStage.apply]] served it, typed: its film, or no film -- Pending while
   *  the model or the stage still waits on something ([[ResolverDecision.awaits]], [[waitingOn]]: a verdict's or a
   *  correction's questions unanswered, a fill's TMDB answers), NoFilm when nothing is. Read, never stored: the served
   *  decision stays the object the projection diffs by. */
  def outcomeOf(decision: ResolverDecision): services.identity.measures.TakeOutcome = {
    // every question its verdict, its take and its correction still lack -- an attempted one too: the settle rule decides
    // without an answer whose ask failed (`tried`), but a failure in its backoff is still pending, not absent (P1). Served as
    // the stage serves it; typed as what it is. A film taken while its correction still waits is Pending too: the
    // correction may move it (the property "refine each take as answers are only filed": Lalka 1001 -> 1002)
    val stage = if (decision.members.isEmpty) Set.empty[Question] else stageWaits(keyOf(decision), decision)
    decision.outcome match {
      case taken: services.identity.measures.TakeOutcome.Taken => if (stage.isEmpty) taken else services.identity.measures.TakeOutcome.Pending(stage)
      case model                                  => if (stage.isEmpty) model else services.identity.measures.TakeOutcome.Pending(decision.awaits ++ stage)
    }
  }

  /** Every question the stage holds `decision`'s take of cluster `id` pending on ([[outcomeOf]]): a hold's, its
   *  cluster's and its correction's waits, a standing correction's re-reads ([[correctionRereads]]), its take's waits
   *  for its shape, and its settled verdict's questions none answered yet ([[settledUnanswered]]). */
  private def stageWaits(id: String, decision: ResolverDecision): Set[Question] =
    // a previous take standing while the cluster's take waits ([[StandingTakes]]): Pending on what it waits on
    standingTakes.holdOf(id, decision.members).fold(Set.empty[Question])(_.waits) ++
      waiting.get(id).fold(Set.empty[Question])(_.gaps) ++ correctionWaits.get(AgreementStage.correctionId(id)).fold(Set.empty[Question])(_.gaps) ++
      correctionRereads(id) ++
      takeWaits.get(id).filter(w => Option(digested.get(decision)).forall(_.digest == w.listings)).fold(Set.empty[Question])(_.gaps) ++
      settledUnanswered(id, decision)

  /** A correction standing unread -- the apply's budget spent, or stored and trusted at boot -- while an answer its
   *  receipt read was filed otherwise since: served as last decided, pending on its re-read (P3, a budget decides how
   *  soon, never what: PL "Lalka" stood on 1001 after a restart, its families' answers since naming 1002). */
  private def correctionRereads(id: String): Set[Question] =
    held(AgreementStage.correctionId(id)).fold(Set.empty[Question])(_.reads.iterator.collect {
      case (read, was) if readNow(read).exists(_ != was) => read
      // a question it asked and read unanswered, not given up on: settled past, pending (P1)
      case (read @ Dependency.Asked(question), _) if !givenUp(question) => read
    }.flatMap {
      case Dependency.Answer(question)       => Some(question)
      case Dependency.Attempted(question)    => Some(question)
      case Dependency.Lookup(_, _, question) => Some(question)
      case Dependency.Asked(question)        => Some(question)
      case _                                 => None
    }.toSet)

  /** A settled verdict's questions no family answered yet, unless given up on: asked on every pass, read again once
   *  answered (PL "SEVENTEEN World Tour 'NEW_'": settled to none past a failed ask, filled by IMDb once it filed). */
  private def settledUnanswered(id: String, decision: ResolverDecision): Set[Question] =
    Option(digested.get(decision)).flatMap(d => held(id).filter(_.listings == d.digest)).fold(Set.empty[Question])(v =>
      v.reads.iterator.collect {
        case (Dependency.Answer(q), Dependency.Unanswered) => q
        case (Dependency.Asked(q), _)                      => q
      }.filterNot(givenUp).toSet)

  /** What cluster `id` waits on now, each set not empty: its families' questions or fill (`waiting`), its take's
   *  (`takeWaits`) and its correction's (`correctionWaits`). */
  def openWaits(id: String): Seq[Set[Question]] =
    Seq(waiting.get(id), takeWaits.get(id), correctionWaits.get(AgreementStage.correctionId(id))).flatten.map(_.gaps).filter(_.nonEmpty)

  /** The clusters the stage waits on now: on their families' questions or fill (`waiting`), or on their take's
   *  correction (`correctionWaits`), by cluster id. */
  def waitingClusters: Set[String] =
    waiting.keySet.toSet ++ correctionWaits.keySet.iterator.map(_.stripPrefix(AgreementStage.CorrectionPrefix))

  /** What the stage waits on for cluster `id`: its families' questions and its correction's. */
  def questionsOf(id: String): Set[Question] =
    waiting.get(id).fold(Set.empty[Question])(_.gaps) ++ correctionWaits.get(AgreementStage.correctionId(id)).fold(Set.empty[Question])(_.gaps)
}
