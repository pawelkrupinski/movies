package services.identity

import java.time.Clock
import scala.concurrent.duration._

/**
 * Which of the model's questions to ask TMDB again because their answer has aged — the one staleness
 * TMDB's change lists cannot report: a brand-new film a stored search or director walk should now
 * return. A question the model has not settled (a family with no film, or one below
 * [[TmdbRefreshes.Settled]]) is due a day after it was last asked; every other one a week after —
 * so a new film reaches every stored search within a week, and the searches still deciding nothing
 * within a day. Oldest first; a question the store does not hold is a gap, the fill's own.
 */
final class TmdbRefreshes(store: TmdbStore, language: String, clock: Clock) {
  import TmdbRefreshes._

  def due(questions: Seq[(Set[CandidateQuery], Boolean)]): Seq[CandidateQuery] = {
    val settledOf = questions.flatMap { case (qs, settled) => qs.map(_ -> settled) }
      .groupMapReduce(_._1)(_._2)(_ && _)                     // a question of any unsettled family is unsettled
    val ids  = settledOf.keys.map(q => q -> TmdbStore.questionId(language, q)).toMap
    val held = store.get(TmdbKind.Query, ids.values.toSeq)
    val now  = clock.millis()
    settledOf.toSeq.flatMap { case (query, settled) =>
      held.get(ids(query)).flatMap(TmdbStore.fetchedAt).orElse(held.get(ids(query)).map(_ => 0L))
        .filter(at => at < now - (if (settled) SettledAge else UnsettledAge).toMillis).map(query -> _)
    }.sortBy { case (query, at) => (at, query.sortKey) }.map(_._1)
  }
}

object TmdbRefreshes {
  /** A decision this sure of its film is settled: its questions are asked again only weekly. */
  val Settled      = 0.9
  val UnsettledAge: FiniteDuration = 1.day
  val SettledAge:   FiniteDuration = 7.days

  /** Each family's questions, and whether every decision in it is settled. */
  def of(families: Iterable[(Set[CandidateQuery], Seq[ResolverDecision])]): Seq[(Set[CandidateQuery], Boolean)] =
    families.map { case (queries, decisions) => queries -> decisions.forall(d => d.film.isDefined && d.confidence >= Settled) }.toSeq
}
