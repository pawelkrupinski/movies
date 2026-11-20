package services.identity.agreement

import services.identity.{Answer, PosterAnswers, PosterHash, PosterQuestion, Question, ResolverDecision, VoterFamily}

import scala.collection.mutable

/** One apply at the families' `version`: where its takes and corrections note what they wait on (`asked`: the families'
 *  questions, the IMDb ids TMDB was not asked about, the posters, the catalogue questions and the records to read again
 *  for their day) with the verdicts it moved, the clusters and model takes it read, and its cost. */
private[agreement] final class AgreementPass(val version: Long, corrections: Int, val reading: AgreementPass.Read, backfills: Int = Int.MaxValue) {
  val asked        = mutable.Set.empty[Question]
  /** The posters the model takes' corrections wait on: handed to the queue a few at a time ([[wanted]]). */
  val correctionPosters = mutable.Set.empty[PosterQuestion]
  val moved        = mutable.ArrayBuffer.empty[StoredVerdict]
  /** The clusters read, by id, and the model takes read against the evidence, by their correction's id. */
  val seen         = mutable.Set.empty[String]
  val correcting   = mutable.Set.empty[String]
  /** The shapes (listings' digests) of the clusters read. */
  val shapes       = mutable.Set.empty[Long]
  /** The new clusters whose families this apply asked inline ([[AgreementStage.askNow]]), with when and what. */
  val inline       = mutable.Map.empty[String, (Long, Set[Question])]
  /** Each decision's cluster key ([[ClusterKeys.assign]]), and the keys a merge absorbed, with the survivor's. */
  val keys         = new java.util.IdentityHashMap[ResolverDecision, String]()
  val merged       = mutable.ArrayBuffer.empty[(String, String)]
  /** What the apply's full validation of the kept verdicts came to ([[AgreementStage.FullValidationEvery]]). */
  var validated    = services.identity.Memo.Validation.None
  private var cleanHits = 0
  private var validateAt, validateUntil = 0
  /** A full validation of the clean verdicts this apply meets from the `from`th, `budget` of them. */
  def validateFrom(from: Int, budget: Int): Unit = { validateAt = from; validateUntil = from + budget }
  /** Is the clean verdict met now one the apply's full validation reads again? Each asking counts one met. */
  def validating(): Boolean = { val at = cleanHits; cleanHits += 1; at >= validateAt && at < validateUntil }
  /** How many clusters the resolver was asked about again — the apply's cost — and how many agreed films a poster vetoed. */
  var resolves     = 0
  var posterVetoed = 0
  /** How long each previous take that stopped standing this apply stood ([[StandingTakes]]): its answers in, or its bound. */
  val holdsEnded   = mutable.ArrayBuffer.empty[Double]
  val holdsBounded = mutable.ArrayBuffer.empty[Double]
  private val freshReads    = new AgreementPass.Budget(corrections)
  private val backfillReads = new AgreementPass.Budget(backfills)
  /** May one more model take be read afresh ([[CorrectionsPerApply]])? Each asking spends one of the budget. */
  def fresh(): Boolean = freshReads.spend()
  /** The clusters whose verdict decided under other rules this apply decided again, by id. */
  val backfilled = mutable.Set.empty[String]
  /** May one more verdict decided under other rules be decided again ([[BackfillPerApply]])? Each asking spends one. */
  def backfill(): Boolean = backfillReads.spend()
  /** Did a take or a verdict go unread for want of budget: asked once the budget was spent? */
  def spent: Boolean = freshReads.overdrawn || backfillReads.overdrawn
  /** What the apply met unanswered: the corrections' posters only `postersAtOnce` at a time, the backfill of every
   *  take's posters draining behind every other task — those not `tried` yet first, so posters that keep failing hold
   *  no slot another poster could use. A poster any take asks AGAIN — `tried` already: its hashes stale, its cap raised
   *  since, a failure retried — waits on none (the settle rule reads what is filed) and drains with them: a renewal of
   *  thousands at once (a cap raised, 2026-11-11: ~8.7k films over the five countries) spread over applies, never one
   *  burst on the queue. */
  def wanted(postersAtOnce: Int, tried: Question => Boolean): Set[Question] = {
    val (renewed, rest) = asked.partition { case question @ Question.Poster(_) => tried(question); case _ => false }
    val draining = correctionPosters ++ renewed.iterator.collect { case Question.Poster(poster) => poster }
    val capped = if (draining.sizeIs <= postersAtOnce) draining
      else draining.toSeq.sortBy(poster => (tried(Question.Poster(poster)), PosterAnswers.idOf(poster))).take(postersAtOnce)
    rest.toSet ++ capped.iterator.map(Question.Poster(_))
  }
}

private[agreement] object AgreementPass {
  /** One apply's allowance of some costly read: each [[spend]] asks for one, granted while any is left; [[overdrawn]] once
   *  one was asked for past it — a read left for a later apply. */
  final class Budget(allowance: Int) {
    private var left = allowance
    def spend(): Boolean = { val granted = left > 0; left -= 1; granted }
    def overdrawn: Boolean = left < 0
  }

  /** `posters` and `programmes` (the family whose site lists the venues' programmes), each answer decoded once: an
   *  apply's reads, dropped with it. A poster's hashes, whether it is fresh and whether it was given up on are each read
   *  of the store once an apply, however many takes and corrections compare it (each a document read and decode). */
  final class Read(hashes: PosterAnswers, family: Option[FamilyAnswers]) {
    private val venues = mutable.HashMap.empty[String, Answer[Option[PosterHash]]]
    private val films  = mutable.HashMap.empty[Int, Answer[Seq[PosterHash]]]
    private val freshness, givenUp = mutable.HashMap.empty[PosterQuestion, Boolean]
    val posters: PosterAnswers = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = venues.getOrElseUpdate(url, hashes.venue(url))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = films.getOrElseUpdate(tmdbId, hashes.film(tmdbId))
      override def unread(question: PosterQuestion): Boolean = givenUp.getOrElseUpdate(question, hashes.unread(question))
      override def fresh(question: PosterQuestion): Boolean  = freshness.getOrElseUpdate(question, hashes.fresh(question))
    }
    val programmes: Option[FamilyAnswers] = family.map { answers =>
      val showings = mutable.HashMap.empty[String, Answer[Seq[Showing]]]
      val records  = mutable.HashMap.empty[String, Answer[Option[SourceRecord]]]
      new FamilyAnswers {
        val family: VoterFamily = answers.family
        def titled(text: String): Answer[Seq[SourceHit]]     = answers.titled(text)
        def directedBy(name: String): Answer[Seq[SourceHit]] = answers.directedBy(name)
        def record(id: String): Answer[Option[SourceRecord]] = records.getOrElseUpdate(id, answers.record(id))
        override def showing(venue: String): Answer[Seq[Showing]] = showings.getOrElseUpdate(venue, answers.showing(venue))
        override def fresh(question: String): Boolean = answers.fresh(question)
      }
    }
  }
}
