package scripts

import models.Country
import services.identity._
import services.identity.adapters.{AgreementQuestions, FamilyAnswerStore, FamilySource, PosterAnswerStore, PosterHashing, TmdbDocuments}
import services.identity.agreement.{AgreementStage, AgreementStageComposition, VoterFamily}
import tools.CatalogueFill

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

/**
 * What one SETTLED agreement stage reads its answers from, and how its open questions get answered: the family answers
 * filed in `docs` first (a [[FamilyAnswerStore]] over them, keyed as production's `identity_family_answers`), the
 * [[StageAnswers.Sources]] for the rest — a recording's sites (`IdentityDrift.recorded`), or the real sites asked live
 * (`services.identity.WhatIfAnswers`). The sources differ only at that seam: every question is asked, filed, attempted
 * and given up on here, as production's queue and memory do it.
 */
class StageAnswers(answering: StageAnswers.Sources, docs: TmdbDocuments, clock: java.time.Clock,
                   /** An IMDb id's TMDB film as a store holds its find, ahead of the sources' ([[IdentityDrift.WholeLookups.found]]). */
                   found: String => Option[Option[Int]]) {
  val store     = new FamilyAnswerStore(docs, clock)
  val posters   = new PosterAnswerStore(store, clock)
  val catalogue = new CatalogueFill(store, clock, answering.fetch)
  private val hashing = new PosterHashing(new services.sharecards.EgressPosterDownload(answering.fetch), new services.sharecards.VipsPosterShrinker(binary = None),
    answering.posterImages, answering.country.language.getLanguage)
  private val failures = new ConcurrentHashMap[(VoterFamily, String), Integer]()
  /** The family questions an ask of this stage filed: answered, given up on by nothing. */
  private val answered = ConcurrentHashMap.newKeySet[(VoterFamily, String)]()
  /** How many attempts were marked: production's memory's marks (`QuestionMemory.marks`), which the stage reads to
   *  tell an attempt since its last apply. */
  private val marks = new java.util.concurrent.atomic.AtomicLong
  /** What a round tried to answer and could not: attempted, as production's memory records a failed ask — a cluster
   *  waiting on it settles ([[AgreementStage.settles]]). An answer filed is attempted by itself. */
  def attempted(question: Question): Boolean = question match {
    case Question.Family(family, asked) => failures.containsKey((family, asked))
    case _                              => false
  }
  /** Asked and come to an outcome -- answered (filed, or a durable non-answer) or failed: production's memory's
   *  `attempted`, what the model's input gate waits on (`IdentityDrift.Worker`). */
  def tried(question: Question): Boolean = question match {
    case Question.Family(family, asked) =>
      answered.contains((family, asked)) || failures.containsKey((family, asked)) || released.contains((family, asked))
    case _                              => false
  }
  /** Answered or given up on, as production's memory reads it (`QuestionMemory.givenUp`): a family question that
   *  failed fewer than [[tools.AgreementQuestionRounds.Attempts]] times in a row is in its backoff, and a take settled
   *  past it is Pending until it is filed or unreachable (the owner, 2026-10-31). */
  def givenUp(question: Question): Boolean = question match {
    case Question.Family(family, asked) => StageAnswers.givenUp(answered.contains((family, asked)), Option(failures.get((family, asked))).map(_.intValue))
    case _                              => true
  }
  def unreachable: Set[(VoterFamily, String)] = failures.asScala.filter(_._2 >= tools.AgreementQuestionRounds.Attempts).keySet.toSet
  def tmdbOf(imdb: String): Answer[Option[Int]] = found(imdb).fold(answering.find(imdb))(Answer.Known(_))
  def stage(lookups: IdentityLookups, verdicts: services.identity.agreement.AgreementVerdicts, budgets: Boolean,
            keys: services.identity.agreement.AgreementKeys = new services.identity.agreement.InMemoryAgreementKeys,
            askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never): AgreementStage =
    AgreementStageComposition(AgreementStageComposition.Sources(answering.families.map(f => f -> store.answers(f)).toMap,
      venuePages = lookups, tmdb = lookups, tmdbOf = tmdbOf, stored = verdicts, clock = clock, normalizer = answering.normalizer,
      identities = modules.wiring.IdentityProjectionWiring.identities(answering.country.code),
      listedOn = modules.wiring.IdentityProjectionWiring.listedOn(answering.country.code), changes = store, posters = posters,
      catalogue = catalogue.answers, attempted = attempted, givenUp = givenUp, attemptsMarked = Some(() => marks.get), keys = keys),
      AgreementStageComposition.Effects(askNow = askNow), overrides = if (budgets) Set.empty else Set(AgreementStageComposition.Override.Unbudgeted))
  /** The family questions the stage's last apply left open, none given up as unreachable. */
  def askable(stage: AgreementStage): Set[(VoterFamily, String)] =
    Question.families(stage.wanted) -- unreachable
  /** Whether the stage's last apply left something to answer. */
  def open(stage: AgreementStage): Boolean =
    askable(stage).nonEmpty || stage.wanted.exists { case Question.Poster(_) | Question.Catalogue(_) => true; case _ => false }
  /** What the stage's last apply left open, for a settle round's line. */
  def described(stage: AgreementStage): String =
    s"${askable(stage).size} question(s), ${stage.wanted.count(_.isInstanceOf[Question.Poster])} poster(s), " +
      s"${stage.wanted.count(_.isInstanceOf[Question.Catalogue])} catalogue question(s) open, backlog ${stage.backlog}"
  /** One round of the queue's work: the stage's open questions answered — a held filed answer released, the rest
   *  asked of the sources — its posters hashed and its catalogue questions filed. */
  def answer(stage: AgreementStage, release: String => Boolean): Unit = {
    val open = StageAnswers.asks(store, askable(stage).toSeq, release)
    tools.ConcurrentAsks.each(stage.wanted.toSeq.collect { case Question.Poster(q) => q }, atOnce = 8)(q => posters.file(q, Try(hashing.of(q)).getOrElse(Nil)))
    catalogue.file(stage.wanted.collect { case Question.Catalogue(q) => q })
    file(open)
  }
  /** The family questions a stage asks INLINE ([[AgreementStage.AskNow]]): answered before its apply reads them again,
   *  as the worker's inline asks answer them ([[services.identity.adapters.InlineFamilyAsks]]) — else a take the worker
   *  reads its answers for at its first apply waited here for the queue's next round, served the model's film meanwhile. */
  def inline(release: String => Boolean): AgreementStage.AskNow = (questions, _) => fileNow(questions, release)
  /** `questions`' family questions answered now -- a held filed answer released, the rest asked of the sources: an
   *  inline ask's, and the input gate's asks ahead of the queue (`IdentityDrift.Worker`). */
  def fileNow(questions: Set[Question], release: String => Boolean): Unit = {
    val open = (Question.families(questions) -- unreachable).toSeq.sortBy { case (f, q) => (f.ordinal, q) }
    // a question whose held filed answer `release` files is answered by that answer: attempted, as the worker's memory has it
    file(StageAnswers.asks(store, open, release, onReleased = question => { released.add(question); () }))
  }
  /** The family questions a [[fileNow]] answered by releasing their held filed answer. */
  private val released = ConcurrentHashMap.newKeySet[(VoterFamily, String)]()
  /** One family question filed as a round files it, unless the store holds it already: what a sub-population's
   *  settle asked, read again alone ([[services.identity.agreement.FamilyFinalReads.AnswerOracle]]). */
  def fileOne(family: VoterFamily, asked: String): Unit =
    if (store.wanted(FamilyAnswerStore.questionId(family, asked))) file(Seq(family -> asked))
  private def file(open: Seq[(VoterFamily, String)]): Unit =
    tools.ConcurrentAsks.inLanes(open.groupBy(_._1).values, 4) { case question @ (family, asked) =>
      try { AgreementQuestions.file(store, family, answering.sources(family), asked); answered.add(question); () }
      catch { case NonFatal(_) => failures.merge(question, 1, (a, b) => a + b); marks.incrementAndGet(); () }
    }
}

object StageAnswers {

  /** Where a stage's open questions are asked: the country's families and their sites, the fetch posters and the
   *  catalogue are read through, and TMDB's finds and posters. A recording's (`IdentityDrift.recorded`) or the live
   *  sites' (`services.identity.WhatIfAnswers`); a family missing from `sources` is a failed ask. */
  trait Sources {
    def country: Country
    def normalizer: services.movies.TitleNormalizer
    def families: Seq[VoterFamily]
    def sources: Map[VoterFamily, FamilySource]
    def fetch: tools.HttpFetch
    def posterImages(tmdbId: Int): Seq[clients.TmdbClient.PosterImage]
    def find(imdb: String): Answer[Option[Int]]
  }

  /** The replay's give-up of a family question, as production's memory's (`QuestionMemory.givenUp`): answered, never
   *  failed, or failed [[tools.AgreementQuestionRounds.Attempts]] times -- unreachable. One failed fewer times is in its
   *  backoff, and a take settled past it is Pending (the owner, 2026-10-31). */
  def givenUp(answered: Boolean, failures: Option[Int]): Boolean =
    answered || failures.forall(_ >= tools.AgreementQuestionRounds.Attempts)

  /** Which of the family questions `open` a round asks the sources: none `release` files (a held filed answer),
   *  nor one whose answer the store holds fresh — skipped, as production's asker skips it
   *  ([[services.identity.adapters.AgreementAsker]], `Fresh`). A wait lists the gaps it met while their answers were
   *  held, and keeps listing them once filed; asked again, the recording's answer replaced the filed one, which no whole
   *  resolve ever reads (PL "LALKA / DOLLY", seed 2, run 3154: RT's filed "Lalka Dolly" search read again as no hits). */
  def asks(store: FamilyAnswerStore, open: Seq[(VoterFamily, String)], release: String => Boolean,
           /** Told each question `release` answered. */
           onReleased: ((VoterFamily, String)) => Unit = _ => ()): Seq[(VoterFamily, String)] =
    open.filter { case asked @ (family, question) =>
      val id = FamilyAnswerStore.questionId(family, question)
      if (release(id)) { onReleased(asked); false } else store.wanted(id)
    }

  /** Applies until the stage asks nothing and holds no backlog — answering between applies: what a round could not
   *  answer is attempted, and the clusters waiting on it settle as production's do. */
  def settle(stage: AgreementStage, apply: () => Resolution, answers: StageAnswers, clock: tools.MutableClock, release: String => Boolean,
             rounds: Int = 60, log: Boolean = true): Resolution = {
    var now = apply()
    var n   = 0
    def quiet = !answers.open(stage) && !stage.backlog
    while (!quiet && n < rounds) {
      n += 1
      if (log) System.err.println(s"[identity-drift] settle round $n: ${answers.described(stage)}")
      answers.answer(stage, release); clock.advance(java.time.Duration.ofMinutes(1)); now = apply()
    }
    if (!quiet) System.err.println(s"[identity-drift] the stage did not settle in $rounds rounds: ${answers.described(stage)}")
    now
  }

}
