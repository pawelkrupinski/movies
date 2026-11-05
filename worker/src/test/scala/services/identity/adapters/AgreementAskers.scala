package services.identity
package adapters

import services.identity.agreement.VoterFamily
import services.sharecards.{PosterDownload, VipsPosterShrinker}
import services.tasks.{Task, TaskType}

import java.time.Clock

/** An [[AgreementAsker]] for a spec that asks one kind of question: the collaborators it names, and for the rest the
 *  families' store's posters and catalogue, no family sources, a TMDB, poster image or catalogue source that fails the
 *  spec if asked, and a non-answer memory of its own. */
object AgreementAskers {
  private def unasked(what: String): Nothing = throw new AssertionError(s"no $what asked")

  def over(families: FamilyAnswerStore, clock: Clock)(
      sources: Map[VoterFamily, FamilySource] = Map.empty,
      find: String => Unit = _ => unasked("TMDB find"),
      reread: Int => Unit = _ => unasked("TMDB record"),
      posters: PosterAnswerStore = new PosterAnswerStore(families, clock),
      hashing: PosterHashing = new PosterHashing(new PosterDownload { def fetch(url: String) = unasked("poster") },
        new VipsPosterShrinker(binary = None), _ => unasked("poster"), "pl"),
      catalogue: CatalogueAnswerStore = new CatalogueAnswerStore(families, clock, Nil),
      mapping: CatalogueMapping = _ => unasked("catalogue id"),
      pages: CatalogueLinkReader = new CatalogueLinkReader(Nil),
      filed: () => Unit = () => (),
      memory: QuestionMemory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock),
      metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent): AgreementAsker =
    new AgreementAsker(families, sources, find, reread, posters, hashing, catalogue, mapping, pages, filed, memory, metrics)

  /** The task asking `questions`, as [[AgreementQuestions.enqueue]] queues it, on its `attempts`th attempt. */
  def task(questions: Question*)(attempts: Int = 1): Task = {
    val payload = AgreementQuestions.payloadOf(questions)
    Task("t", TaskType.AskQuestion, s"agreement|${payload.values.mkString}", payload, attempts)
  }
}
