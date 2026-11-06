package services.identity
package adapters

/**
 * The model's input gate over the film databases' families ([[ListingReadiness]]; docs/design/identity-structural-fixes.md
 * fix 1): a listing waits on every question the agreement stage would ask of its own facts (`questions`, the stage's
 * [[agreement.AgreementStage.questionsOf]]) until each is answered -- filed in the families' store, so the stage reads it
 * known -- or GIVEN UP on: the question memory holds a durable non-answer, or failures its kind gives up on
 * ([[QuestionMemory.concluded]]). An ask still inside its retries holds
 * the listing; no clock of ours ends the wait, the answers' own timeouts and retries do (each ask terminates: a turned-away
 * ask is a failed one, a family with no source is filed as no film).
 *
 * Asked through the question queue, claimed ahead of the agreement's own questions (`enqueue`): a new film's questions are
 * asked straight away (the owner, 2026-10-23), and the listing is taken in once their answers' filings wake the model.
 */
final class AgreementReadiness(questions: Listing => Set[Question], memory: QuestionMemory, enqueue: Set[Question] => Unit)
    extends ListingReadiness {

  def awaiting(listing: Listing): Set[Question] = questions(listing).filterNot(memory.concluded)

  def ask(listing: Listing, waiting: Set[Question]): Unit = if (waiting.nonEmpty) enqueue(waiting)
}
