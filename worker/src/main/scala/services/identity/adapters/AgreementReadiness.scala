package services.identity
package adapters

/**
 * The model's input gate over the film databases' families ([[ListingReadiness]]; docs/design/identity-structural-fixes.md
 * fix 1): a listing waits on every question the agreement stage would ask of its own facts (`questions`, the stage's
 * [[agreement.AgreementStage.questionsOf]]) until each was ATTEMPTED once (`attempted`: the worker's
 * [[QuestionMemory.attempted]], the drift replay's own answers') -- answered, failed or given up on alike: the gate waits
 * on the settle rule, never on a give-up, and [[ModelInputGate.MaxHold]] bounds what the queue does not reach.
 *
 * A question a host's open breaker or the fleet's pace TURNED AWAY ([[QuestionMemory.turnedAway]]) is attempted too, by
 * design (the owner, 2026-11-07): the listing is taken in on what it has, Pending on the question, which is asked again on
 * its backoff. Held instead, every new version of a broken source's listings would stall until the source came back.
 *
 * Asked through the question queue, claimed ahead of the agreement's own questions (`enqueue`): a new film's questions are
 * asked straight away (the owner, 2026-10-23), and the listing is taken in once their answers' filings wake the model.
 */
final class AgreementReadiness(questions: Listing => Set[Question], attempted: Question => Boolean, enqueue: Set[Question] => Unit)
    extends ListingReadiness {

  def awaiting(listing: Listing): Set[Question] = questions(listing).filterNot(attempted)

  def ask(listing: Listing, waiting: Set[Question]): Unit = if (waiting.nonEmpty) enqueue(waiting)
}
