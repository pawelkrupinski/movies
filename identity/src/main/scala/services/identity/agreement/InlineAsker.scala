package services.identity.agreement

import services.identity.vocabulary.Question

import scala.collection.mutable

/**
 * The agreement stage's INLINE ASKS (`askNow`): a cluster, or a model take's correction, met in a SHAPE (its listings'
 * digest) for the first time since the stage started — past its first pass, which meets every cluster — has its families
 * asked now, on the applying thread, so its first take reads their answers (docs/design/identity-questions.md, step 3).
 * Each shape is asked so once, whichever key names its cluster — a key only names (docs/design/identity-no-drift.md,
 * "One dependency index"): [[asked]] keeps the shapes asked, while a cluster of that shape is met.
 */
private[agreement] final class InlineAsker(askNow: AgreementStage.AskNow, clock: java.time.Clock) {
  /** Whether the apply under way is the stage's first: the one that meets every cluster, and asks none inline. */
  var firstPass: Boolean = true
  /** The SHAPES whose families were asked inline. */
  val asked = mutable.HashSet.empty[Long]

  /** May the SHAPE `digest` be asked inline: an inline ask is wired, the first pass is over, no verdict of that shape is
   *  `held` and no inline ask has met it yet? */
  def mayAsk(digest: Long, held: Boolean): Boolean =
    (askNow ne AgreementStage.AskNow.Never) && !firstPass && !held && !asked(digest)

  /** The INLINE ASK of a cluster or a take's correction met in a new shape: `first`, read, asks `firstAsking` now, reads
   *  again, and asks what that read leads to (`asking`, given what was asked) — a round each, asking only what no round
   *  before did, all rounds within [[AgreementStage.AskNowWithin]] and at most [[AgreementStage.InlineRounds]] of them.
   *  What the last read came to; a question still open there is handed to the queue as every other is. */
  def askedInline[A](first: A, firstAsking: Set[Question])(read: () => A)(asking: (A, Set[Question]) => Set[Question]): A = {
    val deadline = clock.millis() + AgreementStage.AskNowWithin.toMillis
    var askedNow = Set.empty[Question]
    var now      = first
    var next     = firstAsking
    var rounds   = 0
    while (next.nonEmpty && rounds < AgreementStage.InlineRounds && clock.millis() < deadline) {
      askedNow ++= next
      rounds += 1
      askNow(next, scala.concurrent.duration.Duration(deadline - clock.millis(), java.util.concurrent.TimeUnit.MILLISECONDS))
      now  = read()
      next = asking(now, askedNow)
    }
    now
  }
}
