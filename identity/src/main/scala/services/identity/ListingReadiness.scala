package services.identity

/**
 * The model's input gate (docs/design/identity-structural-fixes.md, fix 1: asking apart from deciding): which questions a
 * listing's own facts lead to that have no answer yet -- its venue page, and the questions its titles and directors put
 * to the film databases' families -- so a listing VERSION is taken into the model only once each is answered or given up
 * on. Until then the model reads the listing's last ready version, and a listing with none is shown on its own, Pending.
 * Every question is a function of the listing and the answers filed: the set is the same whatever else is billed (the
 * owner's population-independent inputs), and a whole resolve over the ready versions is the incremental one.
 */
trait ListingReadiness {
  /** The questions `listing` waits on: empty once it is ready. */
  def awaiting(listing: Listing): Set[Question]
  /** Ask `questions` (those [[awaiting]] named), ahead of the queue's other questions: a listing waits on them. */
  def ask(listing: Listing, questions: Set[Question]): Unit
}

object ListingReadiness {
  /** A listing's venue page as one of its questions ([[PageWait]]): the evidence its own facts are read from. */
  def ofPage(pages: PageWait): ListingReadiness = new ListingReadiness {
    def awaiting(listing: Listing): Set[Question] =
      if (pages.unattempted(listing)) Set(Question.VenuePage(listing.key)) else Set.empty
    def ask(listing: Listing, questions: Set[Question]): Unit =
      if (questions.contains(Question.VenuePage(listing.key))) pages.request(listing)
  }

  /** Every question each of `parts` names: a listing ready once it is ready by all of them. */
  def all(parts: ListingReadiness*): ListingReadiness = new ListingReadiness {
    def awaiting(listing: Listing): Set[Question]               = parts.iterator.flatMap(_.awaiting(listing)).toSet
    def ask(listing: Listing, questions: Set[Question]): Unit = parts.foreach(_.ask(listing, questions))
  }

  /** Every listing ready at once: a model run that asks nothing (specs, offline harnesses, replays). */
  val Always: ListingReadiness = new ListingReadiness {
    def awaiting(listing: Listing): Set[Question]               = Set.empty
    def ask(listing: Listing, questions: Set[Question]): Unit = ()
  }
}
