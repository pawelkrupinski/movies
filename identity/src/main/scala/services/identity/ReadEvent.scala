package services.identity

import services.identity.measures.Listing
import services.identity.vocabulary.{Answer, DetailFacts, Dependency}

/** One read a resolve made of the seams it is handed, as a recording wrapper ([[RecordingLookups]], [[agreement.RecordingAnswers]])
 *  reports it: what was asked and the answer it got, for a receipt, a trace or a kept verdict to digest as it needs
 *  ([[Dependency.digest]]). */
sealed trait ReadEvent
object ReadEvent {
  /** A venue's detail page of `listing`, and what read of it. */
  final case class DetailAnswered(listing: Listing, answer: Answer[Option[DetailFacts]]) extends ReadEvent
  /** Whether `listing`'s venue publishes a detail page. */
  final case class DetailPresence(listing: Listing, has: Boolean) extends ReadEvent
  /** Any other lookup (a TMDB search or record), by the [[Dependency]] naming it. */
  final case class LookupAnswered(dependency: Dependency, answer: Answer[?]) extends ReadEvent
  /** A family's answer to `question` (`title|…`, `director|…`, `record|…`, `showing|…`). */
  final case class FamilyAnswered(question: String, answer: Answer[?]) extends ReadEvent
}
