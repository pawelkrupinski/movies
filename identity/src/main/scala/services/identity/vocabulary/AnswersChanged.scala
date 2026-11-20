package services.identity.vocabulary

import models.ListingKey

/** Which lookups gained an answer since the model last read them — candidate queries, film
 *  records, and the listings whose detail page did. */
final case class AnswersChanged(queries: Set[CandidateQuery], films: Set[Int], details: Set[ListingKey] = Set.empty) {
  def isEmpty: Boolean = queries.isEmpty && films.isEmpty && details.isEmpty
}

object AnswersChanged { val Empty: AnswersChanged = AnswersChanged(Set.empty, Set.empty) }
