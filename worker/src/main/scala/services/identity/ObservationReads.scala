package services.identity

import services.movies.ListingKey

import scala.collection.mutable

/**
 * Which store observations each of a model's questions read — a candidate query, a film's record,
 * a listing's detail page — so that when the store files new content under a key (the fill, the
 * pipeline's own lookups: `ObservationStore.onNewLookup`), the model re-reads exactly the
 * questions that read it ([[changedBy]]).
 *
 * Filled while the model asks ([[TrackedLookups]] names the question, the store's readers name the
 * keys); read on the same thread. A question read again is filed again, so a key it no longer
 * reads is dropped when its question is next asked ([[forget]]).
 */
class ObservationReads {
  private var asking: Option[ObservationReads.Question] = None
  private val readers = mutable.HashMap.empty[String, Set[ObservationReads.Question]]
  private val keysOf  = mutable.HashMap.empty[ObservationReads.Question, Set[String]]

  /** Run `body` as the asking of `question`: every key read meanwhile is that question's. */
  def asking[A](question: ObservationReads.Question)(body: => A): A = {
    forget(question)
    val outer = asking
    asking = Some(question)
    try body finally asking = outer
  }

  /** The store read `key` for the question being asked, if any. */
  def read(key: String): Unit = asking.foreach { question =>
    readers.updateWith(key)(held => Some(held.getOrElse(Set.empty) + question))
    keysOf.updateWith(question)(held => Some(held.getOrElse(Set.empty) + key))
  }

  /** Drop what `question` read: it is being asked anew. */
  def forget(question: ObservationReads.Question): Unit =
    keysOf.remove(question).foreach(_.foreach(key => readers.updateWith(key)(_.map(_ - question).filter(_.nonEmpty))))

  /** The questions whose answers new content under `keys` can change. */
  def changedBy(keys: Iterable[String]): AnswersChanged = {
    val questions = keys.flatMap(readers.getOrElse(_, Set.empty)).toSet
    AnswersChanged(
      questions.collect { case ObservationReads.Question.Query(query) => query },
      questions.collect { case ObservationReads.Question.Record(id) => id },
      questions.collect { case ObservationReads.Question.Detail(listing) => listing })
  }

  /** How many keys it tracks: the index a model's questions keep over the store. */
  def keys: Int = readers.size
}

object ObservationReads {
  enum Question {
    case Query(query: CandidateQuery)
    case Record(id: Int)
    case Detail(listing: ListingKey)
  }

  /** Reads nothing: a reader nobody tracks. */
  val Untracked: ObservationReads = new ObservationReads { override def read(key: String): Unit = () }
}

/** `inner`, each question it answers named to `reads` while it is asked. */
final class TrackedLookups(inner: IdentityLookups, reads: ObservationReads) extends IdentityLookups {
  import ObservationReads.Question
  def hasDetail(listing: Listing): Boolean = inner.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = reads.asking(Question.Detail(listing.key))(inner.detail(listing))
  def candidates(query: CandidateQuery): Answer[Seq[Hit]] = reads.asking(Question.Query(query))(inner.candidates(query))
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = reads.asking(Question.Record(tmdbId))(inner.film(tmdbId))
}
