package services.identity.vocabulary

import scala.collection.mutable

/**
 * Which store observations each of a model's questions read — a candidate query, a film's record,
 * a listing's detail page — so that when new content is filed under a key (a TMDB document whose
 * value changed: `TmdbStore.onChanged`; or a venue page read anew: the worker's `services.identityvenuepages.VenuePageIndex`), the model re-reads
 * exactly the questions that read it ([[changedBy]]).
 *
 * Filled while the model asks ([[TrackedLookups]] names the question, the store's readers name the
 * keys) — on the model's thread, or on the threads a prefetch asks on: the question being asked is
 * each thread's own. A question read again is filed again, so a key it no longer reads is dropped
 * when its question is next asked ([[forget]]).
 */
class ObservationReads {
  import ObservationReads.{Asked, Change}
  private val asking  = new ThreadLocal[Option[Question]] { override def initialValue() = None }
  // Each key's readers under ONE string of it: the stores build a key afresh on every read (`TmdbStore.keyOf`, a venue
  // page's), and a question's set held the copy its own read made -- one string per (question, key) pair, the same
  // document named by every question reading it: on PL's model ~16% of the live set (JFR old-object samples, 2026-11-16).
  private val readers = mutable.HashMap.empty[String, ObservationReads.Readers]
  private val keysOf  = mutable.HashMap.empty[Question, Asked]
  // One clock for when each question began asking and when each change was announced: a change wakes only the questions
  // asked before it, which read the document as it was.
  private val ticks = new java.util.concurrent.atomic.AtomicLong()

  /** Run `body` as the asking of `question`: every key this thread reads meanwhile is that question's. */
  def asking[A](question: Question)(body: => A): A = {
    forget(question)
    synchronized(keysOf(question) = Asked(ticks.incrementAndGet(), Set.empty))
    val outer = asking.get
    asking.set(Some(question))
    try body finally asking.set(outer)
  }

  /** The store read `key` for the question this thread is asking, if any. */
  def read(key: String): Unit = asking.get.foreach { question => synchronized {
    val held = readers.getOrElseUpdate(key, new ObservationReads.Readers(key))
    held.questions += question
    keysOf.updateWith(question)(asked => Some(asked.fold(Asked(0L, Set(held.key)))(asked => asked.copy(keys = asked.keys + held.key))))
  } }

  /** Drop what `question` read: it is being asked anew. */
  def forget(question: Question): Unit = synchronized {
    keysOf.remove(question).foreach(_.keys.foreach(key => readers.get(key).foreach { held =>
      held.questions -= question
      if (held.questions.isEmpty) readers.remove(key)
    }))
  }

  /** New content was filed (under the key the caller pairs it with), now: by the question this thread is asking, if it
   *  is asking one. */
  def changed(): Change = Change(ticks.incrementAndGet(), asking.get, filed = asking.get.isDefined)
  /** New content, now, filed by no question's ask: a venue page read, a sweep's re-fetch. */
  def announced(): Change = Change(ticks.incrementAndGet(), None, filed = false)

  /** The questions whose answers new content under `keys` can change: every question that reads one. */
  def changedBy(keys: Iterable[String]): AnswersChanged = synchronized(answersOf(keys.flatMap(readers.get(_).fold(Set.empty[Question])(_.questions)).toSet))

  /** The questions whose answers each change can have changed: those that read its key and began asking before it, but
   *  not the one that filed its newest content -- it has read what the key holds -- unless the model still `holdsOpen` it
   *  (holds it unanswered: [[IncrementalResolver.holdsOpen]]), and, for a change no question filed, every reader. The
   *  model holds a question's answer in more than one place: a family's resolve asks a film's record for itself
   *  (`CandidateGeneration.records`) while the corpus holds the record it read before, and the family's live ask filing
   *  it left the corpus's gap open for good -- every later filing of it the same content, announced to no one (Main
   *  #2712, #2716, #2717: 359 UK records, the 2013 "Catching Fire" among them, never read after a TMDB outage). */
  def changedBy(changes: Map[String, Change], holdsOpen: Question => Boolean = _ => false): AnswersChanged =
    synchronized(answersOf(changes.toSeq.flatMap { case (key, change) =>
      readers.get(key).fold(Set.empty[Question])(_.questions).filter(question =>
        !change.filed || (keysOf.get(question).forall(_.at < change.at) && (!change.newestFiler.contains(question) || holdsOpen(question))))
    }.toSet))

  private def answersOf(questions: Set[Question]): AnswersChanged =
    AnswersChanged(
      questions.collect { case Question.Candidate(query) => query },
      questions.collect { case Question.FilmRecord(id) => id },
      questions.collect { case Question.VenuePage(listing) => listing })

  /** How many keys it tracks: the index a model's questions keep over the store. */
  def keys: Int = synchronized(readers.size)

  /** How many distinct string objects its index holds its keys in: one a key, whichever questions read it. */
  private[identity] def keyObjects: Int = synchronized {
    val held = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[String, java.lang.Boolean]())
    readers.keysIterator.foreach(held.add); keysOf.valuesIterator.foreach(_.keys.foreach(held.add))
    held.size
  }

  /** Every key some question reads now — what the store's sweep must keep (`TmdbStoreSweep`). */
  def trackedKeys: Set[String] = synchronized(readers.keySet.toSet)
}

object ObservationReads {
  /** The questions reading one key, under the one string of the key every question's [[Asked]] names. */
  private final class Readers(val key: String) { var questions: Set[Question] = Set.empty }

  /** When a question began asking, and the keys it read. */
  private final case class Asked(at: Long, keys: Set[String])

  /** A key's change: announced at `at` (the latest of several merged), its newest content filed by `newestFiler`'s own
   *  ask, when every announcement of it was `filed` by a question; a change anyone else announced wakes every reader.
   *  Merged, only the newer filing's filer is kept: an earlier filer read content the newer filing replaced, so it is
   *  woken like any other reader (Main #2716: excusing every filer left one holding a superseded search). */
  final case class Change(at: Long, newestFiler: Option[Question], filed: Boolean) {
    def merge(other: Change): Change =
      Change(at max other.at, if (other.at >= at) other.newestFiler else newestFiler, filed && other.filed)
  }

  /** Reads nothing: a reader nobody tracks. */
  val Untracked: ObservationReads = new ObservationReads { override def read(key: String): Unit = () }
}
