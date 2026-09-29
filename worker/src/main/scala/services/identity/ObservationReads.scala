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
 * keys) — on the model's thread, or on the threads a prefetch asks on: the question being asked is
 * each thread's own. A question read again is filed again, so a key it no longer reads is dropped
 * when its question is next asked ([[forget]]).
 */
class ObservationReads {
  private val asking  = new ThreadLocal[Option[ObservationReads.Question]] { override def initialValue() = None }
  private val readers = mutable.HashMap.empty[String, Set[ObservationReads.Question]]
  private val keysOf  = mutable.HashMap.empty[ObservationReads.Question, Set[String]]

  /** Run `body` as the asking of `question`: every key this thread reads meanwhile is that question's. */
  def asking[A](question: ObservationReads.Question)(body: => A): A = {
    forget(question)
    val outer = asking.get
    asking.set(Some(question))
    try body finally asking.set(outer)
  }

  /** The store read `key` for the question this thread is asking, if any. */
  def read(key: String): Unit = asking.get.foreach { question => synchronized {
    readers.updateWith(key)(held => Some(held.getOrElse(Set.empty) + question))
    keysOf.updateWith(question)(held => Some(held.getOrElse(Set.empty) + key))
  } }

  /** Drop what `question` read: it is being asked anew. */
  def forget(question: ObservationReads.Question): Unit = synchronized {
    keysOf.remove(question).foreach(_.foreach(key => readers.updateWith(key)(_.map(_ - question).filter(_.nonEmpty))))
  }

  /** The questions whose answers new content under `keys` can change. */
  def changedBy(keys: Iterable[String]): AnswersChanged = synchronized {
    val questions = keys.flatMap(readers.getOrElse(_, Set.empty)).toSet
    AnswersChanged(
      questions.collect { case ObservationReads.Question.Query(query) => query },
      questions.collect { case ObservationReads.Question.Record(id) => id },
      questions.collect { case ObservationReads.Question.Detail(listing) => listing })
  }

  /** How many keys it tracks: the index a model's questions keep over the store. */
  def keys: Int = synchronized(readers.size)
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

/** `inner`, each question it answers named to `reads` while it is asked. With a `pool`, a
 *  [[prefetch]] asks its questions in parallel — each a store round-trip, so a model taking up a
 *  corpus is network-bound, not CPU-bound — and the asks that follow are served once each from
 *  what it fetched; a prefetch drops whatever the last one fetched and nobody asked. */
final class TrackedLookups(inner: IdentityLookups, reads: ObservationReads,
                           pool: Option[java.util.concurrent.ExecutorService] = None) extends IdentityLookups {
  import ObservationReads.Question
  private val queries = new java.util.concurrent.ConcurrentHashMap[CandidateQuery, Answer[Seq[Hit]]]()
  private val films   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[IdentityMeasures.Film]]]()
  private val details = new java.util.concurrent.ConcurrentHashMap[services.movies.ListingKey, Answer[Option[DetailFacts]]]()

  // Where a model's reading goes: asks served from a prefetch, asks made one by one, and the wall
  // time of each kind — what a take-up's log line reports.
  private val prefetchedAsks = new java.util.concurrent.atomic.AtomicLong()
  private val servedAsks     = new java.util.concurrent.atomic.AtomicLong()
  private val prefetching    = tools.Stopwatch.total()
  private val oneByOne       = tools.Stopwatch.total()
  def render: String =
    f"reads: ${prefetchedAsks.get} prefetched in ${prefetching.seconds}%.1fs (${servedAsks.get} served), " +
      f"${oneByOne.count} one by one in ${oneByOne.seconds}%.1fs"

  private def single[A](ask: => A): A = oneByOne(ask)
  private def served[A](held: A): A = { servedAsks.incrementAndGet(); held }
  private def askQuery(query: CandidateQuery) = reads.asking(Question.Query(query))(inner.candidates(query))
  private def askFilm(id: Int)                = reads.asking(Question.Record(id))(inner.film(id))
  private def askDetail(listing: Listing)     = reads.asking(Question.Detail(listing.key))(inner.detail(listing))

  def hasDetail(listing: Listing): Boolean = inner.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = Option(details.remove(listing.key)).map(served).getOrElse(single(askDetail(listing)))
  def candidates(query: CandidateQuery): Answer[Seq[Hit]] = Option(queries.remove(query)).map(served).getOrElse(single(askQuery(query)))
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Option(films.remove(tmdbId)).map(served).getOrElse(single(askFilm(tmdbId)))

  override def released(asked: Iterable[CandidateQuery], records: Iterable[Int], pages: Iterable[services.movies.ListingKey]): Unit = {
    asked.foreach(query => reads.forget(Question.Query(query)))
    records.foreach(id => reads.forget(Question.Record(id)))
    pages.foreach(key => reads.forget(Question.Detail(key)))
  }

  override def prefetch(asked: Iterable[CandidateQuery], records: Iterable[Int], pages: Iterable[Listing]): Unit = pool.foreach { threads =>
    queries.clear(); films.clear(); details.clear()
    // The inner lookups' own prefetch first: a store that can read a slice's documents in a few
    // batches does, and the asks below are then answered from what it holds. Without it every ask
    // read each of its documents as its own round-trip — 21 s of a UK restore's 28 s.
    prefetching(inner.prefetch(asked, records, pages))
    val tasks: Seq[java.util.concurrent.Callable[Unit]] =
      asked.toSeq.map(query => (() => { queries.put(query, askQuery(query)); () }): java.util.concurrent.Callable[Unit]) ++
        records.toSeq.map(id => (() => { films.put(id, askFilm(id)); () }): java.util.concurrent.Callable[Unit]) ++
        pages.toSeq.map(listing => (() => { details.put(listing.key, askDetail(listing)); () }): java.util.concurrent.Callable[Unit])
    try if (tasks.nonEmpty) {
      import scala.jdk.CollectionConverters._
      // A task that failed is simply not served from the prefetch: its ask, later, asks again.
      prefetching(threads.invokeAll(tasks.asJava).asScala.foreach(future => scala.util.Try(future.get())))
      prefetchedAsks.addAndGet(tasks.size.toLong)
    } finally inner.prefetchAnswered()
  }
}
