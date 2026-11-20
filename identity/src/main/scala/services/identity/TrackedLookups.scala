package services.identity

import services.identity.measures.{Film, Listing}
import services.identity.vocabulary.{Answer, CandidateQuery, DetailFacts, Hit, ObservationReads, Question}

/** `inner`, each question it answers named to `reads` while it is asked. With a `pool`, a
 *  [[prefetch]] asks its questions in parallel — each a store round-trip, so a model taking up a
 *  corpus is network-bound, not CPU-bound — and the asks that follow are served once each from
 *  what it fetched; a prefetch drops whatever the last one fetched and nobody asked.
 *
 *  Safe for regions resolved side by side (`IncrementalResolver`'s take-up threads): a prefetch and the asks it serves
 *  are its calling thread's own, so one region's prefetch never drops another's answers; `inner` is handed one prefetch
 *  at a time (a store holds what a prefetch read until [[prefetchAnswered]], and a second one released it under the
 *  first); and a question asked while another thread asks it is asked ONCE ([[once]]): both get its one answer, and
 *  `reads` files its reads under it once -- two asks of one question at once dropped each other's keys
 *  ([[ObservationReads.asking]] forgets what the question read before), so a change to one woke nothing. */
final class TrackedLookups(inner: IdentityLookups, reads: ObservationReads,
                           pool: Option[java.util.concurrent.ExecutorService] = None) extends ForwardingIdentityLookups(inner) {
  // What this thread's latest prefetch fetched and nobody asked yet: each region's own.
  private val prefetched = ThreadLocal.withInitial[TrackedLookups.Prefetched](() => new TrackedLookups.Prefetched)
  private val prefetchingInner = new Object
  // The questions being asked now, each by the one thread that asks it.
  private val inFlight = new java.util.concurrent.ConcurrentHashMap[Question, TrackedLookups.InFlight]()

  // Where a model's reading goes: asks served from a prefetch, asks made one by one, and the wall
  // time of each kind — what a take-up's log line reports.
  private val prefetchedAsks = new java.util.concurrent.atomic.AtomicLong()
  private val servedAsks     = new java.util.concurrent.atomic.AtomicLong()
  private val sharedAsks     = new java.util.concurrent.atomic.AtomicLong()
  private val prefetching    = tools.Stopwatch.total()
  private val oneByOne       = tools.Stopwatch.total()
  // Each read phase asks ONE kind (a slice's queries, then its films' records, then its detail
  // pages), so a phase's asks and wall time are that kind's; the inner store's batched loads that
  // open each phase are timed beside them.
  private val asksOf    = TrackedLookups.Kinds.map(_ -> new java.util.concurrent.atomic.AtomicLong()).toMap
  private val phaseOf   = TrackedLookups.Kinds.map(_ -> tools.Stopwatch.total()).toMap
  private val batchesOf = tools.Stopwatch.total()
  def render: String =
    f"reads: ${prefetchedAsks.get} prefetched in ${prefetching.seconds}%.1fs (${servedAsks.get} served), " +
      f"${oneByOne.count} one by one in ${oneByOne.seconds}%.1fs, ${sharedAsks.get} shared with an ask in flight; " +
      TrackedLookups.Kinds.map(kind => f"$kind ${asksOf(kind).get} in ${phaseOf(kind).seconds}%.1fs").mkString(", ") +
      f", store batches ${batchesOf.count} in ${batchesOf.seconds}%.1fs"

  private def single[A](ask: => A): A = oneByOne(ask)
  private def served[A](held: A): A = { servedAsks.incrementAndGet(); held }
  private def askQuery(query: CandidateQuery) = once(Question.Candidate(query))(inner.candidates(query))
  private def askFilm(id: Int)                = once(Question.FilmRecord(id))(inner.film(id))
  private def askDetail(listing: Listing)     = once(Question.VenuePage(listing.key))(inner.detail(listing))

  /** `question` asked of `inner` -- or, while another thread asks it, that ask's answer: one ask, its reads filed once. */
  private def once[A](question: Question)(ask: => Answer[A]): Answer[A] = {
    val mine = new TrackedLookups.InFlight(Thread.currentThread)
    val held = inFlight.putIfAbsent(question, mine)
    if (held != null && (held.asker ne Thread.currentThread)) {
      sharedAsks.incrementAndGet()
      try held.answer.join().asInstanceOf[Answer[A]]
      catch { case e: java.util.concurrent.CompletionException if e.getCause != null => throw e.getCause }
    } else if (held != null) reads.asking(question)(ask) // asked again within its own ask: no other thread's to share
    // Every thread waiting on this ask is answered whatever it throws, an interrupt or a fatal error included.
    else try tools.AnyThrowable.rethrownAfter {
      val answer = reads.asking(question)(ask)
      mine.answer.complete(answer)
      answer
    }(e => mine.answer.completeExceptionally(e): Unit)
    finally { val _ = inFlight.remove(question, mine) }
  }

  // Every other question (the hasDetail check, the cast and overview the agreement stage reads, the release day, how the
  // source numbers its films) is forwarded untracked: no read of the model's is tracked by it.
  override def detail(listing: Listing): Answer[Option[DetailFacts]] = Option(prefetched.get.details.remove(listing.key)).map(served).getOrElse(single(askDetail(listing)))
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = Option(prefetched.get.queries.remove(query)).map(served).getOrElse(single(askQuery(query)))
  override def film(tmdbId: Int): Answer[Option[Film]] = Option(prefetched.get.films.remove(tmdbId)).map(served).getOrElse(single(askFilm(tmdbId)))
  /** Nothing to hand on: a [[prefetch]] tells `inner` its asks were answered once it has asked them itself. */
  override def prefetchAnswered(): Unit = ()

  override def released(asked: Iterable[CandidateQuery], records: Iterable[Int], pages: Iterable[models.ListingKey]): Unit = {
    asked.foreach(query => reads.forget(Question.Candidate(query)))
    records.foreach(id => reads.forget(Question.FilmRecord(id)))
    pages.foreach(key => reads.forget(Question.VenuePage(key)))
  }

  override def prefetch(asked: Iterable[CandidateQuery], records: Iterable[Int], pages: Iterable[Listing]): Unit = pool.foreach { threads =>
    val mine = new TrackedLookups.Prefetched
    prefetched.set(mine)
    // The inner lookups' own prefetch first: a store that can read a slice's documents in a few
    // batches does, and the asks below are then answered from what it holds. Without it every ask
    // read each of its documents as its own round-trip — 21 s of a UK restore's 28 s.
    val kind = if (asked.nonEmpty) TrackedLookups.Queries else if (records.nonEmpty) TrackedLookups.Records else TrackedLookups.Pages
    val tasks: Seq[java.util.concurrent.Callable[Unit]] =
      asked.toSeq.map(query => (() => { mine.queries.put(query, askQuery(query)); () }): java.util.concurrent.Callable[Unit]) ++
        records.toSeq.map(id => (() => { mine.films.put(id, askFilm(id)); () }): java.util.concurrent.Callable[Unit]) ++
        pages.toSeq.map(listing => (() => { mine.details.put(listing.key, askDetail(listing)); () }): java.util.concurrent.Callable[Unit])
    prefetchingInner.synchronized {
      try {
        prefetching(phaseOf(kind)(batchesOf(inner.prefetch(asked, records, pages))))
        if (tasks.nonEmpty) {
          import scala.jdk.CollectionConverters._
          // A task that failed is simply not served from the prefetch: its ask, later, asks again.
          prefetching(phaseOf(kind)(threads.invokeAll(tasks.asJava).asScala.foreach(future => scala.util.Try(future.get()))))
          prefetchedAsks.addAndGet(tasks.size.toLong)
          asksOf(kind).addAndGet(tasks.size.toLong)
        }
      } finally inner.prefetchAnswered()
    }
  }
}

object TrackedLookups {
  /** What one prefetch fetched, each answer served once. */
  private final class Prefetched {
    val queries = new java.util.concurrent.ConcurrentHashMap[CandidateQuery, Answer[Seq[Hit]]]()
    val films   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[Film]]]()
    val details = new java.util.concurrent.ConcurrentHashMap[models.ListingKey, Answer[Option[DetailFacts]]]()
  }
  /** A question being asked, by `asker`: its answer once asked. */
  private final class InFlight(val asker: Thread) {
    val answer = new java.util.concurrent.CompletableFuture[Answer[?]]()
  }

  /** The kinds of read a take-up's phases make, as its log line names them. */
  val Queries = "queries"
  val Records = "film records"
  val Pages   = "detail pages"
  val Kinds: Seq[String] = Seq(Queries, Records, Pages)
}
