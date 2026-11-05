package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.jdk.CollectionConverters._

/** The model's lookups: a prefetch asks its questions in parallel, each read still filed under its
 *  own question; the asks that follow are served once each from what it fetched; and the next
 *  prefetch drops what nobody asked, so an answer that changed since is never served stale. */
class TrackedLookupsSpec extends AnyFlatSpec with Matchers {

  /** A store: every question reads the key `q:<sortKey>`, on whatever thread asks, and answers
   *  what `answers` says now. */
  private class Store(reads: ObservationReads) extends IdentityLookups {
    @volatile var answers: Map[CandidateQuery, Seq[Hit]] = Map.empty
    val askedOn = new ConcurrentLinkedQueue[(CandidateQuery, String)]()
    def hasDetail(listing: Listing): Boolean = false
    def detail(listing: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = {
      askedOn.add(query -> Thread.currentThread().getName)
      reads.read("q:" + query.sortKey)
      Answer.Known(answers.getOrElse(query, Nil))
    }
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
  }

  private val queries = (1 to 20).map(i => CandidateQuery.Title(s"Film $i"))
  private def pool() = Executors.newFixedThreadPool(4, { (task: Runnable) => new Thread(task, "prefetch") })

  "a prefetch" should "ask its questions on the pool, and serve each ask that follows from what it fetched" in {
    val reads  = new ObservationReads
    val store  = new Store(reads)
    store.answers = queries.zipWithIndex.map { case (q, i) => q -> Seq(Hit(i, s"Film $i", None, None, 1)) }.toMap
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    queries.map(lookups.candidates) shouldBe queries.map(q => Answer.Known(store.answers(q)))
    store.askedOn.asScala.map(_._1).toSeq.sortBy(_.sortKey) shouldBe queries.sortBy(_.sortKey)   // each once
    store.askedOn.asScala.map(_._2).toSet shouldBe Set("prefetch")
  }

  it should "file every read under its own question, whatever thread made it" in {
    val reads   = new ObservationReads
    val lookups = new TrackedLookups(new Store(reads), reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    queries.foreach(q => reads.changedBy(Seq("q:" + q.sortKey)).queries shouldBe Set(q))
  }

  "a released question" should "leave nothing behind in the reads index" in {
    val reads   = new ObservationReads
    val lookups = new TrackedLookups(new Store(reads), reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    reads.keys shouldBe queries.size
    lookups.released(queries, Nil, Nil)
    reads.keys shouldBe 0
  }

  "a prefetch" should "never serve a later ask what an earlier prefetch fetched and nobody asked" in {
    val reads   = new ObservationReads
    val store   = new Store(reads)
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    val query   = queries.head
    lookups.prefetch(Seq(query), Nil, Nil)
    store.answers = Map(query -> Seq(Hit(9, "Film 1", None, Some(2026), 5)))   // the fill filed a new answer
    lookups.prefetch(Nil, Nil, Nil)                                              // the next read phase
    lookups.candidates(query) shouldBe Answer.Known(Seq(Hit(9, "Film 1", None, Some(2026), 5)))
  }

  // A take-up resolves its regions side by side (IncrementalResolver's `takeUpThreads`), and two regions may ask one
  // question at once. Each ask was its own: the second's `reads.asking` forgot the keys the first had read so far, so a
  // change to one woke neither, and the store was asked twice. Now the second waits for the first's answer.
  "two regions asking one question at once" should "make one ask, and get its one answer, every read filed under it" in {
    val reads   = new ObservationReads
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val query   = queries.head
    val store   = new Store(reads) {
      private val asks = new java.util.concurrent.atomic.AtomicInteger()
      override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = {
        reads.read("early:" + q.sortKey)                       // read before the other region's ask began
        if (asks.incrementAndGet() == 1) { entered.countDown(); release.await() }
        super.candidates(q)
      }
    }
    store.answers = Map(query -> Seq(Hit(7, "Film 1", None, Some(2026), 3)))
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    val regions = Executors.newFixedThreadPool(2)
    try {
      val first = regions.submit(() => lookups.candidates(query))
      entered.await()
      val asker  = new java.util.concurrent.atomic.AtomicReference[Thread]()
      val second = regions.submit(() => { asker.set(Thread.currentThread); lookups.candidates(query) })
      // the second region is either asking the store too, or waiting on the first's ask
      TrackedLookupsSpec.until(store.askedOn.size == 2 ||
        Option(asker.get).exists(_.getState == Thread.State.WAITING) || second.isDone)
      release.countDown()
      Seq(first.get, second.get) shouldBe Seq.fill(2)(Answer.Known(store.answers(query)))
      store.askedOn.size shouldBe 1
      reads.changedBy(Seq("early:" + query.sortKey)).queries shouldBe Set(query)
      reads.changedBy(Seq("q:" + query.sortKey)).queries shouldBe Set(query)
    } finally { release.countDown(); val _ = regions.shutdownNow() }
  }

  "one region's prefetch" should "never drop what another region's prefetch fetched for its own asks" in {
    val reads   = new ObservationReads
    val store   = new Store(reads)
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    val (mine, theirs) = (queries.take(3), queries.slice(3, 6))
    lookups.prefetch(mine, Nil, Nil)
    val other = Executors.newSingleThreadExecutor()
    try other.submit(TrackedLookupsSpec.task(lookups.prefetch(theirs, Nil, Nil))).get() finally { val _ = other.shutdownNow() }
    mine.foreach(lookups.candidates)
    store.askedOn.asScala.map(_._1).toSeq.sortBy(_.sortKey) shouldBe (mine ++ theirs).sortBy(_.sortKey)   // each once
  }

  // The store a prefetch reads holds its documents until `prefetchAnswered` (`StoredTmdbLookups`); a second region's
  // prefetch released them under the first's asks (the `held.clear` NPE of the 2026-09-30 parallel-regions experiment).
  it should "hand the store one prefetch at a time" in {
    val reads   = new ObservationReads
    val inside  = new java.util.concurrent.atomic.AtomicInteger()
    val most    = new java.util.concurrent.atomic.AtomicInteger()
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val calls   = new java.util.concurrent.atomic.AtomicInteger()
    val store   = new Store(reads) {
      override def prefetch(q: Iterable[CandidateQuery], f: Iterable[Int], d: Iterable[Listing]): Unit = {
        most.accumulateAndGet(inside.incrementAndGet(), math.max)
        if (calls.incrementAndGet() == 1) { entered.countDown(); release.await() }
      }
      override def prefetchAnswered(): Unit = { val _ = inside.decrementAndGet() }
    }
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    val regions = Executors.newFixedThreadPool(2)
    try {
      val first = regions.submit(TrackedLookupsSpec.task(lookups.prefetch(queries.take(2), Nil, Nil)))
      entered.await()
      val asker  = new java.util.concurrent.atomic.AtomicReference[Thread]()
      val second = regions.submit(TrackedLookupsSpec.task { asker.set(Thread.currentThread); lookups.prefetch(queries.slice(2, 4), Nil, Nil) })
      TrackedLookupsSpec.until(calls.get == 2 || Option(asker.get).exists(_.getState == Thread.State.BLOCKED) || second.isDone)
      release.countDown()
      first.get(); second.get()
      most.get shouldBe 1
      calls.get shouldBe 2
    } finally { release.countDown(); val _ = regions.shutdownNow() }
  }

  // A take-up's "reads: N prefetched in Xs" said nothing of WHICH reads: a UK restore spent 8-10 s
  // there with no way to tell detail pages from TMDB queries or film records. Each read phase asks
  // one kind, so each kind's count and time can be named.
  "the read report" should "name each kind's asks, and the store's own batched loads beside them" in {
    val reads   = new ObservationReads
    val lookups = new TrackedLookups(new Store(reads), reads, Some(pool()))
    val page    = Listing.of(models.Helios, models.CinemaMovie(models.Movie("Lalka"), models.Helios, None, Some("https://helios.pl/lalka"),
      None, Nil, Nil, Nil), services.movies.SingleCountryNormalizer.titleNormalizer)
    lookups.prefetch(queries, Nil, Nil)
    lookups.prefetch(Nil, Seq(1, 2, 3), Nil)
    lookups.prefetch(Nil, Nil, Seq(page))
    val line = lookups.render
    line should startWith ("reads: 24 prefetched in ")
    line should include ("queries 20 in ")
    line should include ("film records 3 in ")
    line should include ("detail pages 1 in ")
    line should include ("store batches ")
  }

  // TrackedLookups forwarded the trait by hand and dropped the release day and how the source numbers its films: the
  // trait's defaults stood in, a day read off the record and TMDB numbering, whatever the wrapped source said.
  "the tracked lookups" should "answer every question it does not track as the source it wraps does" in {
    val reads = new ObservationReads
    val day   = java.time.LocalDate.of(2026, 12, 8)
    val source = new Store(reads) {
      override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = if (tmdbId == 7) Answer.Known(Some(day)) else Answer.Unknown
      override def numbersFilmsByTmdbId: Boolean = false
    }
    val lookups = new TrackedLookups(source, reads)
    lookups.releaseDay(7) shouldBe Answer.Known(Some(day))
    lookups.releaseDay(8) shouldBe Answer.Unknown
    lookups.numbersFilmsByTmdbId shouldBe false
  }
}

object TrackedLookupsSpec {
  /** `body` as a task an executor runs. */
  def task(body: => Unit): java.util.concurrent.Callable[Unit] = () => body

  /** Wait until `condition` holds -- a state another thread reaches, never a time -- failing past a bound no healthy run
   *  nears (a hang would otherwise hold the suite forever). */
  def until(condition: => Boolean): Unit = {
    var polls = 0
    while (!condition && polls < 200000) { java.util.concurrent.locks.LockSupport.parkNanos(50000L); polls += 1 }
    assert(condition, "the other thread never reached the state waited for")
  }
}
