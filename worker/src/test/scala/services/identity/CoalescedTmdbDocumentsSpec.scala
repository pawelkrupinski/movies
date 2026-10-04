package services.identity

import tools.SpecTimeouts

import org.bson.{BsonDocument, BsonInt32, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Callable, CountDownLatch, Executors}
import scala.util.Try

/** The coalescing decorator keeps the documents' storage contract… */
class CoalescedTmdbDocumentsContractSpec extends TmdbDocumentsBehaviour {
  protected def newDocuments(): TmdbDocuments = new CoalescedTmdbDocuments(new InMemoryTmdbDocuments)
}

/** …and makes concurrent reads and writes one round-trip per batch, each caller still answered with
 *  exactly its own documents. */
class CoalescedTmdbDocumentsSpec extends AnyFlatSpec with Matchers {

  /** An in-memory backend whose every round-trip is counted and held until the test [[open]]s it —
   *  and that refuses to write `refused`. Held, not slept on: a sleep left how callers fell into
   *  batches to the scheduler, so on a loaded machine a slow-waking caller found a slot free and ran
   *  a batch of its own, and the round-trip bounds below failed. */
  private final class HeldDocuments(refused: Set[String] = Set.empty) extends TmdbDocuments {
    private val held = new InMemoryTmdbDocuments
    private val gate = new CountDownLatch(1)
    val gets = new AtomicInteger
    val puts = new AtomicInteger
    def open(): Unit = gate.countDown()
    def seed(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = held.put(kind, docs)
    def stored(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = held.get(kind, ids)
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = { gets.incrementAndGet(); gate.await(); held.get(kind, ids) }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = {
      puts.incrementAndGet(); gate.await()
      docs.find(d => refused(d._1)).foreach(d => throw new IllegalStateException(s"refused ${d._1}"))
      held.put(kind, docs)
    }
  }

  private def doc(n: Int) = new BsonDocument("n", BsonInt32(n))

  /** `n` callers at once on virtual threads, as the take-up's prefetch runs them, the backend held
   *  until every one of them has made its call: at most [[CoalescedTmdbDocuments.InFlight]] batches
   *  are then in flight and the rest queued behind them, however late the callers woke. */
  private def together[A](backend: HeldDocuments, docs: CoalescedTmdbDocuments, n: Int)(call: Int => A): Seq[Try[A]] = {
    val pool    = Executors.newThreadPerTaskExecutor(Thread.ofVirtual().factory())
    val futures = (0 until n).map(i => pool.submit((() => Try(call(i))): Callable[Try[A]]))
    try {
      tools.Eventually.eventually(docs.calling shouldBe n, timeoutMs = SpecTimeouts.Io.toMillis, pollMs = 5)
      backend.open()
      futures.map(_.get())
    } finally { backend.open(); pool.shutdown() }
  }

  /** The most round-trips `together` can take: the batches in flight as the backend opens, then those
   *  that drain the queue behind them — at most one per slot, as each slot's holder polls until it is empty. */
  private val MostRoundTrips = 2 * CoalescedTmdbDocuments.InFlight

  "coalesced writes" should "file every caller's document in far fewer round-trips than callers" in {
    val held = new HeldDocuments()
    val docs = new CoalescedTmdbDocuments(held)
    together(held, docs, 64)(i => docs.put(TmdbKind.Film, Seq(i.toString -> doc(i)))).foreach(_.get)
    held.stored(TmdbKind.Film, (0 until 64).map(_.toString)) shouldBe (0 until 64).map(i => i.toString -> doc(i)).toMap
    held.puts.get should be <= MostRoundTrips
  }

  "coalesced reads" should "answer each caller with its own documents, in far fewer round-trips than callers" in {
    val held = new HeldDocuments()
    held.seed(TmdbKind.Query, (0 until 64).map(i => s"q$i" -> doc(i)))
    val docs = new CoalescedTmdbDocuments(held)
    val answers = together(held, docs, 64)(i => docs.get(TmdbKind.Query, Seq(s"q$i", "never-kept"))).map(_.get)
    answers shouldBe (0 until 64).map(i => Map(s"q$i" -> doc(i)))
    held.gets.get should be <= MostRoundTrips
  }

  it should "hand two callers asking for one document each its own copy" in {
    val held = new HeldDocuments()
    held.seed(TmdbKind.Film, Seq("1018" -> doc(1)))
    val docs = new CoalescedTmdbDocuments(held)
    val answers = together(held, docs, 16)(_ => docs.get(TmdbKind.Film, Seq("1018"))("1018")).map(_.get)
    answers.foreach(_ shouldBe doc(1))
    answers.head.put("n", BsonString("edited"))
    answers.tail.foreach(_ shouldBe doc(1))
  }

  "a write the store refuses" should "fail only its own caller" in {
    val held = new HeldDocuments(refused = Set("13"))
    val docs = new CoalescedTmdbDocuments(held)
    val outcomes = together(held, docs, 32)(i => docs.put(TmdbKind.Film, Seq(i.toString -> doc(i))))
    outcomes.zipWithIndex.collect { case (outcome, i) if outcome.isFailure => i } shouldBe Seq(13)
    held.stored(TmdbKind.Film, (0 until 32).map(_.toString)).keySet shouldBe (0 until 32).filterNot(_ == 13).map(_.toString).toSet
  }

  "a write naming one id twice" should "keep the later document" in {
    val held = new HeldDocuments()
    held.open()
    val docs = new CoalescedTmdbDocuments(held)
    docs.put(TmdbKind.Film, Seq("1" -> doc(1), "1" -> doc(2)))
    docs.get(TmdbKind.Film, Seq("1")) shouldBe Map("1" -> doc(2))
  }
  // A batch's runner interrupted mid-round-trip (a worker's shutdown stops the prefetch pool) took the
  // other callers' requests with it: they waited, forever, for a batch that would never answer them.
  "a batch whose round-trip is interrupted" should "answer every caller it took, not only its runner" in {
    val first   = new CountDownLatch(1)
    val running = new CountDownLatch(1)
    val batches = new java.util.concurrent.ConcurrentLinkedQueue[Set[String]]()
    val coalescer = new CoalescedTmdbDocuments.Coalescer[String, String](maxBatch = 8, inFlight = 1)({ batch =>
      batches.add(batch.toSet)
      if (batch == Seq("first")) { running.countDown(); first.await(); batch.map(scala.util.Success(_)) }
      else throw new InterruptedException("shutting down")
    })
    val pool = Executors.newThreadPerTaskExecutor(Thread.ofVirtual().factory())
    try {
      pool.submit((() => coalescer("first")): Callable[String])
      running.await()
      // Both queue behind the one slot the first batch holds, so the next batch takes them together.
      // (Caught whole: `Try` lets an InterruptedException through.)
      val others = Seq("b", "c").map(r => pool.submit((() =>
        try Right(coalescer(r)) catch { case e: Throwable => Left(e) }): Callable[Either[Throwable, String]]))
      // Waited for, not slept on: were "b" and "c" to run as two batches, each runner would answer
      // only itself and the test would pass whether or not the interrupted batch answers the others.
      tools.Eventually.eventually(coalescer.waiting shouldBe 2, pollMs = 5)
      first.countDown()
      others.map(_.get(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS).isLeft) shouldBe Seq(true, true)
      batches.toArray.toSeq shouldBe Seq(Set("first"), Set("b", "c"))
    } finally { first.countDown(); pool.shutdownNow(); () }
  }
}
