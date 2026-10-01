package services.identity

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

  /** An in-memory backend whose every round-trip takes a while, counted — and refuses to write `refused`. */
  private final class SlowDocuments(refused: Set[String] = Set.empty) extends TmdbDocuments {
    private val held = new InMemoryTmdbDocuments
    val gets = new AtomicInteger
    val puts = new AtomicInteger
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = { gets.incrementAndGet(); Thread.sleep(20); held.get(kind, ids) }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = {
      puts.incrementAndGet(); Thread.sleep(20)
      docs.find(d => refused(d._1)).foreach(d => throw new IllegalStateException(s"refused ${d._1}"))
      held.put(kind, docs)
    }
    def scan(kind: TmdbKind)(page: Seq[(String, Option[Long])] => Unit): Boolean = held.scan(kind)(page)
    def delete(kind: TmdbKind, ids: Seq[String]): Unit = held.delete(kind, ids)
  }

  private def doc(n: Int) = new BsonDocument("n", BsonInt32(n))

  /** `n` callers at once, each released together, on virtual threads as the take-up's prefetch runs. */
  private def together[A](n: Int)(call: Int => A): Seq[Try[A]] = {
    val pool  = Executors.newThreadPerTaskExecutor(Thread.ofVirtual().factory())
    val start = new CountDownLatch(1)
    val futures = (0 until n).map(i => pool.submit((() => { start.await(); Try(call(i)) }): Callable[Try[A]]))
    start.countDown()
    try futures.map(_.get()) finally pool.shutdown()
  }

  "coalesced writes" should "file every caller's document in far fewer round-trips than callers" in {
    val slow = new SlowDocuments()
    val docs = new CoalescedTmdbDocuments(slow)
    together(64)(i => docs.put(TmdbKind.Film, Seq(i.toString -> doc(i)))).foreach(_.get)
    slow.get(TmdbKind.Film, (0 until 64).map(_.toString)) shouldBe (0 until 64).map(i => i.toString -> doc(i)).toMap
    slow.puts.get should be <= 16
  }

  "coalesced reads" should "answer each caller with its own documents, in far fewer round-trips than callers" in {
    val slow = new SlowDocuments()
    slow.put(TmdbKind.Query, (0 until 64).map(i => s"q$i" -> doc(i)))
    val docs = new CoalescedTmdbDocuments(slow)
    val answers = together(64)(i => docs.get(TmdbKind.Query, Seq(s"q$i", "never-kept"))).map(_.get)
    answers shouldBe (0 until 64).map(i => Map(s"q$i" -> doc(i)))
    slow.gets.get should be <= 16 + 1   // the seeding read is not one of them
  }

  it should "hand two callers asking for one document each its own copy" in {
    val slow = new SlowDocuments()
    slow.put(TmdbKind.Film, Seq("1018" -> doc(1)))
    val docs = new CoalescedTmdbDocuments(slow)
    val answers = together(16)(_ => docs.get(TmdbKind.Film, Seq("1018"))("1018")).map(_.get)
    answers.foreach(_ shouldBe doc(1))
    answers.head.put("n", BsonString("edited"))
    answers.tail.foreach(_ shouldBe doc(1))
  }

  "a write the store refuses" should "fail only its own caller" in {
    val slow = new SlowDocuments(refused = Set("13"))
    val docs = new CoalescedTmdbDocuments(slow)
    val outcomes = together(32)(i => docs.put(TmdbKind.Film, Seq(i.toString -> doc(i))))
    outcomes.zipWithIndex.collect { case (outcome, i) if outcome.isFailure => i } shouldBe Seq(13)
    slow.get(TmdbKind.Film, (0 until 32).map(_.toString)).keySet shouldBe (0 until 32).filterNot(_ == 13).map(_.toString).toSet
  }

  "a write naming one id twice" should "keep the later document" in {
    val docs = new CoalescedTmdbDocuments(new SlowDocuments())
    docs.put(TmdbKind.Film, Seq("1" -> doc(1), "1" -> doc(2)))
    docs.get(TmdbKind.Film, Seq("1")) shouldBe Map("1" -> doc(2))
  }
}
