package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}
import tools.{SpecClock, SpecTimeouts}

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/** A new cluster's family questions asked inline: filed before the resolve reads them, the wait capped by the deadline,
 *  and an answer that comes later filed and announced as the queue's would be. */
class InlineFamilyAsksSpec extends AnyFlatSpec with Matchers {
  private def source(of: VoterFamily, answer: => Seq[SourceHit]): FamilySource = new FamilySource {
    val family: VoterFamily                      = of
    def titled(text: String): Seq[SourceHit]     = answer
    def directedBy(name: String): Seq[SourceHit] = Nil
    def record(id: String): Option[SourceRecord] = None
  }
  private val hit = SourceHit("tt16315948", "Klondike", None, Some(2022))
  private def asks(store: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], pool: java.util.concurrent.ExecutorService,
                   filed: () => Unit = () => (), memory: QuestionMemory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", SpecClock.Pinned),
                   stopwatch: _root_.tools.Stopwatch = _root_.tools.SpecStopwatch.Frozen,
                   metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) =
    new InlineFamilyAsks(AgreementAskers.over(store, SpecClock.Pinned)(sources = sources, filed = filed, memory = memory), pool, stopwatch, metrics)

  "inline family asks" should "file each family's answer before returning" in {
    val store = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val pool  = Executors.newFixedThreadPool(4)
    asks(store, Map(VoterFamily.Imdb -> source(VoterFamily.Imdb, Seq(hit))), pool)(Set(Question.Family(VoterFamily.Imdb, "title|Klondike"), Question.Family(VoterFamily.Wiki, "title|Klondike")), 5.seconds)   // no Wiki source: filed as no film
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Imdb, "title|Klondike")) shouldBe false
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Wiki, "title|Klondike")) shouldBe false
    store.answers(VoterFamily.Wiki).titled("Klondike") shouldBe Answer.Known(Nil)
    pool.shutdown()
  }

  it should "stop waiting at the deadline, then file a late answer and ask for a projection" in {
    val store   = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val pool    = Executors.newFixedThreadPool(4)
    val release = new CountDownLatch(1)
    val filed   = new AtomicInteger()
    val slow    = source(VoterFamily.RottenTomatoes, { release.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); Seq(hit) })
    val started = System.nanoTime()
    asks(store, Map(VoterFamily.RottenTomatoes -> slow), pool, () => { filed.incrementAndGet(); () })(Set(Question.Family(VoterFamily.RottenTomatoes, "title|Klondike")), 200.millis)
    (System.nanoTime() - started).nanos should be < 5.seconds
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.RottenTomatoes, "title|Klondike")) shouldBe true
    release.countDown()
    pool.shutdown()
    pool.awaitTermination(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.RottenTomatoes, "title|Klondike")) shouldBe false
    filed.get shouldBe 1
  }

  // Inline asks filed a median 6.5 s after their agreement phase began against <= 0.5 s for the same asker on the queue
  // (prod, 7 days to 11-05), and nothing told the shared pool's wait from the ask itself: each inline ask is timed in two
  // parts, by family -- waiting for the pool, then asking (the host's pace, the wire, its retries).
  it should "time each ask's wait for the pool apart from the ask itself, by family" in {
    val store   = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val clock   = new _root_.tools.MutableClock(java.time.Instant.parse("2026-11-05T12:00:00Z"))
    val pool    = Executors.newFixedThreadPool(1)
    val busy    = new CountDownLatch(1)
    pool.submit(new Runnable { def run(): Unit = { busy.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS); () } })   // the pool's one thread taken
    val timed   = new java.util.concurrent.ConcurrentLinkedQueue[(String, Double, Double)]
    val metrics = new AgreementQuestionMetrics {
      def enqueued(family: String, added: Boolean): Unit = ()
      def asked(family: String, outcome: String): Unit   = ()
      override def inlineAsk(family: String, queuedSeconds: Double, askingSeconds: Double): Unit = { timed.add((family, queuedSeconds, askingSeconds)); () }
    }
    val slow  = source(VoterFamily.RottenTomatoes, { clock.advanceSeconds(3); Seq(hit) })   // 3 s on the wire
    val asker = new Thread(() => asks(store, Map(VoterFamily.RottenTomatoes -> slow), pool, stopwatch = _root_.tools.SpecStopwatch.of(clock),
      metrics = metrics)(Set(Question.Family(VoterFamily.RottenTomatoes, "title|Klondike")), SpecTimeouts.Io))
    asker.start()
    while (pool.asInstanceOf[java.util.concurrent.ThreadPoolExecutor].getQueue.isEmpty) Thread.onSpinWait()
    clock.advanceSeconds(2)                                                                      // 2 s waiting for the pool
    busy.countDown()
    asker.join(SpecTimeouts.Io.toMillis)
    timed.toArray.toSeq shouldBe Seq(("rt", 2.0, 3.0))
    pool.shutdown()
  }

  it should "remember a failed ask, as the queue's asker does, and not ask it inline again inside its wait" in {
    val store  = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", SpecClock.Pinned)
    val pool   = Executors.newFixedThreadPool(1)
    val asked  = new AtomicInteger()
    val klondike = Question.Family(VoterFamily.Imdb, "title|Klondike")
    val down   = asks(store, Map(VoterFamily.Imdb -> source(VoterFamily.Imdb, { asked.incrementAndGet(); throw new java.io.IOException("down") })), pool,
      memory = memory)
    down(Set(klondike), 5.seconds)
    down(Set(klondike), 5.seconds)
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Imdb, "title|Klondike")) shouldBe true
    (asked.get, memory.due(Set[Question](klondike))) shouldBe ((1, Set.empty))
    pool.shutdown()
  }
}
