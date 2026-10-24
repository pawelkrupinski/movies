package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}
import tools.{SpecClock, SpecTimeouts, Stopwatch}

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
                   filed: () => Unit = () => ()) =
    new InlineFamilyAsks(AgreementAskers.over(store, SpecClock.Pinned)(sources = sources, filed = filed), pool, Stopwatch.System)

  "inline family asks" should "file each family's answer before returning" in {
    val store = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val pool  = Executors.newFixedThreadPool(4)
    asks(store, Map(VoterFamily.Imdb -> source(VoterFamily.Imdb, Seq(hit))), pool)(Set(Question.Family(VoterFamily.Imdb, "title|Klondike"), Question.Family(VoterFamily.Wiki, "title|Klondike")), 5.seconds)   // no Wiki source: not asked
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Imdb, "title|Klondike")) shouldBe false
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Wiki, "title|Klondike")) shouldBe true
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

  it should "leave a failed ask to the queue" in {
    val store = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val pool  = Executors.newFixedThreadPool(1)
    asks(store, Map(VoterFamily.Imdb -> source(VoterFamily.Imdb, throw new java.io.IOException("down"))), pool)(Set(Question.Family(VoterFamily.Imdb, "title|Klondike")), 5.seconds)
    store.wanted(FamilyAnswerStore.questionId(VoterFamily.Imdb, "title|Klondike")) shouldBe true
    pool.shutdown()
  }
}
