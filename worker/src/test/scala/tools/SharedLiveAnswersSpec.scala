package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.util.Try

/** A gap-fill convergence leg's passes ask the live web what the recording lacks, side by side in one
 *  JVM: each must be given the SAME answer, or the order-independence claim compares three live fetches
 *  instead of three arrival orders (run 36801876786: one of three concurrent fetches of the same Rotten
 *  Tomatoes page came back without its Tomatometer). */
class SharedLiveAnswersSpec extends AnyFlatSpec with Matchers {

  /** A live web that answers each request differently, slowly enough for the passes to overlap. */
  private final class Fickle extends HttpFetch {
    val calls = new AtomicInteger()
    def get(url: String): String = { Thread.sleep(50); s"answer ${calls.incrementAndGet()}" }
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  "Passes asking the same page at once" should "all be given the one answer the live web gave" in {
    val live   = new Fickle
    val shared = new SharedLiveAnswers
    val passes = Seq.fill(3)(shared.over(live))
    val start  = new CountDownLatch(1)
    val pool   = Executors.newFixedThreadPool(3)
    val asked  = passes.map(p => pool.submit(() => { start.await(); p.get("https://www.rottentomatoes.com/m/tosca") }))
    start.countDown()
    val answers = asked.map(_.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS))
    pool.shutdown()

    answers.distinct shouldBe Seq("answer 1")
    live.calls.get shouldBe 1
  }

  it should "share a failure as it shares an answer, and keep different requests apart" in {
    var n = 0
    val live = new HttpFetch {
      def get(url: String): String = { n += 1; if (url.endsWith("gone")) throw new java.io.IOException(s"down $n") else s"$url $n" }
      def post(url: String, body: String, contentType: String): String = s"post $body ${ { n += 1; n } }"
    }
    val shared = new SharedLiveAnswers
    val (a, b) = (shared.over(live), shared.over(live))

    Try(a.get("x/gone")).failed.get.getMessage shouldBe "down 1"
    Try(b.get("x/gone")).failed.get.getMessage shouldBe "down 1"
    a.get("x/page") shouldBe b.get("x/page")
    a.post("u", "1", "text/plain") should not be b.post("u", "2", "text/plain")
  }
}
