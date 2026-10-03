package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, TimeUnit}

/** The recorder reads prod's coverage BESIDE the archive read rather than after it: the two are
 *  independent reads of one connection, and serially the coverage's ~7 s sat on the US recording's
 *  critical path behind the corpus (run 37111868620). */
class RecordCorpusFixtureSpec extends AnyFlatSpec with Matchers {

  "alongside" should "run the second read while the first is still reading" in {
    val secondStarted = new CountDownLatch(1)
    val (first, second) = RecordCorpusFixture.alongside {
      // Serially, the second read never starts while this one waits for it.
      if (secondStarted.await(10, TimeUnit.SECONDS)) "corpus" else "the second read waited for the first"
    } { secondStarted.countDown(); "coverage" }
    (first, second) shouldBe (("corpus", "coverage"))
  }

  it should "hand back the second read's failure rather than a result without it" in {
    an[IllegalStateException] should be thrownBy
      RecordCorpusFixture.alongside("corpus")(throw new IllegalStateException("coverage read failed"))
  }
}
